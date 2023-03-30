module Perun.Offchain.ChannelTxPool.ChannelTxPool
  ( txpoolsForChannel,
    mkTxPools,
    mkChannelGenesis,
    mkChannelStep,
    mkChannelLast,
    hasValidThreadToken,
    isValidDatum,
    parseDatumFromOutputDatum,
    channelDatumFromDatum,
    ChannelTxErr (..),
  )
where

import Control.Lens
import Control.Monad (unless)
import Control.Monad.Error.Lens (throwing)
import Data.Default
import Data.Either (rights)
import Data.List (elemIndex, find, foldl')
import qualified Data.Map.Strict as Map
import Ledger hiding (ChainIndexTxOut)
import Ledger.Value (assetClassValueOf)
import Perun.Error
import Perun.Offchain (getChannelId)
import Perun.Offchain.ChannelTxPool.Types
import Perun.Onchain
import Plutus.ChainIndex hiding (txFromTxId)
import Plutus.ChainIndex.Api
import Plutus.Contract
import Plutus.Contract.Request
import PlutusTx

txpoolsForChannel ::
  (AsPerunError e, AsContractError e) =>
  ChannelID ->
  Contract w s e [ChannelTxPool]
txpoolsForChannel cid = allTxosAt (channelAddress cid) >>= mkTxPools cid

allTxosAt ::
  (AsPerunError e, AsContractError e) =>
  Address ->
  Contract w s e [TxOutRef]
allTxosAt addr = go (Just def)
  where
    go Nothing = return []
    go (Just np) = do
      txoRefsAt np addr >>= \case
        TxosResponse (Page _ _ []) -> do
          -- No transaction outputs found at channel address.
          return []
        TxosResponse (Page _ Nothing txoRefs) -> do
          -- No more transactions to fetch.
          return txoRefs
        TxosResponse (Page _ np' txoRefs) -> do
          (txoRefs ++) <$> go np'

-- | mkTxPools creates possibly multiple transaction pools distinguish by a
-- ChannelToken for the given channel id using a list of TxOutRefs. This
-- version works in the Contract monad since it is required to interface with
-- the ChainIndex to obtain metadata.
mkTxPools ::
  (AsPerunError e, AsContractError e) =>
  ChannelID ->
  [TxOutRef] ->
  Contract w s e [ChannelTxPool]
mkTxPools cid txoRefs = do
  logInfo @String $ unwords ["mkTxPools", show cid, "with", show . length $ txoRefs]
  uniqueCitxs <- dedup . concat <$> mapM resolveTx txoRefs
  channelTxs <- rights <$> mapM (mkChannelTx cid) uniqueCitxs
  return . filterValidStart . partitionByChannelToken $ channelTxs
  where
    filterValidStart = filter (\(ChannelTxPool _ ctxs) -> any isValidStart ctxs)
    isValidStart (ChannelTx _ Nothing (Just _)) = True
    isValidStart _ = False

-- | resolveTx resolves a TxOutRef to the transactions that created and
-- potentially spent it.
resolveTx ::
  (AsPerunError e, AsContractError e) =>
  TxOutRef ->
  Contract w s e [ChainIndexTx]
resolveTx ref = do
  let txid = txOutRefId ref
  createdTx <-
    txFromTxId txid >>= \case
      Nothing -> do
        -- Abort if the chainindex is corrupted and could not retrieve
        -- the necessary transactions.
        throwing _SubscriptionError NoTxFromTxIdFetchableErr
      Just r -> return r
  -- TODO: Check if the TxOutRef was already spent. This cannot be implemented
  -- currently because the ChainIndex does not support querying for TXs that
  -- consumed some UTXO.
  --
  -- utxoIsSpent ref >>= \case
  --    Nothing -> return [createdTx]
  --    Just createdTx -> return [createdTx, spentTx]
  return [createdTx]

partitionByChannelToken ::
  [(ChannelToken, ChannelTx)] ->
  [ChannelTxPool]
partitionByChannelToken chanTxsWithToken = map (uncurry ChannelTxPool_) $ Map.toList poolMap
  where
    poolMap = foldl' f Map.empty chanTxsWithToken
    f acc (token, chanTx) = Map.insertWith (++) token [chanTx] acc

mkChannelTx ::
  (AsPerunError e, AsContractError e) =>
  ChannelID ->
  ChainIndexTx ->
  Contract w s e (Either ChannelTxErr (ChannelToken, ChannelTx))
mkChannelTx cid citx = do
  allInputTxs <- concat <$> mapM (resolveTx . txInRef) (citx ^. citxInputs)
  i <- case resolveInput cid citx allInputTxs of
    Right i -> return $ Just i
    Left NoChannelInputErr -> return Nothing
    Left err -> throwing _SubscriptionError $ ChannelErr err
  o <- case resolveOutput cid citx of
    Right o -> return $ Just o
    Left NoChannelOutputErr -> return Nothing
    Left err -> throwing _SubscriptionError $ ChannelErr err
  ct <- retrieveChannelToken i o
  fmap (ct,) <$> mkChannelTx' i o
  where
    retrieveChannelToken ::
      (AsPerunError e, AsContractError e) =>
      Maybe (TxOutRef, ChannelAction, ChannelDatum) ->
      Maybe (TxOutRef, ChannelDatum) ->
      Contract w s e ChannelToken
    retrieveChannelToken Nothing Nothing = throwing _SubscriptionError NoChannelTokenAvailableErr
    retrieveChannelToken (Just (_, _, d)) Nothing = return $ channelToken d
    retrieveChannelToken Nothing (Just (_, d)) = return $ channelToken d
    retrieveChannelToken (Just (_, _, d)) (Just (_, d')) = do
      let iToken = channelToken d
          oToken = channelToken d'
      unless (iToken == oToken) $
        throwing _SubscriptionError MismatchedChannelTokenErr
      return iToken
    -- mkChannelTx' returns ChannelTxErr if neither an input, nor an output for
    -- a channel was found. This happens if the tx in question was randomly
    -- sent to the channel address.
    mkChannelTx' ::
      (AsPerunError e, AsContractError e) =>
      Maybe (TxOutRef, ChannelAction, ChannelDatum) ->
      Maybe (TxOutRef, ChannelDatum) ->
      Contract w s e (Either ChannelTxErr ChannelTx)
    mkChannelTx' Nothing Nothing = return $ Left ChannelTxErr
    mkChannelTx' i o = return . Right $ ChannelTx citx i o

-- | resolveInput resolves the input of a ChainIndexTx. It returns the
-- associated action together with the channel datum.
resolveInput ::
  ChannelID ->
  ChainIndexTx ->
  [ChainIndexTx] ->
  Either ChannelTxErr (TxOutRef, ChannelAction, ChannelDatum)
resolveInput cid citx inputCitxs = do
  let channelInputs = rights $ map (resolveValidChannelInputs citx) cInputs
  case channelInputs of
    [] -> do
      -- We found no channel UTXO as the input to this transaction.
      throwError NoChannelInputErr
    [txinref] -> do
      -- A channel can only be consumed once, so we expect a single result
      -- after analyzing all input UTXOs.
      return txinref
    _else -> do
      -- We have multiple channel inputs referencing the same channel,
      -- Perun invariant does not hold, abort.
      throwError ChannelTxErr
  where
    cInputs = citx ^. citxInputs
    cOutputs = citx ^? citxOutputs . _ValidTx
    ourValidator = channelValidator cid

    inputCitx :: TxOutRef -> Either ChannelTxErr ChainIndexTx
    inputCitx ref = do
      let citxOfInterest = find (\citx' -> citx' ^. citxTxId == txOutRefId ref) inputCitxs
      case citxOfInterest of
        Nothing -> throwError NoChannelInputErr
        Just citx' -> return citx'

    inputUtxo ref@(TxOutRef txId idx) = do
      inputCitx ref >>= \citx' -> case citx' ^. citxOutputs of
        InvalidTx -> throwError InvalidTxErr
        ValidTx outs -> return $ outs !! fromIntegral idx

    resolveValidChannelInputs :: ChainIndexTx -> TxIn -> Either ChannelTxErr (TxOutRef, ChannelAction, ChannelDatum)
    resolveValidChannelInputs citx (TxIn ref (Just (ConsumeScriptAddress (Versioned otherValidator _) r rd)))
      | ourValidator == channelValidator cid = do
        d <- case channelDatumFromDatum rd of
          Right d -> return d
          Left err -> throwError err
        chanAction <- case fromBuiltinData . getRedeemer $ r of
          Just tr -> return tr
          Nothing -> throwError NoChannelInputRedeemerErr
        iUtxo <- inputUtxo ref
        unless (hasValidThreadToken cid (channelToken d) (citoValue iUtxo)) $ throwError WrongThreadTokenErr
        return (ref, chanAction, d)
    resolveValidChannelInputs nCitx (TxIn ref Nothing) = do
      iCitx <- inputCitx ref
      mtype <- resolveTxInType cid ref iCitx nCitx
      iUtxo <- inputUtxo ref
      resolveValidChannelInputs nCitx (TxIn ref mtype)
    resolveValidChannelInputs _ (TxIn ref _) = throwError ChannelTxErr

resolveTxInType :: ChannelID -> TxOutRef -> ChainIndexTx -> ChainIndexTx -> Either ChannelTxErr (Maybe TxInType)
resolveTxInType cid ref pCitx nCitx = do
  -- pCitx is predecessor tx of nCitx, which produced ref.
  idxForRef <- case elemIndex ref . map txInRef . _citxInputs $ nCitx of
    Nothing -> throwError NoChannelInputRefErr
    Just idx -> return . fromIntegral $ idx
  redeemer <- case Map.lookup (RedeemerPtr Spend idxForRef) . _citxRedeemers $ nCitx of
    Nothing -> throwError NoRedeemerForInputErr
    Just r -> return r
  txout <- case pCitx ^? citxOutputs . _ValidTx . ix (fromIntegral $ txOutRefIdx ref) of
    Nothing -> throwError NoOutputMatchinInputErr
    Just txout -> return txout
  datum <- outputDatumToDatum $ citoDatum txout
  return . Just $ ConsumeScriptAddress (Versioned (channelValidator cid) PlutusV2) redeemer datum
  where
    outputDatumToDatum NoOutputDatum = throwError NoDatumForInputErr
    outputDatumToDatum (OutputDatum d) = return d
    outputDatumToDatum (OutputDatumHash dh) =
      case Map.lookup dh . _citxData $ pCitx of
        Nothing -> throwError NoDatumForInputErr
        Just d -> return d

resolveOutput ::
  ChannelID ->
  ChainIndexTx ->
  Either ChannelTxErr (TxOutRef, ChannelDatum)
resolveOutput cid citx = do
  os <- case citx ^. citxOutputs of
    InvalidTx -> throwError UnexpectedInvalidTxErr
    ValidTx os -> return os
  let channelOutputs = rights $ zipWith retrieveChannelOutput [1 ..] os
  case channelOutputs of
    [] -> do
      -- We found no channel UTXO as the output for this transaction.
      throwError NoChannelOutputErr
    [txoutref] -> do
      -- A channel can only appear once in the output of a transaction.
      return txoutref
    _else -> do
      -- We have multiple channel outputs referencing the same channel,
      -- Perun invariant does not hold, abort.
      throwError ChannelTxErr
  where
    retrieveChannelOutput :: Integer -> ChainIndexTxOut -> Either ChannelTxErr (TxOutRef, ChannelDatum)
    retrieveChannelOutput idx citxOut = do
      let txOutRef = TxOutRef (citx ^. citxTxId) idx
      d <- case citoDatum citxOut of
        NoOutputDatum -> do
          -- The citx in question does not contain any datums, so it is not
          -- relevant for our channel.
          throwError NoChannelOutputErr
        OutputDatumHash h -> case Map.lookup h (citx ^. citxData) of
          Nothing -> do
            -- The citx in question does contain some datums, but for some
            -- reason is incapable of returning the data. Unexpected error
            -- which might be because of DB corruption or sync issues.
            throwError ChannelCorruptedChainIndexErr
          Just d -> return d
        OutputDatum d -> return d
      cDatum <- case PlutusTx.fromBuiltinData . getDatum $ d of
        Nothing -> throwError InvalidChannelDatumErr
        Just cDatum -> return cDatum
      unless (isValidDatum cid cDatum) $ throwError InvalidChannelDatumErr
      unless (hasValidThreadToken cid (channelToken cDatum) (citoValue citxOut)) $ throwError WrongThreadTokenErr
      (txOutRef,) <$> channelDatumFromDatum d

parseDatumFromOutputDatum :: ChainIndexTx -> OutputDatum -> Either ChannelTxErr Datum
parseDatumFromOutputDatum _ NoOutputDatum = throwError NoChannelOutputDatumErr
parseDatumFromOutputDatum citx (OutputDatumHash h) = case Map.lookup h $ citx ^. citxData of
  Nothing -> throwError ChannelCorruptedChainIndexErr
  Just d -> return d
parseDatumFromOutputDatum _ (OutputDatum d) = return d

-- TODO: Add more sanity checks for ChannelDatum.
-- TODO: Channel might be closed, fix it plz.
isValidDatum :: ChannelID -> ChannelDatum -> Bool
isValidDatum cid d =
  let ch = channelParameters d
   in getChannelId ch == cid

-- | hasValidThreadToken checks whether the given Value contains a valid
-- threadtoken for the given ChannelID.
hasValidThreadToken :: ChannelID -> ChannelToken -> Value -> Bool
hasValidThreadToken cid ct v =
  let ourValidatorHash = channelHash cid
   in assetClassValueOf v (channelTokenAsset ct) == 1
        && channelTokenSymbol (ctTxOutRef ct) == ctSymbol ct
        && channelTokenName ourValidatorHash == ctName ct

channelDatumFromDatum :: Datum -> Either ChannelTxErr ChannelDatum
channelDatumFromDatum (Datum b) = case fromBuiltinData b of
  Nothing -> throwError NoChannelOutputDatumErr
  Just d@ChannelDatum {} -> return d

-- | dedup removes duplicates from a list of triplets, where the first entry is
-- regarded as the comparative argument.
dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup (oref : orems) = go oref orems []
  where
    go :: Eq a => a -> [a] -> [a] -> [a]
    go ref [] res = reverse (ref : res)
    go ref (nextRef : rems) res =
      if ref == nextRef
        then go nextRef rems res
        else go nextRef rems (ref : res)

mkChannelGenesis :: ChannelTx -> ChannelTxFirst
mkChannelGenesis (ChannelTx citx Nothing (Just l)) = ChannelTx_ citx Nothing l
mkChannelGenesis _ = error "mkChannelGenesis: Invalid channel genesis."

mkChannelStep :: ChannelTx -> ChannelTxStep
mkChannelStep (ChannelTx citx (Just l) (Just r)) = ChannelTx_ citx l r
mkChannelStep _ = error "mkChannelStep: Invalid channel step."

mkChannelLast :: ChannelTx -> ChannelTxLast
mkChannelLast (ChannelTx citx (Just l) Nothing) = ChannelTx_ citx l Nothing
mkChannelLast _ = error "mkChannelLast: Invalid channel last."
