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
import Data.List (find, foldl')
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
allTxosAt addr = go def
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
  let resolveTx ref =
        (txFromTxId . txOutRefId $ ref) >>= \case
          Nothing -> do
            -- Abort if the chainindex is corrupted and could not retrieve
            -- the necessary transactions.
            throwing _SubscriptionError CorruptedChainIndexErr
          Just r -> return r
  uniqueCitxs <- dedup <$> mapM resolveTx txoRefs
  channelTxs <- rights <$> mapM (mkChannelTx cid) uniqueCitxs
  return . filterValidStart . partitionByChannelToken $ channelTxs
  where
    filterValidStart = filter (\(ChannelTxPool _ ctxs) -> any isValidStart ctxs)
    isValidStart (ChannelTx _ Nothing (Just _)) = True
    isValidStart _ = False

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
  let resolveTx ref =
        txFromTxId ref >>= \case
          Nothing -> do
            -- Abort if the chainindex is corrupted and could not retrieve
            -- the necessary transactions.
            throwing _SubscriptionError CorruptedChainIndexErr
          Just r -> return r
  allInputTxs <- mapM (resolveTx . txOutRefId . txInRef) (citx ^. citxInputs)
  i <- case resolveInput cid citx allInputTxs of
    Right i -> return $ Just i
    Left NoChannelInputErr -> return Nothing
    _else -> throwing _SubscriptionError CorruptedChainIndexErr
  o <- case resolveOutput cid citx of
    Right o -> return $ Just o
    Left NoChannelOutputErr -> return Nothing
    _else -> throwing _SubscriptionError CorruptedChainIndexErr
  ct <- retrieveChannelToken i o
  (fmap (ct,)) <$> mkChannelTx' i o
  where
    retrieveChannelToken ::
      (AsPerunError e, AsContractError e) =>
      Maybe (TxOutRef, ChannelAction, ChannelDatum) ->
      Maybe (TxOutRef, ChannelDatum) ->
      Contract w s e ChannelToken
    retrieveChannelToken Nothing Nothing = throwing _SubscriptionError CorruptedChainIndexErr
    retrieveChannelToken (Just (_, _, d)) Nothing = return $ channelToken d
    retrieveChannelToken Nothing (Just (_, d)) = return $ channelToken d
    retrieveChannelToken (Just (_, _, d)) (Just (_, d')) = do
      let iToken = channelToken d
          oToken = channelToken d'
      unless (iToken == oToken) $
        throwing _SubscriptionError CorruptedChainIndexErr
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

data ChannelTxErr
  = ChannelTxErr
  | NoChannelOutputDatumErr
  | NoChannelInputRedeemerErr
  | NoChannelInputErr
  | NoStartStateFoundErr
  | InvalidTxErr
  | NoChannelOutputErr
  | UnexpectedInvalidTxErr
  | ChannelCorruptedChainIndexErr
  | WrongThreadTokenErr
  | InvalidChannelDatumErr
  | InvalidTxOutRefErr
  | InvalidThreadTokenErr
  deriving (Show)

resolveInput ::
  ChannelID ->
  ChainIndexTx ->
  [ChainIndexTx] ->
  Either ChannelTxErr (TxOutRef, ChannelAction, ChannelDatum)
resolveInput cid citx inputCitxs = do
  let cInputs = citx ^. citxInputs
      ourValidator = channelValidator cid
  let inputValue (TxOutRef txId idx) = do
        let citxOfInterest = find (\citx' -> citx' ^. citxTxId == txId) inputCitxs
        output <- case citxOfInterest of
          Nothing -> throwError NoChannelInputErr
          Just citx' -> case citx' ^. citxOutputs of
            InvalidTx -> throwError InvalidTxErr
            ValidTx outs -> return $ outs !! fromIntegral idx
        return $ citoValue output
  channelInputs <-
    rights
      <$> mapM
        ( \case
            TxIn ref (Just (ConsumeScriptAddress (Versioned otherValidator _) r rd))
              | ourValidator == otherValidator -> do
                d <- case channelDatumFromDatum rd of
                  Right d -> return d
                  Left err -> throwError err
                tr <- case fromBuiltinData . getRedeemer $ r of
                  Just tr -> return tr
                  Nothing -> throwError NoChannelInputRedeemerErr
                val <- inputValue ref
                unless (hasValidThreadToken cid (channelToken d) val) $ throwError WrongThreadTokenErr
                return $ Right (ref, tr, d)
            _else -> return $ Left ChannelTxErr
        )
        cInputs
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
