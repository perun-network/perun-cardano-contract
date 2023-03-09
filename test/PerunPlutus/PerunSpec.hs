{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module PerunPlutus.PerunSpec where

import qualified Cardano.Crypto.Wallet as Crypto
import Control.Lens hiding (both)
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Data
import Data.Monoid (Last (..))
import qualified Data.Semigroup as Semigroup
import Ledger hiding (version)
import qualified Ledger.Ada as Ada
import qualified Ledger.Value as Value
import Perun hiding (ChannelAction (..))
import Perun.Error
import PerunPlutus.Test.EvilContract
import Plutus.Contract.Oracle
import qualified Plutus.Contract.Request as Trace
import Plutus.Contract.Test (Wallet, mockWalletPaymentPubKey, mockWalletPaymentPubKeyHash, w1, w2, w3, w4)
import Plutus.Contract.Test.ContractModel as CM
import qualified Plutus.Contract.Types as ET
import qualified Plutus.Trace.Effects.Assert as TA (assert)
import Plutus.Trace.Effects.RunContract (ContractConstraints)
import qualified Plutus.Trace.Emulator as Trace
import qualified Plutus.Trace.Emulator.Types as ET
import PlutusTx.Prelude hiding (mapM, unless)
import Wallet.Emulator.MultiAgent as Emulator
import qualified Wallet.Emulator.Wallet as Trace
import qualified Prelude as P

wallets :: [Wallet]
wallets = [w1, w2, w3]

adversaryWallets :: [Wallet]
adversaryWallets = [w4]

exVal :: Value.Value
exVal = Ada.lovelaceValueOf 1

type PerunModel = Maybe PerunModelState

-- | PerunModelState
data PerunModelState = PerunModelState
  { _chanState :: !ChannelState,
    _chanToken :: !SymToken,
    _chanTimeLock :: !Integer,
    _chanFunding :: ![Integer],
    _chanFunded :: !Bool,
    _chanDisputed :: !Bool
  }
  deriving (P.Show, P.Eq, Data)

makeLenses ''PerunModelState

deriving instance P.Eq (ContractInstanceKey PerunModel w schema err params)

deriving instance P.Ord (ContractInstanceKey PerunModel w schema err params)

deriving instance P.Show (ContractInstanceKey PerunModel w schema err params)

instance ContractModel PerunModel where
  {--
   - On-chain actions.
   - Only supporting two party channels for now.
  --}
  data Action PerunModel
    = 
      -- Start the channel (with insufficient funding)
      -- Participants ChanelID Balances Timelock Nonce
      Start [Wallet] ChannelID [Integer] Integer Integer
    | -- Fund the channel
      -- Funder Index ChannelID
      Fund Wallet Integer ChannelID SymToken
    | -- Abort the channel
      -- Issuer wallets ChannelID
      Abort Wallet [Wallet] ChannelID SymToken
    | -- Open Issuer Participants ChannelID Balances Timelock Nonce
      Open Wallet [Wallet] ChannelID [Integer] Integer Integer
    | -- Close Issuer Participants ChannelId.
      Close Wallet [Wallet] ChannelID SymToken
    | -- ForceClose Issuer Participants ChannelID.
      ForceClose Wallet [Wallet] ChannelID SymToken
    | -- Dispute -> Issuer Participants ChannelId ProposedState
      Dispute Wallet [Wallet] ChannelID ChannelState SymToken
    | -- Update
      Update ChannelState
    | -- Finalize: sets the final bit in the state
      Finalize
    | -- Wait Slots
      Wait Integer
    | -- MaliciousFund -> WaitTime Issuer ChannelID FunderIndex FundingTestcase.
      MaliciousFund Integer Wallet ChannelID Integer FundCase
    | MaliciousAbort Integer Wallet ChannelID AbortCase
    | MaliciousClose Integer Wallet ChannelID [Wallet] [Integer] CloseCase
    | MaliciousForceClose Integer Wallet ChannelID ForceCloseCase
    | MaliciousDispute Integer Wallet ChannelID [Wallet] [Integer] DisputeCase
    deriving stock (P.Show, P.Eq)
    deriving (Data)

  data ContractInstanceKey PerunModel w schema err param where
    -- Only one type of contract under test, so we define the
    -- `ContractInstanceKey` with a single constructor distinguished by the
    -- wallet they are running in.
    Participant :: Wallet -> ContractInstanceKey PerunModel TokenState ChannelSchema PerunError ()
    Adversary :: Wallet -> ContractInstanceKey PerunModel EvilContractState EvilSchema PerunError ()

  -- Start contract instances for the perun contract `Participant` and a
  -- malicious contract `Adversary`.
  initialInstances =
    ((`StartContract` ()) . Participant <$> wallets)
      ++ ((`StartContract` ()) . Adversary <$> wallets)

  instanceContract _ Participant {} _ = contract
  instanceContract _ Adversary {} _ = evilContract

  instanceWallet (Participant w) = w
  instanceWallet (Adversary w) = w

  arbitraryAction _ = P.undefined

  -- Initially there is not channel.
  initialState = Nothing

  -- Opening a channel sets the agreed upon initial `ChannelState` for the
  -- channel.
  -- Furthermore the `Contract` managing the funds has to know that it holds
  -- the balances for each wallet.
  nextState (Start parties cid startBalances timeLock _) = do
    ct <- createToken "ChannelToken"
    modifyContractState $ \_ ->
      Just $
        PerunModelState
          { _chanState =
              ChannelState
                { channelId = cid,
                  balances = startBalances,
                  version = 0,
                  final = False
                },
            _chanToken = ct,
            _chanTimeLock = timeLock,
            _chanFunding = head startBalances : tail (map (const 0) startBalances),
            _chanFunded = False,
            _chanDisputed = False
          }
    withdraw (head parties) . Ada.lovelaceValueOf $ head startBalances
    wait 1
    CM.mint $ symAssetClassValue ct 1

  nextState (Fund funder idx _ _) = do
    modifyContractState
      ( \case
          Nothing -> P.error "Funding only works on existing channels"
          Just pms@(PerunModelState chst _  _tl oldFunding isFunded isDisputed)
            | isFunded || isDisputed -> P.error "Funding only works on unfunded & undisputed channels"
            | otherwise ->
              let newFunding = addFunding (balances chst !! idx) idx oldFunding
               in Just $
                    pms
                      { _chanFunding = newFunding,
                        _chanFunded = newFunding == balances chst
                      }
      )
    wait 3
    s <-
      getContractState >>= \case
        Nothing -> P.error "unable to read contract state"
        Just ps -> return $ ps ^. chanState
    withdraw funder $ Ada.lovelaceValueOf (balances s !! idx)
  nextState (Abort _ parties _ _) = do
    (curFunding, token) <-
      getContractState >>= \case
        Nothing -> P.error "Abort only works on existing channels"
        Just ps -> return (ps ^. chanFunding, ps ^. chanToken)
    burn $ symAssetClassValue token 1
    zipWithM_ deposit parties (map Ada.lovelaceValueOf curFunding)
    wait 1
  nextState (Open funder _ cid openBalances timeLock _) = do
    ct <- createToken "ChannelToken"
    modifyContractState $ \_ ->
      Just $
        PerunModelState
          { _chanState =
              ChannelState
                { channelId = cid,
                  balances = openBalances,
                  version = 0,
                  final = False
                },
            _chanToken = ct,
            _chanTimeLock = timeLock,
            _chanFunding = [],
            _chanFunded = True,
            _chanDisputed = False
          }
    -- Move funds from chan -> Contract instance.
    withdraw funder $ Ada.lovelaceValueOf $ sum openBalances
    wait 1
    CM.mint $ symAssetClassValue ct 1
  nextState (Update cs) = do
    modifyContractState
      ( \case
          Nothing -> P.error "Update only works on existing channels"
          (Just ps) -> Just $ ps & chanState .~ cs
      )
  nextState Dispute {} = do
    modifyContractState
      ( \case
          Nothing -> P.error "Dispute only works on existing channels"
          Just pms
            | not (pms ^. chanFunded) -> P.error "Dispute only works on funded channels"
            | otherwise -> Just $ pms & chanDisputed .~ True
      )
    wait 3
  nextState (Close _ parties _ _) = do
    (s, t) <-
      getContractState >>= \case
        Nothing -> P.error "close only works on existing channels"
        Just pms
          | not (pms ^. chanFunded) -> P.error "Close only works on funded channels"
          | otherwise -> return (pms ^. chanState, pms ^. chanToken)
    wait 1
    burn $ symAssetClassValue t 1
    zipWithM_ deposit parties (map Ada.lovelaceValueOf (balances s))
  -- ForceClosing does nothing to the contract state, yet.
  nextState (ForceClose _ parties _ _) = do
    (s, t) <-
      getContractState >>= \case
        Nothing -> P.error "ForceClose only works on existing channels"
        Just pms
          | not (pms ^. chanFunded) -> P.error "Close only works on funded channels"
          | otherwise -> return (pms ^. chanState, pms ^. chanToken)
    burn $ symAssetClassValue t 1
    zipWithM_ deposit parties (map Ada.lovelaceValueOf (balances s))
    wait 1
  nextState Finalize =
    modifyContractState
      ( \case
          Nothing -> P.error "Finalize only works on existing channels"
          Just pms -> Just $ pms & chanState %~ \os -> os {final = True, version = version os + 1}
      )
  nextState (Wait duration) = wait duration
  nextState (MaliciousFund n _ _ _ _) = invariant >> wait n
  nextState (MaliciousAbort n _ _ _) = invariant >> wait n
  nextState (MaliciousClose n _ _ _ _ _) = invariant >> wait n
  nextState (MaliciousForceClose n _ _ _) = invariant >> wait n
  nextState (MaliciousDispute n _ _ _ _ _) = invariant >> wait n

  perform handle tokenMap s cmd = case cmd of
    Start parties cid startBalances timeLock nonce -> do
      let h = handle $ Participant (head parties)
      Trace.callEndpoint @"start"
        h
        ( OpenParams
            cid
            (map mockWalletPaymentPubKey parties)
            (map mockWalletPaymentPubKeyHash parties)
            startBalances
            timeLock
            nonce
        )
      delay 1
      Trace.observableState h >>= \case
        Last (Just ct) -> registerToken "ChannelToken" (channelTokenAsset ct)
        _ -> P.error "Could not find channel token"
    Fund funder idx cid channelSymToken -> do
      Trace.callEndpoint @"fund"
        (handle $ Participant funder)
        (FundParams cid (tokenMap channelSymToken) idx)
      delay 3
    Abort issuer _ cid channelSymToken -> do
      Trace.callEndpoint @"abort"
        (handle $ Participant issuer)
        (AbortParams cid (tokenMap channelSymToken))
      delay 1
    Open wf parties cid bals timelock nonce -> do
      let h = handle $ Participant wf
      Trace.callEndpoint @"open"
        h
        ( OpenParams
            cid
            (map mockWalletPaymentPubKey parties)
            (map mockWalletPaymentPubKeyHash parties)
            bals
            timelock
            nonce
        )
      delay 1
      os <- Trace.observableState h
      case getLast os of
        Just ct -> registerToken "ChannelToken" (channelTokenAsset ct)
        Nothing -> P.error "Could not find channel token"

    Update {} -> do
      -- Update is an offchain action, so we do not perform any computation
      -- onchain wise.
      return ()
    Dispute issuer parties cid chState channelSymToken -> do
      wss <- mapM Trace.agentState parties
      let sks = map (unPaymentPrivateKey . Trace.ownPaymentPrivateKey) wss
      let pks = map Trace.ownPaymentPublicKey wss
      Trace.callEndpoint @"dispute"
        (handle $ Participant issuer)
        ( DisputeParams
            cid
            (tokenMap channelSymToken)
            pks
            (AllSignedStates (map (signMessage' chState) sks))
        )
      delay 3
    Close issuer parties cid channelSymToken -> do
      let cs = s ^. contractState
      chan <- case cs of
        Nothing -> Trace.throwError . Trace.GenericError $ "no channel to close"
        Just pms -> return $ pms ^. chanState
      (ss, pks, _) <- verifiedSignedStateAndKeys parties chan
      Trace.callEndpoint @"close" (handle $ Participant issuer) (CloseParams cid (tokenMap channelSymToken) pks ss)
      delay 1
    ForceClose issuer _ cID channelSymToken -> do
      Trace.callEndpoint @"forceClose"
        (handle $ Participant issuer)
        (ForceCloseParams cID (tokenMap channelSymToken))
      delay 1
    Finalize -> return ()
    Wait duration -> delay duration
    MaliciousFund _ w cID idx fc ->
      requireInvalidTxEndpoint @"fund" (handle $ Adversary w) (EvilFund cID idx fc) "malicious funding should not work"
    MaliciousAbort _ w cID ac ->
      requireInvalidTxEndpoint @"abort" (handle $ Adversary w) (EvilAbort cID ac) "malicious abourt should not work"
    MaliciousClose _ w cID parties bals c -> do
      let cs = s ^. contractState
      p <- Trace.agentState w
      let sk = unPaymentPrivateKey . Trace.ownPaymentPrivateKey $ p
      chan <- case cs of
        Nothing -> Trace.throwError . Trace.GenericError $ "no channel to close"
        Just pms -> return $ pms ^. chanState
      (_, pks, _) <- verifiedSignedStateAndKeys parties chan
      requireInvalidTxEndpoint @"close" (handle $ Adversary w) (EvilClose cID (AllSignedStates [signMessage' (ChannelState (ChannelID "abc") [0] 0 False) sk]) pks bals c) "malicious closing should not work"
    MaliciousForceClose _ w cid c -> do
      requireInvalidTxEndpoint @"forceClose" (handle $ Adversary w) (EvilForceClose cid c) "malicious forceClose should not work"
    MaliciousDispute _ w cid parties bals c -> do
      let cs = s ^. contractState
      chan <- case cs of
        Nothing -> Trace.throwError . Trace.GenericError $ "no channel to close"
        Just pms -> return $ pms ^. chanState
      (ss, pks, _) <- verifiedSignedStateAndKeys parties chan
      requireInvalidTxEndpoint @"dispute" (handle $ Adversary w) (EvilDispute cid pks (map paymentPubKeyHash pks) bals ss c) "malicious dispute should not work"

verifiedSignedStateAndKeys :: [Wallet] -> ChannelState -> SpecificationEmulatorTrace (AllSignedStates, [PaymentPubKey], [Crypto.XPrv])
verifiedSignedStateAndKeys parties chan = do
  wss <- mapM Trace.agentState parties
  let sks = map (unPaymentPrivateKey . Trace.ownPaymentPrivateKey) wss
  let pks = map Trace.ownPaymentPublicKey wss
  return (AllSignedStates (map (signMessage' chan) sks), pks, sks)

-- | Model does not change!
invariant :: Spec s ()
invariant = return ()

data EndpointError = ExpectedEndpointError P.String | UnexpectedEndpointError P.String deriving (P.Show, P.Eq)

-- | requireInvalidTxEndpoint calls the given endpoint with the given value and
-- asserts that the contract, which has to track its sent tx in its state, sent
-- a tx which was rejected onchain because its validation failed.
requireInvalidTxEndpoint ::
  forall l ev s e.
  ( ToJSON ev,
    FromJSON e,
    ContractConstraints s,
    Trace.HasEndpoint l ev s
  ) =>
  Trace.ContractHandle EvilContractState s e ->
  ev ->
  P.String ->
  SpecificationEmulatorTrace ()
requireInvalidTxEndpoint h v msg = do
  Trace.callEndpoint @l h v
  delay 1
  css <- Trace.getContractState h
  case view ET.observableState $ ET.instContractState css of
    Nothing -> Trace.throwError . Trace.GenericError $ "evilcontract did not send tx"
    Just (Semigroup.Last txid) -> TA.assert msg $ \es ->
      case transaction (view (Emulator.chainState . Trace.chainNewestFirst) es) txid of
        Just _ -> True
        Nothing -> False
