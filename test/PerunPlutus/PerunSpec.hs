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
import Plutus.Contract.Test.ContractModel
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

-- | PerunModel: State, Timelock, Funding, Funded
newtype PerunModel = PerunModel (Maybe (ChannelState, Integer, [Integer], Bool))
  deriving (P.Show, P.Eq, Data)

deriving instance P.Eq (ContractInstanceKey PerunModel w schema err params)

deriving instance P.Ord (ContractInstanceKey PerunModel w schema err params)

deriving instance P.Show (ContractInstanceKey PerunModel w schema err params)

instance ContractModel PerunModel where
  {--
   - On-chain actions.
   - Only supporting two party channels for now.
  --}
  data Action PerunModel
    = -- Start the channel (with insufficient funding)
      -- Participants ChanelID Balances Timeloc
      Start [Wallet] Integer [Integer] Integer
    | -- Fund the channel
      -- Funder Index ChannelID
      Fund Wallet Integer Integer
    | -- Abort the channel
      -- Issuer wallets ChannelID
      Abort Wallet [Wallet] Integer
    | -- Open Issuer Participants ChannelID Balances Timelock.
      Open Wallet [Wallet] Integer [Integer] Integer
    | -- Close Issuer Participants ChannelId.
      Close Wallet [Wallet] Integer
    | -- ForceClose Issuer Participants ChannelID.
      ForceClose Wallet [Wallet] Integer
    | -- Dispute -> Issuer Participants ChannelId ProposedState
      Dispute Wallet [Wallet] Integer ChannelState
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
    Participant :: Wallet -> ContractInstanceKey PerunModel () ChannelSchema PerunError ()
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
  initialState = PerunModel Nothing

  -- Opening a channel sets the agreed upon initial `ChannelState` for the
  -- channel.
  -- Furthermore the `Contract` managing the funds has to know that it holds
  -- the balances for each wallet.
  nextState (Start parties cid startBalances timeLock) = do
    modifyContractState $ \_ ->
      PerunModel
        ( Just
            ( ChannelState
                { channelId = cid,
                  balances = startBalances,
                  version = 0,
                  final = False
                },
              timeLock,
              head startBalances : tail (map (const 0) startBalances),
              False
            )
        )
    withdraw (head parties) $ Ada.lovelaceValueOf $ head startBalances
    wait 1
  nextState (Fund funder idx _) = do
    modifyContractState
      ( \case
          PerunModel Nothing -> P.error "Funding only works on existing channels"
          PerunModel (Just (_, _, _, True)) -> P.error "Funding only works on unfunded channels"
          PerunModel (Just (cs, tl, oldFunding, _)) ->
            let newFunding = addFunding (balances cs !! idx) idx oldFunding
             in PerunModel
                  ( Just
                      (cs, tl, newFunding, newFunding == balances cs)
                  )
      )
    s <-
      getContractState >>= \case
        PerunModel (Just (s, _, _, _)) -> return s
        _ -> P.error "unable to read contract state"
    withdraw funder $ Ada.lovelaceValueOf (balances s !! idx)
    wait 1
  nextState (Abort _ parties _) = do
    curFunding <-
      getContractState >>= \case
        PerunModel (Just (_, _, f, False)) -> return f
        _ -> P.error "Abort only works on existing channels"

    zipWithM_ deposit parties (map Ada.lovelaceValueOf curFunding)
    wait 1
  nextState (Open funder _ cid openBalances timeLock) = do
    modifyContractState $ \_ ->
      PerunModel
        ( Just
            ( ChannelState
                { channelId = cid,
                  balances = openBalances,
                  version = 0,
                  final = False
                },
              timeLock,
              [],
              True
            )
        )
    -- Move funds from chan -> Contract instance.
    withdraw funder $ Ada.lovelaceValueOf $ sum openBalances
    wait 1
  nextState (Update cs) = do
    modifyContractState
      ( \case
          PerunModel Nothing -> P.error "Update only works on existing channels"
          PerunModel (Just (_, tl, newFunding, newFunded)) -> PerunModel . Just $ (cs, tl, newFunding, newFunded)
      )
  nextState Dispute {} = do
    modifyContractState
      ( \case
          PerunModel Nothing -> P.error "Dispute only works on existing channels"
          PerunModel (Just (_, _, _, False)) -> P.error "Dispute only works on funded channels"
          x -> x
      )
    wait 1
  nextState (Close _ parties _) = do
    s <-
      getContractState >>= \case
        PerunModel Nothing -> P.error "close only works on existing channels"
        PerunModel (Just (_, _, _, False)) -> P.error "Close only works on funded channels"
        PerunModel (Just (s, _, _, True)) -> return s
    zipWithM_ deposit parties (map Ada.lovelaceValueOf (balances s))
    wait 1
  -- ForceClosing does nothing to the contract state, yet.
  nextState (ForceClose _ parties _) = do
    s <-
      getContractState >>= \case
        PerunModel Nothing -> P.error "ForceClose only works on existing channels"
        PerunModel (Just (_, _, _, False)) -> P.error "Close only works on funded channels"
        PerunModel (Just (s, _, _, True)) -> return s
    zipWithM_ deposit parties (map Ada.lovelaceValueOf (balances s))
    wait 1
  nextState Finalize =
    modifyContractState
      ( \case
          PerunModel Nothing -> P.error "Finalize only works on existing channels"
          PerunModel (Just (s, tl, fx, f)) -> PerunModel . Just $ (s {final = True, version = version s + 1}, tl, fx, f)
      )
  nextState (Wait duration) = wait duration
  nextState (MaliciousFund n _ _ _ _) = invariant >> wait n
  nextState (MaliciousAbort n _ _ _) = invariant >> wait n
  nextState (MaliciousClose n _ _ _ _ _) = invariant >> wait n
  nextState (MaliciousForceClose n _ _ _) = invariant >> wait n
  nextState (MaliciousDispute n _ _ _ _ _) = invariant >> wait n

  perform handle _ s cmd = case cmd of
    Start parties cid startBalances timeLock -> do
      Trace.callEndpoint @"start"
        (handle $ Participant (head parties))
        ( OpenParams
            cid
            (map mockWalletPaymentPubKey parties)
            (map mockWalletPaymentPubKeyHash parties)
            startBalances
            timeLock
        )
      delay 1
    Fund funder idx cid -> do
      Trace.callEndpoint @"fund"
        (handle $ Participant funder)
        (FundParams cid idx)
      delay 1
    Abort issuer _ cid -> do
      Trace.callEndpoint @"abort"
        (handle $ Participant issuer)
        (AbortParams cid)
      delay 1
    Open wf parties cid bals timelock -> do
      Trace.callEndpoint @"open"
        (handle $ Participant wf)
        ( OpenParams
            cid
            (map mockWalletPaymentPubKey parties)
            (map mockWalletPaymentPubKeyHash parties)
            bals
            timelock
        )
      delay 1
    Update {} -> do
      -- Update is an offchain action, so we do not perform any computation
      -- onchain wise.
      return ()
    Dispute issuer parties _ chState -> do
      wss <- mapM Trace.agentState parties
      let sks = map (unPaymentPrivateKey . Trace.ownPaymentPrivateKey) wss
      let pks = map Trace.ownPaymentPublicKey wss
      Trace.callEndpoint @"dispute"
        (handle $ Participant issuer)
        ( DisputeParams
            pks
            (SignedState (map (signMessage' chState) sks))
        )
      delay 1
    Close issuer parties _ -> do
      let cs = s ^. contractState
      chan <- case cs of
        PerunModel Nothing -> Trace.throwError . Trace.GenericError $ "no channel to close"
        PerunModel (Just (chan, _, _, _)) -> return chan
      (ss, pks, _) <- verifiedSignedStateAndKeys parties chan
      Trace.callEndpoint @"close" (handle $ Participant issuer) (CloseParams pks ss)
      delay 1
    ForceClose issuer _ cID -> do
      Trace.callEndpoint @"forceClose"
        (handle $ Participant issuer)
        (ForceCloseParams cID)
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
        PerunModel Nothing -> Trace.throwError . Trace.GenericError $ "no channel to close"
        PerunModel (Just (chan, _, _, _)) -> return chan
      (_, pks, _) <- verifiedSignedStateAndKeys parties chan
      requireInvalidTxEndpoint @"close" (handle $ Adversary w) (EvilClose cID (SignedState [signMessage' (ChannelState 0 [0] 0 False) sk]) pks bals c) "malicious closing should not work"
    MaliciousForceClose _ w cid c -> do
      requireInvalidTxEndpoint @"forceClose" (handle $ Adversary w) (EvilForceClose cid c) "malicious forceClose should not work"
    MaliciousDispute _ w cid parties bals c -> do
      let cs = s ^. contractState
      chan <- case cs of
        PerunModel Nothing -> Trace.throwError . Trace.GenericError $ "no channel to close"
        PerunModel (Just (chan, _, _, _)) -> return chan
      (ss, pks, _) <- verifiedSignedStateAndKeys parties chan
      requireInvalidTxEndpoint @"dispute" (handle $ Adversary w) (EvilDispute cid pks (map paymentPubKeyHash pks) bals ss c) "malicious dispute should not work"

verifiedSignedStateAndKeys :: [Wallet] -> ChannelState -> SpecificationEmulatorTrace (SignedState, [PaymentPubKey], [Crypto.XPrv])
verifiedSignedStateAndKeys parties chan = do
  wss <- mapM Trace.agentState parties
  let sks = map (unPaymentPrivateKey . Trace.ownPaymentPrivateKey) wss
  let pks = map Trace.ownPaymentPublicKey wss
  return (SignedState (map (signMessage' chan) sks), pks, sks)

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
