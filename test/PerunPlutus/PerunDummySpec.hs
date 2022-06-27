{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module PerunPlutus.PerunDummySpec where

import Control.Lens hiding (both)
import Data.Data
import Data.Text (Text)
import Data.Tuple.Extra (both)
import qualified Ledger.Ada as Ada
import Ledger.Address
import qualified Ledger.Value as Value
import PerunDummy
import Plutus.Contract.Oracle
import Plutus.Contract.Test (Wallet, mockWalletPaymentPubKey, mockWalletPaymentPubKeyHash, w1, w2, w3)
import Plutus.Contract.Test.ContractModel
import qualified Plutus.Trace.Emulator as Trace
import qualified Wallet.Emulator.Wallet as Trace
import Control.Monad

wallets :: [Wallet]
wallets = [w1, w2, w3]

exVal :: Value.Value
exVal = Ada.lovelaceValueOf 1

newtype PerunModel = PerunModel (Maybe (ChannelState, Integer)) deriving (Show, Eq, Data)

deriving instance Eq (ContractInstanceKey PerunModel w schema err params)

deriving instance Ord (ContractInstanceKey PerunModel w schema err params)

deriving instance Show (ContractInstanceKey PerunModel w schema err params)

instance ContractModel PerunModel where
  {--
   - On-chain actions.
   - Only supporting two party channels for now.
  --}
  data Action PerunModel
    = -- | Open Issuer Participants ChannelID Balances Timelock.
      Open Wallet [Wallet] Integer [Integer] Integer
    | -- | Close Issuer Participants ChannelId.
      Close Wallet [Wallet] Integer
    | -- | ForceClose Issuer Participants ChannelID.
      ForceClose Wallet [Wallet] Integer
    | -- | Dispute -> Issuer Participants ChannelId ProposedState
      Dispute Wallet [Wallet] Integer ChannelState
    | -- | Update
      Update ChannelState
    | -- | Finalize: sets the final bit in the state
      Finalize
    | -- | Wait Slots
      Wait Integer
    deriving stock (Show, Eq)
    deriving (Data)

  data ContractInstanceKey PerunModel w schema err param where
    -- Only one type of contract under test, so we define the
    -- `ContractInstanceKey` with a single constructor distinguished by the
    -- wallet they are running in.
    Participant :: Wallet -> ContractInstanceKey PerunModel () ChannelSchema Text ()

  initialInstances = (`StartContract` ()) . Participant <$> wallets

  instanceContract _ Participant {} _ = contract

  instanceWallet (Participant w) = w

  arbitraryAction _ = undefined

  -- Initially there is not channel.
  initialState = PerunModel Nothing

  -- Opening a channel sets the agreed upon initial `ChannelState` for the
  -- channel.
  -- Furthermore the `Contract` managing the funds has to know that it holds
  -- the balances for each wallet.
  nextState (Open funder _ cid openBalances timeLock) = do
    modifyContractState $ \_ ->
      PerunModel
        ( Just
            (ChannelState
              { channelId = cid,
                balances = openBalances,
                version = 0,
                final = False
              }, timeLock)
        )
    -- Move funds from chan -> Contract instance.
    withdraw funder $ Ada.lovelaceValueOf $ sum openBalances
    wait 1
  nextState (Update cs) = do
    modifyContractState (\case
        PerunModel Nothing -> error "Update only works on existing channels"
        PerunModel (Just (_, tl)) ->  PerunModel . Just $ (cs, tl))
  -- TODO add disputed bit to PerunModel and handle it here!
  nextState Dispute {} = do
    modifyContractState (\case
        PerunModel Nothing -> error "Dispute only works on existing channels"
        x -> x)
    wait 1
  nextState (Close _ parties _) = do
    s <-
      getContractState >>= \case
        PerunModel Nothing -> error "close only works on existing channels"
        PerunModel (Just (s, _)) -> return s
    zipWithM_ deposit parties (map Ada.lovelaceValueOf (balances s))
    wait 1
  -- ForceClosing does nothing to the contract state, yet.
  nextState (ForceClose _ parties _)  = do
    s <-
      getContractState >>= \case
        PerunModel Nothing -> error "ForceClose only works on existing channels"
        PerunModel (Just (s, _)) -> return s
    zipWithM_ deposit parties (map Ada.lovelaceValueOf (balances s))
    wait 1
  nextState Finalize = modifyContractState (\case
        PerunModel Nothing -> error "Finalize only works on existing channels"
        PerunModel (Just (s, tl)) ->  PerunModel . Just $ (s{final=True}, tl))
  nextState (Wait duration) = wait duration

  perform handle _ s cmd = case cmd of
    (Open wf parties cid balances timelock) -> do
      Trace.callEndpoint @"open"
        (handle $ Participant wf)
        ( OpenParams
            cid
            (map mockWalletPaymentPubKey parties)
            (map mockWalletPaymentPubKeyHash parties)
            balances
            timelock
        )
      delay 1
    Update {} -> do
      -- Update is an offchain action, so we do not perform any computation
      -- onchain wise.
      return ()
    (Dispute issuer parties _ state) -> do
      walletStates <- mapM (Trace.agentState) parties 
      let sks = map (unPaymentPrivateKey . Trace.ownPaymentPrivateKey) walletStates
      let pks = map Trace.ownPaymentPublicKey walletStates
      Trace.callEndpoint @"dispute"
        (handle $ Participant issuer)
        ( DisputeParams
            pks
            (SignedState (map (signMessage' state) sks))
        )
      delay 1
    (Close issuer parties cid) -> do
      let cs = s ^. contractState
      chan <- case cs of
        PerunModel Nothing -> Trace.throwError . Trace.GenericError $ "no channel to close"
        PerunModel (Just (chan, _)) -> return chan
      walletStates <- mapM (Trace.agentState) parties 
      let sks = map (unPaymentPrivateKey . Trace.ownPaymentPrivateKey) walletStates
      let pks = map Trace.ownPaymentPublicKey walletStates
      Trace.callEndpoint @"close"
        (handle $ Participant issuer)
        ( CloseParams
            pks
            (SignedState (map (signMessage' chan) sks))
        )
      delay 1
    (ForceClose issuer _ cID) -> do
      Trace.callEndpoint @"forceClose"
        (handle $ Participant issuer)
        (ForceCloseParams cID)
      delay 1
    Finalize -> return ()
    Wait duration -> delay duration

