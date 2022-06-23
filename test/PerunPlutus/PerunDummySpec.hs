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
    = -- | Open Wallet (Wallet, Wallet) -> ChannelID -> BalanceA -> BalanceB -> timelock.
      Open Wallet (Wallet, Wallet) Integer Integer Integer Integer
    | -- | Close Issuer -> (Wallet, Wallet) -> ChannelID -> BalanceA -> BalanceB -> Version.
      Close Wallet (Wallet, Wallet) Integer Integer Integer Integer
    | -- | ForceClosing Issuer -> (Wallet, Wallet) -> ChannelID.
      ForceClose Wallet (Wallet, Wallet) Integer
    | -- | Disputing -> Issuer (Wallet, Wallet) -> ChannelId -> ChannelState
      Dispute Wallet (Wallet, Wallet) Integer ChannelState
    | -- | Off-chain action of updating the channelstate.
      Update ChannelState
    |
      Finalize
    | 
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
  nextState (Open wf (_, _) cid balA balB timelock) = do
    modifyContractState $ \_ ->
      PerunModel
        ( Just
            (ChannelState
              { channelId = cid,
                balanceA = balA,
                balanceB = balB,
                version = 0,
                final = False
              }, timelock)
        )
    -- Move funds from chan -> Contract instance.
    withdraw wf $ Ada.lovelaceValueOf (balA + balB)
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
  nextState (Close _ (wa, wb) _ _ _ _) = do
    s <-
      getContractState >>= \case
        PerunModel Nothing -> error "close only works on existing channels"
        PerunModel (Just (s, _)) -> return s
    deposit wa $ Ada.lovelaceValueOf (balanceA s)
    deposit wb $ Ada.lovelaceValueOf (balanceB s)
    wait 1
  -- ForceClosing does nothing to the contract state, yet.
  nextState (ForceClose _ (wa, wb) _)  = do
    s <-
      getContractState >>= \case
        PerunModel Nothing -> error "ForceClose only works on existing channels"
        PerunModel (Just (s, _)) -> return s
    deposit wa $ Ada.lovelaceValueOf (balanceA s)
    deposit wb $ Ada.lovelaceValueOf (balanceB s)
    wait 1
  nextState Finalize = modifyContractState (\case
        PerunModel Nothing -> error "Finalize only works on existing channels"
        PerunModel (Just (s, tl)) ->  PerunModel . Just $ (s{final=True}, tl))
  nextState (Wait duration) = wait duration

  perform handle _ s cmd = case cmd of
    (Open wf (wa, wb) cid balA balB timelock) -> do
      Trace.callEndpoint @"open"
        (handle $ Participant wf)
        ( OpenParams
            cid
            (mockWalletPaymentPubKey wa)
            (mockWalletPaymentPubKey wb)
            (mockWalletPaymentPubKeyHash wa)
            (mockWalletPaymentPubKeyHash wb)
            balA
            balB
            timelock
        )
      delay 1
    Update {} -> do
      -- Update is an offchain action, so we do not perform any computation
      -- onchain wise.
      return ()
    (Dispute issuer (wa, wb) _ state) -> do
      (ska, skb) <- (,) <$> Trace.agentState wa <*> Trace.agentState wb <&> both (unPaymentPrivateKey . Trace.ownPaymentPrivateKey)
      (pka, pkb) <- (,) <$> Trace.agentState wa <*> Trace.agentState wb <&> both Trace.ownPaymentPublicKey
      Trace.callEndpoint @"dispute"
        (handle $ Participant issuer)
        ( DisputeParams
            pka
            pkb
            (SignedState (signMessage' state ska) (signMessage' state skb))
        )
      delay 1
    (Close issuer (wa, wb) _ _ _ _) -> do
      let cs = s ^. contractState
      chan <- case cs of
        PerunModel Nothing -> Trace.throwError . Trace.GenericError $ "no channel to close"
        PerunModel (Just (chan, _)) -> return chan
      (ska, skb) <- (,) <$> Trace.agentState wa <*> Trace.agentState wb <&> both (unPaymentPrivateKey . Trace.ownPaymentPrivateKey)
      (pka, pkb) <- (,) <$> Trace.agentState wa <*> Trace.agentState wb <&> both Trace.ownPaymentPublicKey
      Trace.callEndpoint @"close"
        (handle $ Participant issuer)
        ( CloseParams
            pka
            pkb
            (SignedState (signMessage' chan ska) (signMessage' chan skb))
        )
      delay 1
    (ForceClose issuer (_, _) cID) -> do
      Trace.callEndpoint @"forceClose"
        (handle $ Participant issuer)
        (ForceCloseParams cID)
      delay 1
    Finalize -> return ()
    Wait duration -> delay duration

