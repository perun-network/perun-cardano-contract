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
import Test.QuickCheck as QC hiding (checkCoverage, (.&&.))

wallets :: [Wallet]
wallets = [w1, w2, w3]

exVal :: Value.Value
exVal = Ada.lovelaceValueOf 1

newtype PerunModel = PerunModel (Maybe ChannelState) deriving (Show, Eq, Data)

deriving instance Eq (ContractInstanceKey PerunModel w schema err params)

deriving instance Ord (ContractInstanceKey PerunModel w schema err params)

deriving instance Show (ContractInstanceKey PerunModel w schema err params)

instance ContractModel PerunModel where
  {--
   - On-chain actions.
   - Only supporting two party channels for now.
  --}
  data Action PerunModel
    = -- | Open Wallet (Wallet, Wallet) -> ChannelID -> BalanceA -> BalanceB.
      Open Wallet (Wallet, Wallet) Integer Integer Integer
    | -- | Close Wallet -> ChannelID -> BalanceA -> BalanceB -> Version.
      Close (Wallet, Wallet) Integer Integer Integer Integer
    | -- | ForceClosing Wallet -> ChannelID.
      ForceClose Wallet Integer
    | -- | Disputing -> ChannelID -> BalanceA -> BalanceB -> Version -> Final.
      Dispute (Wallet, Wallet) Integer Integer Integer Integer Bool
    | -- | Off-chain action of updating the channelstate.
      Update ChannelState
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
  nextState (Open wf (_, _) cid balA balB) = do
    modifyContractState $ \_ ->
      PerunModel
        ( Just $
            ChannelState
              { channelId = cid,
                balanceA = balA,
                balanceB = balB,
                version = 0,
                final = False
              }
        )
    -- Move funds from chan -> Contract instance.
    withdraw wf $ Ada.lovelaceValueOf (balA + balB)
    wait 1
  nextState (Update cs) = do
    modifyContractState $ \_ -> PerunModel . Just $ cs
  -- Disputing does nothing to the contract state, yet.
  nextState Dispute {} = return ()
  -- Closing does nothing to the contract state, yet.
  nextState (Close (wa, wb) _ _ _ _) = do
    s <-
      getContractState >>= \case
        PerunModel Nothing -> error "close only works on existing channels"
        PerunModel (Just s) -> return s
    deposit wa $ Ada.lovelaceValueOf (balanceA s)
    deposit wb $ Ada.lovelaceValueOf (balanceB s)
    wait 1
  -- ForceClosing does nothing to the contract state, yet.
  nextState ForceClose {} = return ()

  perform handle _ s cmd = case cmd of
    (Open wf (wa, wb) cid balA balB) -> do
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
            30_000_000
        )
      delay 1
    Update {} -> do
      -- Update is an offchain action, so we do not perform any computation
      -- onchain wise.
      return ()
    Dispute {} -> do
      return ()
    (Close (wa, wb) cid balA balB v) -> do
      let cs = s ^. contractState
      chan <- case cs of
        PerunModel Nothing -> Trace.throwError . Trace.GenericError $ "no channel to close"
        PerunModel (Just chan) -> return chan
      (ska, skb) <- (,) <$> Trace.agentState wa <*> Trace.agentState wb <&> both (unPaymentPrivateKey . Trace.ownPaymentPrivateKey)
      (pka, pkb) <- (,) <$> Trace.agentState wa <*> Trace.agentState wb <&> both (Trace.ownPaymentPublicKey)
      Trace.callEndpoint @"close"
        (handle $ Participant wa)
        ( CloseParams
            pka
            pkb
            (SignedState (signMessage' chan ska) (signMessage' chan skb))
        )
      delay 1
    ForceClose {} ->
      return ()

-- Testcases

-- unitTest example of creating a channel, updating it once and closing.
unitTest :: (Wallet, Wallet, Wallet) -> DL PerunModel ()
unitTest (wa, wb, wf) = do
  (initBalA, initBalB) <- forAllQ $ both chooseQ ((5_000_000, 20_000_000), (5_000_000, 20_000_000))
  action $ Open wf (wa, wb) 42069 initBalA initBalB

  modChSt <-
    getContractState >>= \case
      PerunModel Nothing -> fail "opening a channel should be tracked in PerunModel"
      PerunModel (Just cs) -> modifyChannelStateA cs 420
  action $ Update modChSt

  getContractState >>= \case
    PerunModel Nothing -> fail "unclosed channel should persist in PerunModel"
    PerunModel (Just (ChannelState cid ba bb v _)) -> action $ Close (wa, wb) cid ba bb v


modifyChannelStateA :: ChannelState -> Integer -> DL PerunModel ChannelState
modifyChannelStateA cs@(ChannelState _ bA bB v _) delta = do
  return cs {balanceA = bA - delta, balanceB = bB + delta, version = v + 1}

propPerunDummy :: Actions PerunModel -> Property
propPerunDummy = propRunActions_

propUnitTest :: Property
propUnitTest = withMaxSuccess 1 $ forAllDL (unitTest (w1, w2, w3)) propPerunDummy
