{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module PerunPlutus.PerunDummySpec where

import Control.Lens ((^.))
import Data.Data
import Data.Text (Text)
import qualified Ledger
import qualified Ledger.Ada as Ada
import qualified Ledger.Value as Value
import PerunDummy
import Plutus.Contract
import Plutus.Contract.Test (Wallet, minLogLevel, mockWalletPaymentPubKeyHash, w1, w2, w3)
import qualified Plutus.Contract.Test.ContractModel as CM
import qualified Plutus.Trace.Emulator as Trace
import qualified PlutusTx.Prelude as PP
import Test.Hspec
  ( Spec,
    describe,
    it,
  )
import Test.Hspec.Expectations.Pretty
import Test.QuickCheck as QC hiding (checkCoverage, (.&&.))

spec :: Spec
spec =
  describe "PerunPlutus" $
    it "does nothing yet" $ 1 `shouldBe` 1

wallets :: [Wallet]
wallets = [w1, w2]

exVal :: Value.Value
exVal = Ada.lovelaceValueOf 1

data PerunModel
  = PerunModel
      -- Channel participants.
      [Wallet]
      -- Externally tracked channelstate.
      ChannelState
  deriving (Show, Eq, Data)

deriving instance Eq (CM.ContractInstanceKey PerunModel w schema err params)

deriving instance Ord (CM.ContractInstanceKey PerunModel w schema err params)

deriving instance Show (CM.ContractInstanceKey PerunModel w schema err params)

instance CM.ContractModel PerunModel where
  -- Commands available to a testcase.
  data Action PerunModel
    = -- On-chain actions.
      Fund Wallet Integer
    | Open Wallet OpenParams
    | Close Wallet CloseParams
    | ForceClose Wallet ForceCloseParams
    | Dispute Wallet DisputeParams
    | Withdraw Wallet Integer
    | -- Off-chain actions.
      Update ChannelState
    deriving stock (Show, Eq)
    deriving (Data)

  data ContractInstanceKey PerunModel w schema err param where
    -- Only one type of contract under test, so we define the
    -- `ContractInstanceKey` with a single constructor distinguished by the
    -- wallet they are running in.
    Participant :: Wallet -> CM.ContractInstanceKey PerunModel () ChannelSchema Text ()

  initialInstances = (`CM.StartContract` ()) . Participant <$> wallets

  instanceContract _ Participant {} _ = contract

  instanceWallet (Participant w) = w

  arbitraryAction curState =
    let cs = curState ^. CM.contractState
     in oneof
          [ Open <$> genWallet <*> genOpen cs,
            Close <$> genWallet <*> genClose cs,
            ForceClose <$> genWallet <*> genForceClose cs,
            Dispute <$> genWallet <*> genDispute cs,
            Withdraw <$> genWallet <*> genValue,
            Update <$> genUpdate cs
          ]

  initialState = PerunModel wallets emptyChannelState

  nextState (Open w op) = undefined
  nextState (Update cs) = undefined
  nextState (Dispute w dp) = undefined
  nextState (Close w cp) = undefined
  nextState (ForceClose w fcp) = undefined
  nextState (Withdraw w a) = undefined

  precondition s (Open w op) = undefined
  precondition s (Update cs) = undefined
  precondition s (Dispute w dp) = undefined
  precondition s (Close w cp) = undefined
  precondition s (ForceClose w fcp) = undefined
  precondition s (Withdraw w a) = undefined

  perform handle _ s cmd = case cmd of
    (Open w op) -> do
      Trace.callEndpoint @"open" (handle $ Participant w) op
    (Update cs) -> undefined
    (Dispute w dp) -> do
      Trace.callEndpoint @"dispute" (handle $ Participant w) dp
    (Close w cp) ->
      Trace.callEndpoint @"close" (handle $ Participant w) cp
    (ForceClose w fcp) ->
      Trace.callEndpoint @"forceClose" (handle $ Participant w) fcp
    (Withdraw w a) -> undefined

  shrinkAction s Open {} = undefined
  shrinkAction s Close {} = undefined
  shrinkAction s ForceClose {} = undefined
  shrinkAction s Dispute {} = undefined
  shrinkAction s Update {} = undefined
  shrinkAction s Withdraw {} = undefined

emptyChannelState :: ChannelState
emptyChannelState = ChannelState 0 0 0 0

genWallet :: Gen Wallet
genWallet = elements wallets

genValue :: Gen Integer
genValue = choose (Ada.getLovelace Ledger.minAdaTxOut, 100_000_000)

genOpen :: PerunModel -> Gen OpenParams
genOpen _m = undefined

genClose :: PerunModel -> Gen CloseParams
genClose _m = undefined

genForceClose :: PerunModel -> Gen ForceCloseParams
genForceClose _m = undefined

genDispute :: PerunModel -> Gen DisputeParams
genDispute _m = undefined

genUpdate :: PerunModel -> Gen ChannelState
genUpdate _m = undefined
