{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
module PerunPlutus.TestCases where
import Plutus.Contract.Test
import PerunPlutus.PerunDummySpec
import Plutus.Contract.Test.ContractModel
import PerunDummy
import Test.QuickCheck
import Data.Tuple.Extra (both)

-- Testcases

-- unitTest example of creating a channel, updating it once and closing.
honestPaymentTest :: (Wallet, Wallet, Wallet) -> DL PerunModel ()
honestPaymentTest (wa, wb, wf) = do
  (initBalA, initBalB) <- forAllQ $ both chooseQ ((5_000_000, 20_000_000), (5_000_000, 20_000_000))
  action $ Open wf (wa, wb) 42069 initBalA initBalB 15000

  modChSt <-
    getContractState >>= \case
      PerunModel Nothing -> fail "opening a channel should be tracked in PerunModel"
      PerunModel (Just (cs, _)) -> aPaysB cs 420
  action $ Update modChSt

  action Finalize

  getContractState >>= \case
    PerunModel Nothing -> fail "unclosed channel should persist in PerunModel"
    PerunModel (Just (ChannelState cid ba bb v _, _)) -> action $ Close wb (wa, wb) cid ba bb v

singleDisputeTest :: (Wallet, Wallet, Wallet) -> DL PerunModel ()
singleDisputeTest (wa, wb, wf) = do
  (initBalA, initBalB) <- forAllQ $ both chooseQ ((5_000_000, 20_000_000), (5_000_000, 20_000_000))
  action $ Open wf (wa, wb) 42069 initBalA initBalB 15000

  modChSt <-
    getContractState >>= \case
      PerunModel Nothing -> fail "opening a channel should be tracked in PerunModel"
      PerunModel (Just (cs, _)) -> aPaysB cs 420
  action $ Update modChSt

  getContractState >>= \case
    PerunModel Nothing -> fail "unclosed channel should persist in PerunModel"
    PerunModel (Just _) -> action $ Dispute wb (wa, wb) 42069 modChSt
  action $ Wait 16


  getContractState >>= \case
    PerunModel Nothing -> fail "unclosed channel should persist in PerunModel"
    PerunModel (Just _) -> action $ ForceClose wb (wa, wb) 42069

maliciousDisputeTest :: (Wallet, Wallet, Wallet) -> DL PerunModel ()
maliciousDisputeTest (wa, wb, wf) = do
  (initBalA, initBalB) <- forAllQ $ both chooseQ ((5_000_000, 20_000_000), (5_000_000, 20_000_000))
  action $ Open wf (wa, wb) 42069 initBalA initBalB 15000

  firstUpdate <-
    getContractState >>= \case
      PerunModel Nothing -> fail "opening a channel should be tracked in PerunModel"
      PerunModel (Just (cs, _)) -> aPaysB cs 420
  action $ Update firstUpdate

  secondUpdate <-
    getContractState >>= \case
      PerunModel Nothing -> fail "opening a channel should be tracked in PerunModel"
      PerunModel (Just (cs, _)) -> aPaysB cs 6969
  action $ Update secondUpdate

  getContractState >>= \case
    PerunModel Nothing -> fail "unclosed channel should persist in PerunModel"
    PerunModel (Just _) -> action $ Dispute wa (wa, wb) 42069 firstUpdate

  getContractState >>= \case
    PerunModel Nothing -> fail "unclosed channel should persist in PerunModel"
    PerunModel (Just _) -> action $ Dispute wb (wa, wb) 42069 secondUpdate
  action $ Wait 16


  getContractState >>= \case
    PerunModel Nothing -> fail "unclosed channel should persist in PerunModel"
    PerunModel (Just _) -> action $ ForceClose wb (wa, wb) 42069



aPaysB :: ChannelState -> Integer -> DL PerunModel ChannelState
aPaysB cs@(ChannelState _ bA bB v _) delta = do
  return cs {balanceA = bA - delta, balanceB = bB + delta, version = v + 1}

propPerunDummy :: Actions PerunModel -> Property
propPerunDummy = propRunActions_

-- TODO find a way to actually run *all* unit tests
propUnitTest :: Property
propUnitTest = withMaxSuccess 1 $ forAllDL (maliciousDisputeTest (w1, w2, w3)) propPerunDummy
