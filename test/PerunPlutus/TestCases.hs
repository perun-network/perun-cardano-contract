{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module PerunPlutus.TestCases where

import Data.Functor ((<&>))
import Data.Tuple.Extra (both)
import PerunDummy
import PerunPlutus.PerunDummySpec
import Plutus.Contract.Test
import Plutus.Contract.Test.ContractModel
import Test.QuickCheck

-- Testcases

-- Testconfiguration
initBalLB :: Integer
initBalLB = 5_000_000

initBalUB :: Integer
initBalUB = 5 * initBalLB

defaultTimeLock :: Integer
defaultTimeLock = 15_000

-- | honestPaymentTest tests the happy path where two parties open a channel,
-- | update it once and close if afterwards.
honestPaymentTest :: (Wallet, Wallet, Wallet) -> DL PerunModel ()
honestPaymentTest (wa, wb, wf) = do
  channelID <- forAllQ $ arbitraryQ @Integer
  (initBalA, initBalB) <- forAllQ $ both chooseQ ((initBalLB, initBalUB), (initBalLB, initBalUB))
  action $ Open wf (wa, wb) channelID initBalA initBalB defaultTimeLock
  modChSt <-
    requireWithChannel
      "channel must be available after opening"
      (\(cs, _) -> forAllQ (chooseQ (0, initBalLB)) >>= aPaysB cs)
  action $ Update modChSt
  action Finalize
  ChannelState cid ba bb v _ <- requireGetChannel "channel must be available after finalization" <&> fst
  action $ Close wb (wa, wb) cid ba bb v

singleDisputeTest :: (Wallet, Wallet, Wallet) -> DL PerunModel ()
singleDisputeTest (wa, wb, wf) = do
  channelID <- forAllQ $ arbitraryQ @Integer
  (initBalA, initBalB) <- forAllQ $ both chooseQ ((initBalLB, initBalUB), (initBalLB, initBalUB))
  action $ Open wf (wa, wb) channelID initBalA initBalB defaultTimeLock

  modChSt <-
    requireWithChannel
      "channel must be available after opening"
      (\(cs, _) -> forAllQ (chooseQ (0, initBalLB)) >>= aPaysB cs)
  action $ Update modChSt
  action $ Dispute wb (wa, wb) channelID modChSt
  action $ Wait 16
  action $ ForceClose wb (wa, wb) channelID

requireGetChannel :: String -> DL PerunModel (ChannelState, Integer)
requireGetChannel msg =
  getContractState >>= \case
    PerunModel Nothing -> fail $ "PerunModel must contain active channel: " <> msg
    PerunModel (Just c) -> return c

requireWithChannel :: String -> ((ChannelState, Integer) -> DL PerunModel a) -> DL PerunModel a
requireWithChannel msg f = requireGetChannel msg >>= f

maliciousDisputeTest :: (Wallet, Wallet, Wallet) -> DL PerunModel ()
maliciousDisputeTest (wa, wb, wf) = do
  channelID <- forAllQ $ arbitraryQ @Integer
  (initBalA, initBalB) <- forAllQ $ both chooseQ ((initBalLB, initBalUB), (initBalLB, initBalUB))
  action $ Open wf (wa, wb) channelID initBalA initBalB defaultTimeLock
  updates <- genChainedChannelUpdates 20
  action `mapM` map Update updates
  action `mapM` zipWith (\w update -> Dispute w (wa, wb) channelID update) [wa, wb] updates
  action $ Wait 16
  action $ ForceClose wb (wa, wb) channelID

-- | genChainedChannelUpdates generates `n` successive channel updates on the
-- | channel contained in `PerunModel`.
-- | It generates valid updates as long as `n <= initBalLB`.
genChainedChannelUpdates :: Integer -> DL PerunModel [ChannelState]
genChainedChannelUpdates n =
  mapM
    ( const $
        requireGetChannel "channel must be available after opening"
          >>= ( \(cs, _) ->
                  forAllQ (chooseQ (0, initBalLB `div` n))
                    >>= aPaysB cs
              )
    )
    $ take (fromIntegral n) [() ..]

aPaysB :: ChannelState -> Integer -> DL PerunModel ChannelState
aPaysB cs@(ChannelState _ bA bB v _) delta =
  return cs {balanceA = bA - delta, balanceB = bB + delta, version = v + 1}

bPaysA :: ChannelState -> Integer -> DL PerunModel ChannelState
bPaysA cs@(ChannelState _ bA bB v _) delta =
  return cs {balanceA = bA + delta, balanceB = bB - delta, version = v + 1}

propPerunDummy :: Actions PerunModel -> Property
propPerunDummy = propRunActions_

-- TODO find a way to actually run *all* unit tests
propUnitTest :: Property
propUnitTest = withMaxSuccess 1 $ forAllDL (maliciousDisputeTest (w1, w2, w3)) propPerunDummy
