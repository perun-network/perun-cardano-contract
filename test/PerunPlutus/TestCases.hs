{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}

module PerunPlutus.TestCases (runPerunDummyTests) where

import Data.Functor ((<&>))
import PerunDummy
import PerunPlutus.PerunDummySpec
import Plutus.Contract.Test
import Plutus.Contract.Test.ContractModel
import Test.QuickCheck
import Data.Tuple.Extra

-- Testcases

-- Testconfiguration
initBalLB :: Integer
initBalLB = 5_000_000

initBalUB :: Integer
initBalUB = 5 * initBalLB

defaultTimeLockSlots :: Integer
defaultTimeLockSlots = 15

defaultTimeLock :: Integer
defaultTimeLock = 15 * 1000



-- | honestPaymentTest test scenario:
-- | a third party opens a channel between A and B,
-- | A pays B some Ada, then they close the channel (with B issuing the closing transaction) 
-- | on the state after the payment
honestPaymentTest :: (Wallet, Wallet, Wallet) -> DL PerunModel ()
honestPaymentTest (wa, wb, wf) = do
  channelID <- forAllQ arbitraryQ
  (initBalA, initBalB) <- forAllQ $ both chooseQ ((initBalLB, initBalUB), (initBalLB, initBalUB))
  action $ Open wf [wa, wb] channelID [initBalA, initBalB] defaultTimeLock
  modChSt <-
    requireWithChannel
      "channel must be available after opening"
      (\(cs, _, _, _) -> forAllQ (chooseQ (0, initBalLB)) >>= aPaysB cs)
  action $ Update modChSt
  action Finalize
  (ChannelState cid _ _ _, _, _, _)  <- requireGetChannel "channel must be available after finalization"
  action $ Close wb [wa, wb] cid


-- | singleDisputeTest test scenario:
-- | a third party opens a channel between A and B,
-- | A issues a payment to B,
-- | B wants to close the channel, but A does not answer 
-- | (because A lost money in respect to the initial state),
-- | B disputes with the state after the payment,
-- | B force-closes the channel after waiting for the timelock to expire
singleDisputeTest :: (Wallet, Wallet, Wallet) -> DL PerunModel ()
singleDisputeTest (wa, wb, wf) = do
  channelID <- forAllQ arbitraryQ
  (initBalA, initBalB) <- forAllQ $ both chooseQ ((initBalLB, initBalUB), (initBalLB, initBalUB))
  action $ Open wf [wa, wb] channelID [initBalA, initBalB] defaultTimeLock

  modChSt <-
    requireWithChannel
      "channel must be available after opening"
      (\(cs, _, _, _) -> forAllQ (chooseQ (0, initBalLB)) >>= aPaysB cs)
  action $ Update modChSt
  action $ Dispute wb [wa, wb] channelID modChSt
  action $ Wait defaultTimeLockSlots
  action $ ForceClose wb [wa, wb] channelID


-- | maliciousDisputeTest test scenario:
-- | a third party opens a channel between A and B,
-- | A issues a payment to B (-> State 1),
-- | A issues another payment to B (-> State 2),
-- | A maliciously disputes with the outdated State 1
-- | (where A still has more money),
-- | A disputes with the newer (newest) State 2,
-- | A can not dispute with a newer signed state,
-- | B force-closes the channel after waiting for the timelock to expire 
maliciousDisputeTest :: (Wallet, Wallet, Wallet) -> DL PerunModel ()
maliciousDisputeTest (wa, wb, wf) = do
  channelID <- forAllQ arbitraryQ
  (initBalA, initBalB) <- forAllQ $ both chooseQ ((initBalLB, initBalUB), (initBalLB, initBalUB))
  action $ Open wf [wa, wb] channelID [initBalA, initBalB] defaultTimeLock
  firstUpdate <-
    requireGetChannel "channel must be available after opening"
      >>= ( \(cs, _, _, _) ->
              forAllQ (chooseQ (0, initBalLB `div` 2))
                >>= aPaysB cs
          )
  action $ Update firstUpdate
  secondUpdate <-
    requireGetChannel "channel must be available after opening"
      >>= ( \(cs, _, _, _) ->
              forAllQ (chooseQ (0, initBalLB `div` 2))
                >>= aPaysB cs
          )
  action $ Update secondUpdate

  action $ Dispute wa [wa, wb] channelID firstUpdate
  action $ Dispute wb [wa, wb] channelID secondUpdate

  action $ Wait defaultTimeLockSlots
  action $ ForceClose wb [wa, wb] channelID


-- | twoPartyFundingAndPaymentTest test scenario:
-- | A starts a channel between A and B providing A's funds,
-- | B funds the channel with B's funds (which marks the channel
-- | as funded),
-- | A issues a payment to B,
-- | A and B close the channel together
twoPartyFundingAndPaymentTest :: (Wallet, Wallet) -> DL PerunModel ()
twoPartyFundingAndPaymentTest (wa, wb) = do
  channelID <- forAllQ arbitraryQ
  (initBalA, initBalB) <- forAllQ $ both chooseQ ((initBalLB, initBalUB), (initBalLB, initBalUB))
  action $ Start [wa, wb] channelID [initBalA, initBalB] defaultTimeLock
  action $ Fund wb 1 channelID
  modChSt <-
    requireWithChannel
      "channel must be available after opening"
      (\(cs, _, _, _) -> forAllQ (chooseQ (0, initBalLB)) >>= aPaysB cs)
  action $ Update modChSt
  action Finalize
  (ChannelState cid _ _ _, _, _, _) <- requireGetChannel "channel must be available after finalization"
  action $ Close wb [wa, wb] cid

-- | twoPartyFundingAndPaymentTest test scenario:
-- | A starts a channel between A and B and C providing A's funds,
-- | B funds the channel with B's funds,
-- | C funds the channel with C's funds (which marks the channel
-- | as funded),
-- | A issues a payment to B,
-- | A, B and C close the channel together
threePartyFundingAndPaymentTest :: (Wallet, Wallet, Wallet) -> DL PerunModel ()
threePartyFundingAndPaymentTest (wa, wb, wc) = do
  channelID <- forAllQ arbitraryQ
  [initBalA, initBalB, initBalC] <- forAllQ $ map chooseQ [(initBalLB, initBalUB), (initBalLB, initBalUB), (initBalLB, initBalUB)]
  action $ Start [wa, wb, wc] channelID [initBalA, initBalB, initBalC] defaultTimeLock
  action $ Fund wb 1 channelID
  action $ Fund wc 2 channelID
  modChSt <-
    requireWithChannel
      "channel must be available after opening"
      (\(cs, _, _, _) -> forAllQ (chooseQ (0, initBalLB)) >>= aPaysB cs)
  action $ Update modChSt
  action Finalize
  (ChannelState cid _ _ _, _, _, _) <- requireGetChannel "channel must be available after finalization"
  action $ Close wb [wa, wb, wc] cid

-- | twoPartyFundingAbortTest test scenario: 
-- | A starts a channel between A and B providing A's funds,
-- | B does not fund the channel,
-- | A recovers their funds by aborting the channel
twoPartyFundingAbortTest :: (Wallet, Wallet) -> DL PerunModel ()
twoPartyFundingAbortTest (wa, wb) = do
  channelID <- forAllQ arbitraryQ
  (initBalA, initBalB) <- forAllQ $ both chooseQ ((initBalLB, initBalUB), (initBalLB, initBalUB))
  action $ Start [wa, wb] channelID [initBalA, initBalB] defaultTimeLock
  action $ Abort wa [wa, wb] channelID

-- | twoPartyFundingAbortTest test scenario: 
-- | A starts a channel between A and B and C providing A's funds,
-- | B funds the channel with B's funds,
-- | C does not fund the channel,
-- | B recovers the funds to the respective funders (A and B) by aborting the channel
threePartyFundingAbortTest :: (Wallet, Wallet, Wallet) -> DL PerunModel ()
threePartyFundingAbortTest (wa, wb, wc) = do
  channelID <- forAllQ arbitraryQ
  [initBalA, initBalB, initBalC] <- forAllQ $ map chooseQ [(initBalLB, initBalUB), (initBalLB, initBalUB), (initBalLB, initBalUB)]  
  action $ Start [wa, wb, wc] channelID [initBalA, initBalB, initBalC] defaultTimeLock
  action $ Fund wb 1 channelID
  action $ Abort wb [wa, wb, wc] channelID


requireGetChannel :: String -> DL PerunModel (ChannelState, Integer, [Integer], Bool)
requireGetChannel msg =
  getContractState >>= \case
    PerunModel Nothing -> fail $ "PerunModel must contain active channel: " <> msg
    PerunModel (Just c) -> return c

requireWithChannel :: String -> ((ChannelState, Integer, [Integer], Bool) -> DL PerunModel a) -> DL PerunModel a
requireWithChannel msg f = requireGetChannel msg >>= f

-- | genChainedChannelUpdates generates `n` successive channel updates on the
-- | channel contained in `PerunModel`.
-- | It generates valid updates as long as `n <= initBalLB`.
-- | NOTE: Something is weird when executing actions using `mapM/mapM_`, which
-- | is necessary to make proper use of this function.
genChainedChannelUpdates :: Integer -> DL PerunModel [ChannelState]
genChainedChannelUpdates n =
  mapM
    ( const $
        requireGetChannel "channel must be available after opening"
          >>= ( \(cs, _, _, _) ->
                  forAllQ (chooseQ (0, initBalLB `div` n))
                    >>= aPaysB cs
              )
    )
    $ take (fromIntegral n) [() ..]

aPaysB :: ChannelState -> Integer -> DL PerunModel ChannelState
aPaysB cs@(ChannelState _ balances v _) delta =
  return cs {balances = [head balances - delta, balances!!1 + delta] ++ tail (tail balances), version = v + 1}

propPerunDummy :: Actions PerunModel -> Property
propPerunDummy = propRunActions_

prop_HonestPaymentTest :: Property
prop_HonestPaymentTest = withMaxSuccess 1 $ forAllDL (honestPaymentTest (w1, w2, w3)) propPerunDummy

prop_SingleDisputeTest :: Property
prop_SingleDisputeTest = withMaxSuccess 1 $ forAllDL (singleDisputeTest (w1, w2, w3)) propPerunDummy

prop_MaliciousDisputeTest :: Property
prop_MaliciousDisputeTest = withMaxSuccess 1 $ forAllDL (maliciousDisputeTest (w1, w2, w3)) propPerunDummy

prop_TwoPartyFundingAndPaymentTest :: Property
prop_TwoPartyFundingAndPaymentTest = withMaxSuccess 1 $ forAllDL (twoPartyFundingAndPaymentTest (w1, w2)) propPerunDummy

prop_TwoPartyFundingAbortTest :: Property
prop_TwoPartyFundingAbortTest = withMaxSuccess 1 $ forAllDL (twoPartyFundingAndPaymentTest (w1, w2)) propPerunDummy

prop_ThreePartyFundingAndPaymentTest :: Property
prop_ThreePartyFundingAndPaymentTest = withMaxSuccess 1 $ forAllDL (threePartyFundingAndPaymentTest (w1, w2, w3)) propPerunDummy

prop_ThreePartyFundingAbortTest :: Property
prop_ThreePartyFundingAbortTest = withMaxSuccess 1 $ forAllDL (threePartyFundingAbortTest (w1, w2, w3)) propPerunDummy


return [] -- <- Needed for TemplateHaskell to do some magic and find the property tests.

-- NOTE: Automatically discovered properties are functions of type `Property`
-- having a `prop_` prefix!
runPerunDummyTests :: IO Bool
runPerunDummyTests = $quickCheckAll
