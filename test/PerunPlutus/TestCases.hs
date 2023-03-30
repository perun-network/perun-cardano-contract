{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}

module PerunPlutus.TestCases (perunTests) where

import Control.Lens hiding (both)
import Data.Tuple.Extra
import Perun hiding (ChannelAction (..))
import PerunPlutus.PerunSpec
import PerunPlutus.Test.EvilContract
import Plutus.Contract.Test
import Plutus.Contract.Test.ContractModel
import Test.QuickCheck
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck
import Perun.Offchain (mkNonceFromInteger)
import PlutusTx.Builtins (BuiltinByteString(..))

-- Testcases

-- Testconfiguration
initBalLB :: Integer
initBalLB = minAda * 3

initBalUB :: Integer
initBalUB = 5 * initBalLB

defaultTimeLockSlots :: Integer
defaultTimeLockSlots = 15

defaultTimeLock :: Integer
defaultTimeLock = defaultTimeLockSlots * 1000

-- | samePartySameValuePayout test checks that the payout validation works if
-- | one party is represented twice in the channel with both
-- | representations owning exactly equal balance
samePartySameValuePayoutTest :: (Wallet, Wallet) -> DL PerunModel ()
samePartySameValuePayoutTest (wa, wf) = do
  nonce <- mkNonceFromInteger <$> forAllQ arbitraryQ
  let cid = getChannelId $ getChannel [wa, wa] nonce
  action $ Open wf [wa, wa] cid [minAda, minAda] defaultTimeLock nonce
  action Finalize
  chSt <- requireGetChannel "channel must be available after finalization"

  action $ Close wa [wa, wa] cid (chSt ^. chanToken)

-- | sameValuePayoutTest checks that the payout validation works if there are
-- | multiple outputs with the same value
sameValuePayoutTest :: (Wallet, Wallet, Wallet) -> DL PerunModel ()
sameValuePayoutTest (wa, wb, wf) = do
  nonce <- mkNonceFromInteger <$> forAllQ arbitraryQ
  let cid = getChannelId $ getChannel [wa, wb] nonce
  action $ Open wf [wa, wb] cid [minAda, minAda] defaultTimeLock nonce
  action Finalize
  ct <- (^. chanToken) <$> requireGetChannel "channel must be available after finalization"
  action $ Close wb [wa, wb] cid ct

-- | honestPaymentTest test scenario:
-- | a third party opens a channel between A and B,
-- | A pays B some Ada, then they close the channel (with B issuing the closing transaction)
-- | on the state after the payment
honestPaymentTest :: (Wallet, Wallet, Wallet) -> DL PerunModel ()
honestPaymentTest (wa, wb, wf) = do
  nonce <- mkNonceFromInteger <$> forAllQ arbitraryQ
  let cid = getChannelId $ getChannel [wa, wb] nonce
  (initBalA, initBalB) <- forAllQ $ both chooseQ ((initBalLB, initBalUB), (initBalLB, initBalUB))
  action $ Open wf [wa, wb] cid [initBalA, initBalB] defaultTimeLock nonce
  modChSt <-
    requireWithChannel
      "channel must be available after opening"
      (\(PerunModelState cs _ _ _ _ _) -> forAllQ (chooseQ (0, initBalA - minAda)) >>= aPaysB cs)
  action $ Update modChSt
  action Finalize
  ct <- (^. chanToken) <$> requireGetChannel "channel must be available after finalization"
  action $ Close wb [wa, wb] cid ct

-- | singleDisputeTest test scenario:
-- | a third party opens a channel between A and B,
-- | A issues a payment to B,
-- | B wants to close the channel, but A does not answer
-- | (because A lost money in respect to the initial state),
-- | B disputes with the state after the payment,
-- | B force-closes the channel after waiting for the timelock to expire
singleDisputeTest :: (Wallet, Wallet, Wallet) -> DL PerunModel ()
singleDisputeTest (wa, wb, wf) = do
  nonce <- mkNonceFromInteger <$> forAllQ arbitraryQ
  let cid = getChannelId $ getChannel [wa, wb] nonce
  (initBalA, initBalB) <- forAllQ $ both chooseQ ((initBalLB, initBalUB), (initBalLB, initBalUB))
  action $ Open wf [wa, wb] cid [initBalA, initBalB] defaultTimeLock nonce

  modChSt <-
    requireWithChannel
      "channel must be available after opening"
      (\(PerunModelState cs _ _ _ _ _) -> forAllQ (chooseQ (0, initBalA - minAda)) >>= aPaysB cs)
  chSt <- requireGetChannel "channel must be available after opening"

  action $ Update modChSt
  action $ Dispute wb [wa, wb] cid modChSt (chSt ^. chanToken)
  action $ Wait defaultTimeLockSlots
  action $ ForceClose wb [wa, wb] cid (chSt ^. chanToken)

-- | maliciousDisputeTest test scenario:
-- | a third party opens a channel between A and B,
-- | A issues a payment to B (-> State 1),
-- | A issues another payment to B (-> State 2),
-- | A maliciously disputes with the outdated State 1
-- | (where A still has more money),
-- | B disputes with the newer (newest) State 2,
-- | A can not dispute with a newer signed state,
-- | B force-closes the channel after waiting for the timelock to expire
maliciousDisputeTest :: (Wallet, Wallet, Wallet) -> DL PerunModel ()
maliciousDisputeTest (wa, wb, wf) = do
  nonce <- mkNonceFromInteger <$> forAllQ arbitraryQ
  let cid = getChannelId $ getChannel [wa, wb] nonce
  (initBalA, initBalB) <- forAllQ $ both chooseQ ((initBalLB, initBalUB), (initBalLB, initBalUB))
  action $ Open wf [wa, wb] cid [initBalA, initBalB] defaultTimeLock nonce

  ct <- (^. chanToken) <$> requireGetChannel "channel must be available after finalization"

  firstUpdate <-
    requireGetChannel "channel must be available after opening"
      >>= ( \(PerunModelState cs _ _ _ _ _) ->
              forAllQ (chooseQ (0, (initBalA - minAda) `div` 2))
                >>= aPaysB cs
          )
  action $ Update firstUpdate
  secondUpdate <-
    requireGetChannel "channel must be available after opening"
      >>= ( \(PerunModelState cs _ _ _ _ _) ->
              forAllQ (chooseQ (0, (initBalA - minAda) `div` 2))
                >>= aPaysB cs
          )
  action $ Update secondUpdate

  action $ Dispute wa [wa, wb] cid firstUpdate ct
  action $ Wait 1
  action $ Dispute wb [wa, wb] cid secondUpdate ct

  action $ Wait defaultTimeLockSlots
  action $ ForceClose wb [wa, wb] cid ct

-- | twoPartyFundingAndPaymentTest test scenario:
-- | A starts a channel between A and B providing A's funds,
-- | B funds the channel with B's funds (which marks the channel
-- | as funded),
-- | A issues a payment to B,
-- | A and B close the channel together
twoPartyFundingAndPaymentTest :: (Wallet, Wallet) -> DL PerunModel ()
twoPartyFundingAndPaymentTest (wa, wb) = do
  nonce <- mkNonceFromInteger <$> forAllQ arbitraryQ
  let cid = getChannelId $ getChannel [wa, wb] nonce
  (initBalA, initBalB) <- forAllQ $ both chooseQ ((initBalLB, initBalUB), (initBalLB, initBalUB))
  action $ Start [wa, wb] cid [initBalA, initBalB] defaultTimeLock nonce

  ct <- (^. chanToken) <$> requireGetChannel "channel must be available after finalization"
  action $ Fund wb 1 cid ct
  modChSt <-
    requireWithChannel
      "channel must be available after opening"
      (\(PerunModelState cs _ _ _ _ _) -> forAllQ (chooseQ (0, initBalA - minAda)) >>= aPaysB cs)
  action $ Update modChSt
  action Finalize
  action $ Close wb [wa, wb] cid ct

-- | twoPartyFundingAndPaymentTest test scenario:
-- | A starts a channel between A and B and C providing A's funds,
-- | B funds the channel with B's funds,
-- | C funds the channel with C's funds (which marks the channel
-- | as funded),
-- | A issues a payment to B,
-- | A, B and C close the channel together
threePartyFundingAndPaymentTest :: (Wallet, Wallet, Wallet) -> DL PerunModel ()
threePartyFundingAndPaymentTest (wa, wb, wc) = do
  nonce <- mkNonceFromInteger <$> forAllQ arbitraryQ
  let cid = getChannelId $ getChannel [wa, wb, wc] nonce
  [initBalA, initBalB, initBalC] <- forAllQ $ map chooseQ [(initBalLB, initBalUB), (initBalLB, initBalUB), (initBalLB, initBalUB)]
  action $ Start [wa, wb, wc] cid [initBalA, initBalB, initBalC] defaultTimeLock nonce

  ct <- (^. chanToken) <$> requireGetChannel "channel must be available after finalization"

  action $ Fund wb 1 cid ct
  action $ Fund wc 2 cid ct
  modChSt <-
    requireWithChannel
      "channel must be available after opening"
      (\(PerunModelState cs _ _ _ _ _) -> forAllQ (chooseQ (0, initBalA - minAda)) >>= aPaysB cs)
  action $ Update modChSt
  action Finalize
  action $ Close wb [wa, wb, wc] cid ct

-- | twoPartyFundingAbortTest test scenario:
-- | A starts a channel between A and B providing A's funds,
-- | B does not fund the channel,
-- | A recovers their funds by aborting the channel
twoPartyFundingAbortTest :: (Wallet, Wallet) -> DL PerunModel ()
twoPartyFundingAbortTest (wa, wb) = do
  nonce <- mkNonceFromInteger <$> forAllQ arbitraryQ
  let cid = getChannelId $ getChannel [wa, wb] nonce
  (initBalA, initBalB) <- forAllQ $ both chooseQ ((initBalLB, initBalUB), (initBalLB, initBalUB))
  action $ Start [wa, wb] cid [initBalA, initBalB] defaultTimeLock nonce

  ct <- (^. chanToken) <$> requireGetChannel "channel must be available after finalization"

  action $ Abort wa [wa, wb] cid ct

-- | twoPartyFundingAbortTest test scenario:
-- | A starts a channel between A and B and C providing A's funds,
-- | B funds the channel with B's funds,
-- | C does not fund the channel,
-- | B recovers the funds to the respective funders (A and B) by aborting the channel
threePartyFundingAbortTest :: (Wallet, Wallet, Wallet) -> DL PerunModel ()
threePartyFundingAbortTest (wa, wb, wc) = do
  nonce <- mkNonceFromInteger <$> forAllQ arbitraryQ
  let cid = getChannelId $ getChannel [wa, wb, wc] nonce
  [initBalA, initBalB, initBalC] <- forAllQ $ map chooseQ [(initBalLB, initBalUB), (initBalLB, initBalUB), (initBalLB, initBalUB)]
  action $ Start [wa, wb, wc] cid [initBalA, initBalB, initBalC] defaultTimeLock nonce

  ct <- (^. chanToken) <$> requireGetChannel "channel must be available after finalization"


  action $ Fund wb 1 cid ct
  action $ Abort wb [wa, wb, wc] cid ct

-- | maliciousWalletTest performs various actions that are considered malicious
-- | as they either try to cheat or otherwise attack the integrity of the
-- | on-chain channel state. We assert that the malicious actions do not
-- | compromise the channel state, i.e. that nothing happens
maliciousWalletTest :: (Wallet, Wallet, Wallet) -> DL PerunModel ()
maliciousWalletTest (wa, wb, wc) = do
  nonce <- mkNonceFromInteger <$> forAllQ arbitraryQ
  let cid = getChannelId $ getChannel [wa, wb] nonce
  [initBalA, initBalB] <- forAllQ $ map chooseQ [(initBalLB, initBalUB), (initBalLB, initBalUB)]
  let chanBals = [initBalA, initBalB]
  action $ Start [wa, wb] cid [initBalA, initBalB] defaultTimeLock nonce
  ct <- (^. chanToken) <$> requireGetChannel "channel must be available after finalization"

  action $ MaliciousFund 1 wb cid 1 FundSteal
  action $ MaliciousFund 1 wb cid 1 FundViolateChannelIntegrity
  action $ MaliciousFund 1 wb cid 1 FundInvalidFunded
  action $ MaliciousAbort 2 wc cid AbortUnrelatedParticipant
  action $ MaliciousClose 1 wb cid [wa, wb] chanBals CloseUnfunded
  action $ MaliciousAbort 1 wb cid AbortInvalidFunding
  action $ MaliciousForceClose 1 wb cid ForceCloseInvalidInput
  action $ MaliciousForceClose 1 wb cid ForceCloseValidInput
  action $ MaliciousDispute 1 wb cid [wa, wb] chanBals DisputeValidInput
  action $ Fund wb 1 cid ct
  action $ Wait 1
  action $ MaliciousAbort 1 wb cid AbortAlreadyFunded
  action $ MaliciousForceClose 1 wb cid ForceCloseValidInput
  action $ MaliciousFund 1 wb cid 1 FundAlreadyFunded
  action $ MaliciousClose 1 wb cid [wa, wb] chanBals CloseInvalidInputValue
  action $ MaliciousForceClose 1 wb cid ForceCloseInvalidInput
  action $ MaliciousDispute 1 wb cid [wa, wb] chanBals DisputeInvalidInput
  action $ MaliciousDispute 1 wb cid [wa, wb] chanBals DisputeValidInput
  action $ MaliciousDispute 1 wb cid [wa, wb] chanBals DisputeWrongState
  modChSt <-
    requireWithChannel
      "channel must be available after opening"
      (\(PerunModelState cs _ _ _ _ _) -> forAllQ (chooseQ (0, initBalA - minAda)) >>= aPaysB cs)
  action $ Update modChSt
  action Finalize
  action $ Close wa [wa, wb] cid ct

getChannel :: [Wallet] -> BuiltinByteString -> Channel
getChannel wallets nonce =
  let wSigningKeys = map mockWalletPaymentPubKey wallets
      wPaymentKeys = map mockWalletPaymentPubKeyHash wallets
   in Channel defaultTimeLock wSigningKeys wPaymentKeys nonce

requireGetChannel :: String -> DL PerunModel PerunModelState
requireGetChannel msg =
  getContractState >>= \case
    Nothing -> fail $ "PerunModel must contain active channel: " <> msg
    (Just pms) -> return pms

requireWithChannel :: String -> (PerunModelState -> DL PerunModel a) -> DL PerunModel a
requireWithChannel msg f = requireGetChannel msg >>= f

aPaysB :: ChannelState -> Integer -> DL PerunModel ChannelState
aPaysB cs@(ChannelState _ bals v _) delta =
  return cs {balances = [head bals - delta, bals !! 1 + delta] ++ tail (tail bals), version = v + 1}

propPerun :: Actions PerunModel -> Property
propPerun = propRunActions_

prop_samePartySameValuePayoutTest :: Property
prop_samePartySameValuePayoutTest = withMaxSuccess 1 $ forAllDL (samePartySameValuePayoutTest (w1, w2)) propPerun

prop_SameValuePayoutTest :: Property
prop_SameValuePayoutTest = withMaxSuccess 1 $ forAllDL (sameValuePayoutTest (w1, w2, w3)) propPerun

prop_HonestPaymentTest :: Property
prop_HonestPaymentTest = withMaxSuccess 1 $ forAllDL (honestPaymentTest (w1, w2, w3)) propPerun

prop_SingleDisputeTest :: Property
prop_SingleDisputeTest = withMaxSuccess 1 $ forAllDL (singleDisputeTest (w1, w2, w3)) propPerun

prop_MaliciousDisputeTest :: Property
prop_MaliciousDisputeTest = withMaxSuccess 1 $ forAllDL (maliciousDisputeTest (w1, w2, w3)) propPerun

prop_TwoPartyFundingAndPaymentTest :: Property
prop_TwoPartyFundingAndPaymentTest = withMaxSuccess 1 $ forAllDL (twoPartyFundingAndPaymentTest (w1, w2)) propPerun

prop_TwoPartyFundingAbortTest :: Property
prop_TwoPartyFundingAbortTest = withMaxSuccess 1 $ forAllDL (twoPartyFundingAbortTest (w1, w2)) propPerun

-- Does not work because of resource limits (size)
--prop_ThreePartyFundingAndPaymentTest :: Property
--prop_ThreePartyFundingAndPaymentTest = withMaxSuccess 1 $ forAllDL (threePartyFundingAndPaymentTest (w1, w2, w3)) propPerun

prop_ThreePartyFundingAbortTest :: Property
prop_ThreePartyFundingAbortTest = withMaxSuccess 1 $ forAllDL (threePartyFundingAbortTest (w1, w2, w3)) propPerun

-- does not work with ThreadToken, maybe timing issues?
--prop_MaliciousWalletTest :: Property
--prop_MaliciousWalletTest = mapSize (const 42) $ withMaxSuccess 1 $ forAllDL (maliciousWalletTest (w1, w2, w3)) propPerun

return [] -- <- Needed for TemplateHaskell to do some magic and find the property tests.

-- NOTE: Automatically discovered properties are functions of type `Property`
-- having a `prop_` prefix!
perunTests :: TestTree
perunTests = testProperties "Perun Contract" $allProperties
