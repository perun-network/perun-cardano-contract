{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Wallet.Primitive.AddressDerivation (NetworkDiscriminant (..))
import qualified Cardano.Wallet.Primitive.Types as Types
import Cardano.Wallet.Shelley.Compatibility ()
import Cardano.Wallet.Shelley.Network.Discriminant
import Client
import Control.Lens
import Control.Monad.State
import Data.Default
import qualified Data.ByteString.Lazy as BSL
import Data.Either
import Data.Proxy
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Base16 as BS16
import Data.Text.Class (fromText)
import GHC.TypeLits hiding (Mod)
import Ledger (PaymentPubKeyHash (..))
import qualified Ledger.Crypto as Crypto
import Multi
import Options.Applicative hiding (Success)
import Options.Applicative.Types
import Perun (DisputeParams (..), ForceCloseParams (..), FundParams (..))
import Perun.Offchain (OpenParams (..), getChannelId, mkNonceFromInteger)
import Perun.Onchain (Channel (..), ChannelState (..), channelTokenAsset, ChannelID(..))
import Plutus.PAB.Types (Config (..), WebserverConfig (..), defaultWebServerConfig)
import Servant.Client.Core.BaseUrl (BaseUrl (..), Scheme (..))
import System.Random.Stateful
import Wallet.Emulator.Wallet (Wallet (..), WalletId (..))
import Prelude hiding (concat)
import qualified PlutusTx.Builtins as Builtins

data CmdLineArgs = CLA
  { myWallet :: !Wallet,
    peerPaymentPubKey :: !Wallet,
    testnetMagic :: !SomeNetworkDiscriminant,
    channelID :: !(Maybe ChannelID)
  }

cmdLineParser :: Parser CmdLineArgs
cmdLineParser =
  CLA
    <$> parseWallet
      ( long "walletid"
          <> short 'a'
          <> metavar "WALLET-ID"
          <> help
            "Alice's wallet identifier"
      )
    <*> parseWallet
      ( long "walletid-peer"
          <> short 'b'
          <> metavar "WALLET-ID"
          <> help
            "Bob's wallet identifier"
      )
    <*> parseSomeNetworkDiscriminant
      ( long "testnet-id"
          <> short 'i'
          <> metavar "ID"
          <> help
            "Specify the testnet id to use"
      )
    <*> parseChannelId
      ( long "channel-id"
          <> short 'c'
          <> metavar "CHANNEL-ID"
          <> help
            "Specify the channel id to use. Using this option is exclusive and only sets up a subscription to the given channel."
          <> value ""
      )
  where
    parseWallet :: Mod OptionFields String -> Parser Wallet
    parseWallet opts = Wallet Nothing . WalletId . right . fromText . pack <$> strOption opts
    right (Right a) = a
    right _ = error "parsing failed"
    parseChannelId :: Mod OptionFields String -> Parser (Maybe ChannelID)
    parseChannelId opts = parseChannelId' <$> strOption opts
      where parseChannelId' "" = Nothing
            parseChannelId' s = rightToMaybe $ (ChannelID . Builtins.toBuiltin) <$> (BS16.decode . encodeUtf8 . pack $ s)

            rightToMaybe :: Either a b -> Maybe b
            rightToMaybe (Right a) = Just a
            rightToMaybe _ = Nothing
    parseSomeNetworkDiscriminant :: Mod OptionFields Integer -> Parser SomeNetworkDiscriminant
    parseSomeNetworkDiscriminant opts = parseNetworkDiscriminant <$> intOption opts
      where
        parseNetworkDiscriminant :: Integer -> SomeNetworkDiscriminant
        parseNetworkDiscriminant i =
          case someNatVal i of
            Nothing -> error "invalid testnet-id given"
            Just (SomeNat p) -> mkSomeNetworkDiscriminant p

        mkSomeNetworkDiscriminant :: forall n. KnownNat n => Proxy n -> SomeNetworkDiscriminant
        mkSomeNetworkDiscriminant _ = SomeNetworkDiscriminant (Proxy @('Testnet n))

        intOption :: Mod OptionFields Integer -> Parser Integer
        intOption = option int

        int :: ReadM Integer
        int = read @Integer <$> readerAsk


defaultTimeLock :: Integer
defaultTimeLock = 90 * 1000

defaultPabConfig :: Config
defaultPabConfig = def

defaultBalance :: Integer
defaultBalance = 20_000_000

main :: IO ()
main = execParser opts >>= main'
  where
    opts =
      info
        (cmdLineParser <**> helper)
        (fullDesc <> progDesc "Perun Client Application")

walletId :: Wallet -> Types.WalletId
walletId (Wallet _ (WalletId wid)) = wid

main' :: CmdLineArgs -> IO ()
main' (CLA aliceWallet bobWallet network mChanId) = do
  let walletURL = BaseUrl Http "localhost" 8090 ""
      apiURL = Plutus.PAB.Types.baseUrl defaultWebServerConfig
  mstates <-
    mapM
      (runPerunClientInitializer . uncurry (withWallet network walletURL apiURL))
      [ (aliceWallet, (alicePhrase, alicePassphrase)),
        (bobWallet, (bobPhrase, bobPassphrase))
      ]
  let states =
        mstates >>= \case
          Left err -> error . show $ err
          Right r -> return r

  -- MultiClient description.
  void . runMultiClientWith @["alice", "bob"] states $ do
    case mChanId of
      Just chanId -> do
        subscribeAdjudicator @"alice" chanId
      Nothing -> fullTestTrace
  return ()

fullTestTrace :: MultiClient ["alice", "bob"] ()
fullTestTrace = do
    alicePKH <- actionBy @"alice" $ gets (PaymentPubKeyHash . (^. pubKeyHash))
    aliceSPK <- actionBy @"alice" $ gets (^. signingPubKey)
    bobPKH <- actionBy @"bob" $ gets (PaymentPubKeyHash . (^. pubKeyHash))
    bobSPK <- actionBy @"bob" $ gets (^. signingPubKey)
    -- Also subscribe to Websocket events for the contract instance of alice.
    async $ subscribeToContractEvents @"alice"
    nonce <- mkNonceFromInteger . abs <$> randomM globalStdGen
    -- Endpoint Parameters
    let payPubKeyHashes = [alicePKH, bobPKH]
        signingPKs = [aliceSPK, bobSPK]
        chanId = getChannelId (Channel defaultTimeLock signingPKs payPubKeyHashes nonce)
        startParams =
          OpenParams
            { spChannelId = chanId,
              spSigningPKs = signingPKs,
              spPaymentPKs = payPubKeyHashes,
              spBalances = [defaultBalance, defaultBalance],
              spTimeLock = defaultTimeLock,
              spNonce = nonce
            }
    -- Run the adjudicator for "alice".
    async $ subscribeAdjudicator @"alice" chanId
    async $ subscribeAdjudicator @"bob" chanId
    -- Trace definition.
    withChannelToken @"alice" startParams $ \ct -> do
      let ctAsset = channelTokenAsset ct
          fundParams = FundParams chanId ctAsset 1
          stateV1 = ChannelState chanId [defaultBalance `div` 2, (defaultBalance `div` 2) * 3] 1 False
          change = defaultBalance `div` 4
          stateV2 = ChannelState chanId [defaultBalance + change, defaultBalance - change] 2 False
          forceCloseParams = ForceCloseParams chanId ctAsset
      signedStateV1 <- update stateV1
      signedStateV2 <- update stateV2
      let disputeBobParams = DisputeParams chanId ctAsset signingPKs signedStateV1
          disputeAliceParams = DisputeParams chanId ctAsset signingPKs signedStateV2
      delayAll 15_000_000
      callEndpointFor @"bob" "fund" fundParams
      delayAll 30_000_000
      callEndpointFor @"bob" "dispute" disputeBobParams
      delayAll 30_000_000
      callEndpointFor @"alice" "dispute" disputeAliceParams
      delayAll . fromIntegral $ defaultTimeLock * 1000 + (10 * 1000 * 1000)
      callEndpointFor @"alice" "forceClose" forceCloseParams
      -- Wait additional 6 seconds in case the contract subscription still
      -- responds with some event.
      delayAll 6_000_000


alicePhrase :: [Text]
alicePhrase =
  [ "lottery",
    "ring",
    "detect",
    "drip",
    "black",
    "match",
    "spoon",
    "post",
    "bind",
    "suit",
    "gather",
    "someone",
    "notice",
    "hero",
    "scrap"
  ]

bobPhrase :: [Text]
bobPhrase =
  [ "drill",
    "piece",
    "rotate",
    "badge",
    "rapid",
    "foam",
    "ginger",
    "panda",
    "velvet",
    "version",
    "duck",
    "travel",
    "script",
    "police",
    "enemy"
  ]

alicePassphrase :: Crypto.Passphrase
alicePassphrase = Crypto.Passphrase "0123456789"

bobPassphrase :: Crypto.Passphrase
bobPassphrase = Crypto.Passphrase "0123456789"
