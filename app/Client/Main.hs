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
import Data.Either
import Data.Proxy
import Data.Text (Text, pack)
import Data.Text.Class (fromText)
import GHC.TypeLits hiding (Mod)
import Ledger (PaymentPubKeyHash (..))
import qualified Ledger.Crypto as Crypto
import Multi
import Options.Applicative hiding (Success)
import Options.Applicative.Types
import Perun (DisputeParams (..), ForceCloseParams (..), FundParams (..))
import Perun.Offchain (OpenParams (..), getChannelId)
import Perun.Onchain (Channel (..), ChannelState (..), channelTokenAsset)
import Plutus.PAB.Types (Config (..), WebserverConfig (..), defaultWebServerConfig)
import Servant.Client.Core.BaseUrl (BaseUrl (..), Scheme (..))
import System.Random.Stateful
import Wallet.Emulator.Wallet (Wallet (..), WalletId (..))
import Prelude hiding (concat)

data CmdLineArgs = CLA
  { myWallet :: !Wallet,
    peerPaymentPubKey :: !Wallet,
    testnetMagic :: !SomeNetworkDiscriminant
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
  where
    parseWallet :: Mod OptionFields String -> Parser Wallet
    parseWallet opts = Wallet Nothing . WalletId . right . fromText . pack <$> strOption opts
    right (Right a) = a
    right _ = error "parsing failed"
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
main' (CLA aliceWallet bobWallet network) = do
  let -- network = SomeNetworkDiscriminant (Proxy @('Testnet 42))
      walletURL = BaseUrl Http "localhost" 8090 ""
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
    alicePKH <- actionBy @"alice" $ gets (PaymentPubKeyHash . (^. pubKeyHash))
    aliceSPK <- actionBy @"alice" $ gets (^. signingPubKey)
    bobPKH <- actionBy @"bob" $ gets (PaymentPubKeyHash . (^. pubKeyHash))
    bobSPK <- actionBy @"bob" $ gets (^. signingPubKey)
    -- Also subscribe to Websocket events for the contract instance of alice.
    subscribeToContractEvents @"alice"
    nonce <- abs <$> randomM globalStdGen
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
  return ()

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
