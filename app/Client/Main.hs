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
import Ledger (PaymentPubKeyHash (..))
import qualified Ledger.Crypto as Crypto
import Multi
import Options.Applicative hiding (Success)
import Perun (DisputeParams (..), ForceCloseParams (..), FundParams (..))
import Perun.Offchain (OpenParams (..))
import Perun.Onchain (ChannelState (..))
import Plutus.PAB.Types (Config (..), WebserverConfig (..), defaultWebServerConfig)
import Servant.Client.Core.BaseUrl (BaseUrl (..), Scheme (..))
import System.Random.Stateful
import Wallet.Emulator.Wallet (Wallet (..), WalletId (..))
import Prelude hiding (concat)

data CmdLineArgs = CLA
  { myWallet :: !Wallet,
    peerPaymentPubKey :: !Wallet
  }

cmdLineParser :: Parser CmdLineArgs
cmdLineParser =
  CLA
    <$> parseWallet
      ( long "walletid"
          <> short 'a'
          <> help
            "Alice's wallet identifier"
      )
    <*> parseWallet
      ( long "walletid-peer"
          <> short 'b'
          <> help
            "Bob's wallet identifier"
      )
  where
    parseWallet :: Mod OptionFields String -> Parser Wallet
    parseWallet opts = Wallet Nothing . WalletId . right . fromText . pack <$> strOption opts
    right (Right a) = a
    right _ = error "parsing failed"

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
main' (CLA aliceWallet bobWallet) = do
  let testnet = SomeNetworkDiscriminant (Proxy @('Testnet 42))
      walletURL = BaseUrl Http "localhost" 8090 ""
      apiURL = Plutus.PAB.Types.baseUrl defaultWebServerConfig
  mstates <-
    mapM
      (runPerunClientInitializer . uncurry (withWallet testnet walletURL apiURL))
      [ (aliceWallet, (alicePhrase, alicePassphrase)),
        (bobWallet, (bobPhrase, bobPassphrase))
      ]
  let states =
        mstates >>= \case
          Left err -> error . show $ err
          Right r -> return r

  -- MultiClient description.
  randChannelId <- abs <$> randomM globalStdGen
  void . runMultiClientWith @["alice", "bob"] states $ do
    alicePKH <- actionBy @"alice" $ gets (PaymentPubKeyHash . (^. pubKeyHash))
    aliceSPK <- actionBy @"alice" $ gets (^. signingPubKey)
    bobPKH <- actionBy @"bob" $ gets (PaymentPubKeyHash . (^. pubKeyHash))
    bobSPK <- actionBy @"bob" $ gets (^. signingPubKey)
    -- Also subscribe to Websocket events for the contract instance of alice.
    subscribeToContractEvents @"alice"
    -- Endpoint Parameters
    let payPubKeyHashes = [alicePKH, bobPKH]
        signingPKs = [aliceSPK, bobSPK]
        startParams =
          OpenParams
            { spChannelId = randChannelId,
              spSigningPKs = signingPKs,
              spPaymentPKs = payPubKeyHashes,
              spBalances = [defaultBalance, defaultBalance],
              spTimeLock = defaultTimeLock
            }

        fundParams =
          FundParams randChannelId 1

        stateV1 = ChannelState randChannelId [defaultBalance `div` 2, (defaultBalance `div` 2) * 3] 1 False
        stateV2 = ChannelState randChannelId [defaultBalance + 5_000_000, defaultBalance - 5_000_000] 2 False
        forceCloseParams = ForceCloseParams randChannelId
    signedStateV1 <- update stateV1
    signedStateV2 <- update stateV2
    let disputeBobParams = DisputeParams signingPKs signedStateV1
        disputeAliceParams = DisputeParams signingPKs signedStateV2

    -- Trace definition.
    callEndpointFor @"alice" "start" startParams
    delayAll 10_000_000
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
