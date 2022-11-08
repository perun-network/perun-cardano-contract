{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Api
import Cardano.Crypto.Wallet (XPrv, xpub)
import Cardano.Mnemonic
import Cardano.Wallet.Api.Client (AddressClient (..), addressClient)
import qualified Cardano.Wallet.Api.Link as Link
import Cardano.Wallet.Api.Types as AT (ApiAccountKey (..), ApiAddress (..), ApiT (..), KeyFormat (..), WalletStyle (..))
import Cardano.Wallet.Primitive.AddressDerivation (NetworkDiscriminant (..))
import Cardano.Wallet.Primitive.Passphrase (Passphrase (..))
import qualified Cardano.Wallet.Primitive.Types as Types
import Cardano.Wallet.Primitive.Types.Address (Address (..))
import Cardano.Wallet.Shelley.Compatibility ()
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad
import Control.Monad ((>=>))
import Data.Aeson (Result (..), fromJSON, toJSON)
import Data.ByteString (concat)
import Data.Default
import Data.Either
import Data.Text (Text, pack, unpack)
import Data.Text.Class (fromText)
import Data.Text.Encoding (encodeUtf8)
import Ledger (PaymentPrivateKey (..), PaymentPubKey (..), PaymentPubKeyHash (..), xPubToPublicKey)
import Ledger.Crypto (toPublicKey)
import qualified Ledger.Crypto as Crypto
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Simple hiding (Proxy)
import Options.Applicative hiding (Success)
import PAB (StarterContracts (..))
import Perun (CloseParams (..), DisputeParams (..), ForceCloseParams (..), FundParams (..), SignedState (..))
import Perun.Offchain (OpenParams (..))
import Perun.Onchain (ChannelState (..))
import Plutus.Contract.Oracle (signMessage)
import Plutus.PAB.Types (Config (..), WebserverConfig (..), defaultWebServerConfig)
import Plutus.PAB.Webserver.Client (InstanceClient (..), PabClient (..), pabClient)
import Plutus.PAB.Webserver.Types (ContractActivationArgs (..))
import Servant.Client (ClientError (..), mkClientEnv, runClientM)
import Servant.Client.Core.BaseUrl (BaseUrl (..), Scheme (..), showBaseUrl)
import System.Exit
import System.Random.Stateful
import Wallet.Emulator.Wallet (Wallet (..), WalletId (..))
import Websocket
import Prelude hiding (concat)

data CmdLineArgs = CLA
  { myWallet :: Wallet,
    peerPaymentPubKey :: Wallet
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

addressFromApi :: ApiAddress n -> Address
addressFromApi = getApiT . fst . AT.id

main' :: forall n. (n ~ 'Testnet 42) => CmdLineArgs -> IO ()
main' (CLA aliceWallet bobWallet) = do
  -- Get parameters to establish connection.
  mgr <- newManager defaultManagerSettings
  let apiUrl = Plutus.PAB.Types.baseUrl defaultWebServerConfig
      walletUrl = BaseUrl Http "localhost" 8090 ""
      walletEnv = mkClientEnv mgr walletUrl
      clientEnv = mkClientEnv mgr apiUrl

  -- Get addresses for wallets.
  let AddressClient {listAddresses} = addressClient
      aliceCl = listAddresses (ApiT (walletId aliceWallet)) Nothing
      bobCl = listAddresses (ApiT (walletId bobWallet)) Nothing
  resultAddresses <-
    mapM
      ( \i ->
          runClientM i walletEnv >>= \case
            Left err -> print err >> exitFailure
            Right (v : _) -> return . fromJSON @(ApiAddress n) $ v
            Right _ -> print @String "No addresses associated with given wallet" >> exitFailure
      )
      [aliceCl, bobCl]
  addresses <-
    mapM
      ( \case
          Success addr -> return . Api.deserialiseFromRawBytes Api.AsShelleyAddress . unAddress . addressFromApi $ addr
          Error s -> print s >> exitFailure
      )
      resultAddresses
  pubKeyHashes <-
    mapM
      ( \case
          Just x -> case Api.shelleyPayAddrToPlutusPubKHash x of
            Just pkh -> return . PaymentPubKeyHash $ pkh
            Nothing -> print @String "unable to derive PubKeyHash" >> exitFailure
          Nothing -> print @String "unable to derive PubKeyHash" >> exitFailure
      )
      addresses

  let (_, endpointUrlA) = Link.getAccountKey @'Shelley (ApiT (walletId aliceWallet)) (Just Extended)
      (_, endpointUrlB) = Link.getAccountKey @'Shelley (ApiT (walletId bobWallet)) (Just Extended)
  eitherPubKeys <-
    mapM
      ( parseRequest . unpack . ((pack (showBaseUrl walletUrl) <> "/") <>) >=> httpJSON @IO @ApiAccountKey
          >=> return
            . xpub
            . getApiAccountKey
            . getResponseBody
      )
      [endpointUrlA, endpointUrlB]
  let paymentPubKeys = map (PaymentPubKey . xPubToPublicKey) (rights eitherPubKeys)

  -- Activate contracts.
  let caAlice =
        ContractActivationArgs
          { caID = PerunContract,
            caWallet = Just aliceWallet
          }
      PabClient
        { activateContract,
          instanceClient
        } = pabClient @StarterContracts @Integer

  let caBob =
        ContractActivationArgs
          { caID = PerunContract,
            caWallet = Just bobWallet
          }
      PabClient
        { activateContract,
          instanceClient
        } = pabClient @StarterContracts @Integer

  cidAlice <-
    runClientM (activateContract caAlice) clientEnv >>= \case
      Left err -> print err >> exitFailure
      Right cid -> return cid

  cidBob <-
    runClientM (activateContract caBob) clientEnv >>= \case
      Left err -> print err >> exitFailure
      Right cid -> return cid

  void . async $ runContractSubscription apiUrl cidAlice
  void . async $ runContractSubscription apiUrl cidBob

  randChannelId <- abs <$> randomM globalStdGen

  -- Create client allowing to call endpoints on `PerunContract`.
  let aliceInstanceClient = instanceClient cidAlice
      bobInstanceClient = instanceClient cidBob
      callStartAlice = callInstanceEndpoint aliceInstanceClient "start"
      callStartBob = callInstanceEndpoint bobInstanceClient "start"
      callFundAlice = callInstanceEndpoint aliceInstanceClient "fund"
      callFundBob = callInstanceEndpoint bobInstanceClient "fund"
      callAbortAlice = callInstanceEndpoint aliceInstanceClient "abort"
      callAbortBob = callInstanceEndpoint bobInstanceClient "abort"
      callOpenAlice = callInstanceEndpoint aliceInstanceClient "open"
      callOpenBob = callInstanceEndpoint bobInstanceClient "open"
      callDisputeAlice = callInstanceEndpoint aliceInstanceClient "dispute"
      callDisputeBob = callInstanceEndpoint bobInstanceClient "dispute"
      callCloseAlice = callInstanceEndpoint aliceInstanceClient "close"
      callCloseBob = callInstanceEndpoint bobInstanceClient "close"
      callForceCloseAlice = callInstanceEndpoint aliceInstanceClient "forceClose"
      callForceCloseBob = callInstanceEndpoint bobInstanceClient "forceClose"
      --callDummy = callInstanceEndpoint "dummy"
      --callDummyPayment = callInstanceEndpoint "dummyPayment"

      -- Endpoint Parameters
      startParams =
        OpenParams
          { spChannelId = randChannelId,
            spSigningPKs = [signingPubKeyAlice, signingPubKeyBob],
            spPaymentPKs = pubKeyHashes,
            spBalances = [defaultBalance, defaultBalance],
            spTimeLock = defaultTimeLock
          }

      fundParams =
        FundParams randChannelId 1

      signingPKs = [signingPubKeyAlice, signingPubKeyBob]

      stateV1 = ChannelState randChannelId [defaultBalance `div` 2, (defaultBalance `div` 2) * 3] 1 False
      signedStateV1 = update stateV1
      stateV2 = ChannelState randChannelId [defaultBalance + 5_000_000, defaultBalance - 5_000_000] 2 False
      signedStateV2 = update stateV2
      disputeBobParams = DisputeParams signingPKs signedStateV1
      disputeAliceParams = DisputeParams signingPKs signedStateV2
      forceCloseParams = ForceCloseParams randChannelId

  -- Start Alice
  runClientM (callStartAlice . toJSON $ startParams) clientEnv >>= \case
    Left ce -> case ce of
      f@(FailureResponse _ _) -> print f >> exitFailure
      d@(DecodeFailure _ _) -> print d >> exitFailure
      uct@(UnsupportedContentType _ _) -> print uct >> exitFailure
      icth@(InvalidContentTypeHeader _) -> print icth >> exitFailure
      cerr@(ConnectionError _) -> print cerr >> exitFailure
    Right _ -> print ("Alice initialized start for channel with ID: " <> (show . spChannelId $ startParams))

  threadDelay 120_000_000

  -- Fund Bob
  runClientM (callFundBob . toJSON $ fundParams) clientEnv >>= \case
    Left ce -> case ce of
      f@(FailureResponse _ _) -> print f >> exitFailure
      d@(DecodeFailure _ _) -> print d >> exitFailure
      uct@(UnsupportedContentType _ _) -> print uct >> exitFailure
      icth@(InvalidContentTypeHeader _) -> print icth >> exitFailure
      cerr@(ConnectionError _) -> print cerr >> exitFailure
    Right _ -> print ("Bob initialized funding for channel with ID: " <> (show . spChannelId $ startParams))

  threadDelay 120_000_000

  -- (Malicious) Dispute Bob
  runClientM (callDisputeBob . toJSON $ disputeBobParams) clientEnv >>= \case
    Left ce -> case ce of
      f@(FailureResponse _ _) -> print f >> exitFailure
      d@(DecodeFailure _ _) -> print d >> exitFailure
      uct@(UnsupportedContentType _ _) -> print uct >> exitFailure
      icth@(InvalidContentTypeHeader _) -> print icth >> exitFailure
      cerr@(ConnectionError _) -> print cerr >> exitFailure
    Right _ -> print ("Bob initialized malicious dispute with old state for channel with ID: " <> (show . spChannelId $ startParams))

  threadDelay 120_000_000

  -- Dispute Alice
  runClientM (callDisputeAlice . toJSON $ disputeAliceParams) clientEnv >>= \case
    Left ce -> case ce of
      f@(FailureResponse _ _) -> print f >> exitFailure
      d@(DecodeFailure _ _) -> print d >> exitFailure
      uct@(UnsupportedContentType _ _) -> print uct >> exitFailure
      icth@(InvalidContentTypeHeader _) -> print icth >> exitFailure
      cerr@(ConnectionError _) -> print cerr >> exitFailure
    Right _ -> print ("Alice initialized dispute with most recent state for channel with ID: " <> (show . spChannelId $ startParams))
  print ("Waiting for timelock: " <> show defaultTimeLock <> " + chain-index lag")

  threadDelay 120_000_000

  -- ForceClose Alice
  runClientM (callForceCloseAlice . toJSON $ forceCloseParams) clientEnv >>= \case
    Left ce -> case ce of
      f@(FailureResponse _ _) -> print f >> exitFailure
      d@(DecodeFailure _ _) -> print d >> exitFailure
      uct@(UnsupportedContentType _ _) -> print uct >> exitFailure
      icth@(InvalidContentTypeHeader _) -> print icth >> exitFailure
      cerr@(ConnectionError _) -> print cerr >> exitFailure
    Right _ -> print ("Alice initialized force close for channel with ID: " <> (show . spChannelId $ startParams))

update :: ChannelState -> SignedState
update newState =
  SignedState
    [ signMessage newState signingKeyAlice alicePassphrase',
      signMessage newState signingKeyBob bobPassphrase'
    ]

signingKeyAlice :: PaymentPrivateKey
signingKeyAlice = PaymentPrivateKey getKeyAlice

signingPubKeyAlice :: PaymentPubKey
signingPubKeyAlice = PaymentPubKey . toPublicKey $ getKeyAlice

signingKeyBob :: PaymentPrivateKey
signingKeyBob = PaymentPrivateKey getKeyBob

signingPubKeyBob :: PaymentPubKey
signingPubKeyBob = PaymentPubKey . toPublicKey $ getKeyBob

getKeyAlice :: XPrv
getKeyAlice = Crypto.generateFromSeed (concat $ map encodeUtf8 alicePhrase) alicePassphrase'

alicePassphrase :: Passphrase "encryption"
alicePassphrase = Passphrase "01233456789"

getKeyBob :: XPrv
getKeyBob = Crypto.generateFromSeed (concat $ map encodeUtf8 bobPhrase) bobPassphrase'

getMnemonic :: [Text] -> (SomeMnemonic, Maybe SomeMnemonic)
getMnemonic phrase =
  let mn =
        mkSomeMnemonic @'[15] phrase
   in case mn of
        Right b -> (b, Nothing)
        _ -> error "whatever"

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

bobPassphrase :: Passphrase "encryption"
bobPassphrase = Passphrase "01233456789"

alicePassphrase' :: Crypto.Passphrase
alicePassphrase' = Crypto.Passphrase "0123456789"

bobPassphrase' :: Crypto.Passphrase
bobPassphrase' = Crypto.Passphrase "0123456789"
