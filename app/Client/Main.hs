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
import Cardano.Wallet.Primitive.AddressDerivation.Shelley (generateKeyFromSeed, getKey)
import Cardano.Wallet.Primitive.Passphrase (Passphrase (..))
import qualified Cardano.Wallet.Primitive.Types as Types
import Cardano.Wallet.Primitive.Types.Address (Address (..))
import Cardano.Wallet.Shelley.Compatibility ()
import Control.Concurrent (threadDelay)
import Control.Monad ((>=>))
import Data.Aeson (Result (..), encode, fromJSON, toJSON)
import Data.Default
import Data.Either
import Data.Text (Text, pack, unpack)
import Data.Text.Class (fromText)
import Ledger (PaymentPrivateKey (..), PaymentPubKey (..), PaymentPubKeyHash (..), PrivateKey, paymentPubKeyHash, xPubToPublicKey)
import Ledger.Crypto (toPublicKey)
import qualified Ledger.Crypto as Crypto
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Simple
import Options.Applicative hiding (Success)
import PAB (StarterContracts (..))
import Perun (CloseParams (..), SignedState (..))
import Perun.Offchain (OpenParams (..))
import Perun.Onchain (ChannelState (..))
import Plutus.Contract.Oracle (SignedMessage, signMessage, signMessage')
import Plutus.PAB.Events.ContractInstanceState (logs)
import Plutus.PAB.Types (Config (..), WebserverConfig (..), defaultWebServerConfig)
import Plutus.PAB.Webserver.Client (InstanceClient (..), PabClient (..), pabClient)
import Plutus.PAB.Webserver.Types (ContractActivationArgs (..), ContractInstanceClientState (..))
import Servant.Client (ClientError (..), mkClientEnv, runClientM)
import Servant.Client.Core.BaseUrl (BaseUrl (..), Scheme (..), showBaseUrl)
import System.Exit
import Wallet.Emulator.Wallet (Wallet (..), WalletId (..))

data CmdLineArgs = CLA
  { myWallet :: Wallet,
    peerPaymentPubKey :: Wallet
  }

cmdLineParser :: Parser CmdLineArgs
cmdLineParser =
  CLA
    <$> parseWallet
      ( long "walletid"
          <> short 'w'
          <> help
            "Wallet identifier from local cardano-wallet"
      )
    <*> parseWallet
      ( long "walletid-peer"
          <> short 'p'
          <> help
            "Wallet identifier of peer"
      )
  where
    parseWallet :: Mod OptionFields String -> Parser Wallet
    parseWallet opts = Wallet Nothing . WalletId . right . fromText . pack <$> strOption opts
    right (Right a) = a
    right _ = error "parsing failed"

defaultTimeLock :: Integer
defaultTimeLock = 15 * 1000

defaultPabConfig :: Config
defaultPabConfig = def

defaultBalance :: Integer
defaultBalance = 100_000_000

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
main' (CLA myWallet peerWallet) = do
  -- Get parameters to establish connection.
  mgr <- newManager defaultManagerSettings
  let apiUrl = Plutus.PAB.Types.baseUrl defaultWebServerConfig
      walletUrl = BaseUrl Http "localhost" 8090 ""
      walletEnv = mkClientEnv mgr walletUrl
      clientEnv = mkClientEnv mgr apiUrl

  print myWallet
  print peerWallet
  -- Get addresses for wallets.
  let AddressClient {listAddresses} = addressClient
      myCl = listAddresses (ApiT (walletId myWallet)) Nothing
      pCl = listAddresses (ApiT (walletId peerWallet)) Nothing
  resultAddresses <-
    mapM
      ( \i ->
          runClientM i walletEnv >>= \case
            Left err -> print err >> exitFailure
            Right (v : _) -> return . fromJSON @(ApiAddress n) $ v
            Right _ -> print @String "No addresses associated with given wallet" >> exitFailure
      )
      [myCl, pCl]
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

  let (_, endpointUrlA) = Link.getAccountKey @'Shelley (ApiT (walletId myWallet)) (Just Extended)
      (_, endpointUrlB) = Link.getAccountKey @'Shelley (ApiT (walletId peerWallet)) (Just Extended)
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

  -- Activate contract.
  let ca =
        ContractActivationArgs
          { caID = PerunContract,
            caWallet = Just myWallet
          }
      PabClient
        { activateContract,
          instanceClient
        } = pabClient @StarterContracts @Integer

  cid <-
    runClientM (activateContract ca) clientEnv >>= \case
      Left err -> print err >> exitFailure
      Right cid -> return cid

  print @String "started contract instance:"
  print cid

  -- Create client allowing to call endpoints on `PerunContract`.
  let InstanceClient {getInstanceStatus, callInstanceEndpoint} = instanceClient cid
      callStart = callInstanceEndpoint "start"
      -- callFund = callInstanceEndpoint "fund"
      -- callAbort = callInstanceEndpoint "abort"
      callOpen = callInstanceEndpoint "open"
      -- callDispute = callInstanceEndpoint "dispute"
      callClose = callInstanceEndpoint "close"
      -- callForceClose = callInstanceEndpoint "forceClose"
      callDummy = callInstanceEndpoint "dummy"
      callDummyPayment = callInstanceEndpoint "dummyPayment"

      -- Start a channel.
      openParams =
        OpenParams
          { spChannelId = 42069, -- TODO: Hardcoded...
            spSigningPKs = [signingPubKeyAlice, signingPubKeyBob],
            spPaymentPKs = pubKeyHashes,
            spBalances = [defaultBalance, defaultBalance],
            spTimeLock = defaultTimeLock
          }

  --  runClientM (callOpen . toJSON $ openParams) clientEnv >>= \case
  --    Left ce -> case ce of
  --      f@(FailureResponse _ _) -> print f >> exitFailure
  --      d@(DecodeFailure _ _) -> print d >> exitFailure
  --      uct@(UnsupportedContentType _ _) -> print uct >> exitFailure
  --      icth@(InvalidContentTypeHeader _) -> print icth >> exitFailure
  --      cerr@(ConnectionError _) -> print cerr >> exitFailure
  --    Right _ -> print ("successfully requested open for channel with ID: " <> (show . spChannelId $ openParams))

  --threadDelay 10_000_000sdf



  let newState = ChannelState 42069 [defaultBalance `div` 2, (defaultBalance `div` 2) * 3] 1 True
      signedState = update newState
      closeParams = CloseParams [signingPubKeyAlice, signingPubKeyBob] signedState

  runClientM (callClose . toJSON $ closeParams) clientEnv >>= \case
    Left ce -> case ce of
      f@(FailureResponse _ _) -> print f >> exitFailure
      d@(DecodeFailure _ _) -> print d >> exitFailure
      uct@(UnsupportedContentType _ _) -> print uct >> exitFailure
      icth@(InvalidContentTypeHeader _) -> print icth >> exitFailure
      cerr@(ConnectionError _) -> print cerr >> exitFailure
    Right _ -> print ("successfully requested close for channel with ID: " <> (show . spChannelId $ openParams))

FIX THIS PLS:


update :: ChannelState -> SignedState
update newState = SignedState [signMessage newState signingKeyAlice alicePassphrase', signMessage newState signingKeyBob bobPassphrase']

signingKeyAlice :: PaymentPrivateKey
signingKeyAlice = PaymentPrivateKey getKeyAlice

signingPubKeyAlice :: PaymentPubKey
signingPubKeyAlice = PaymentPubKey $ toPublicKey getKeyAlice

signingKeyBob :: PaymentPrivateKey
signingKeyBob = PaymentPrivateKey getKeyBob

signingPubKeyBob :: PaymentPubKey
signingPubKeyBob = PaymentPubKey $ toPublicKey getKeyBob

getKeyAlice :: XPrv
getKeyAlice = getKey $ generateKeyFromSeed (getMnemonic alicePhrase) alicePassphrase

alicePassphrase :: Passphrase "encryption"
alicePassphrase = Passphrase "01233456789"

getKeyBob :: XPrv
getKeyBob = getKey $ generateKeyFromSeed (getMnemonic bobPhrase) bobPassphrase

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