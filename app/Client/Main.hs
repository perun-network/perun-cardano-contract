{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Wallet.Api.Client (AddressClient (..), addressClient)
import Cardano.Wallet.Api.Types (ApiT (..))
import qualified Cardano.Wallet.Primitive.Types as Types
import Data.Aeson (toJSON)
import Data.Default
import Data.Text (pack)
import Data.Text.Class (fromText)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Options.Applicative
import PAB (StarterContracts (..))
import Perun.Offchain (OpenParams (..))
import Plutus.PAB.Types (Config (..), WebserverConfig (..), defaultWebServerConfig)
import Plutus.PAB.Webserver.Client (InstanceClient (..), PabClient (..), pabClient)
import Plutus.PAB.Webserver.Types (ContractActivationArgs (..))
import Servant.Client (ClientError (..), mkClientEnv, runClientM)
import Servant.Client.Core.BaseUrl (BaseUrl (..), Scheme (..))
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
defaultBalance = 100_000

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
  resA <-
    runClientM myCl walletEnv >>= \case
      Left err -> print err >> exitFailure
      Right v -> return v
  resB <- runClientM pCl walletEnv

  print resA
  print resB

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

  print "started contract instance:"
  print cid
  _ <- exitSuccess

  -- Create client allowing to call endpoints on `PerunContract`.
  let InstanceClient {callInstanceEndpoint} = instanceClient cid
      callStart = callInstanceEndpoint "start"
      callFund = callInstanceEndpoint "fund"
      callAbort = callInstanceEndpoint "abort"
      callOpen = callInstanceEndpoint "open"
      callDispute = callInstanceEndpoint "dispute"
      callClose = callInstanceEndpoint "close"
      callForceClose = callInstanceEndpoint "forceClose"

      -- Start a channel.
      openParams =
        OpenParams
          { spChannelId = 42069, -- TODO: Hardcoded...
            spSigningPKs = undefined, -- [myPk, ppk],
            spPaymentPKs = undefined, -- map (PaymentPubKeyHash . Crypto.pubKeyHash) [myPk, ppk],
            spBalances = [defaultBalance, defaultBalance],
            spTimeLock = defaultTimeLock
          }
  runClientM (callStart . toJSON $ openParams) clientEnv >>= \case
    Left ce -> case ce of
      f@(FailureResponse _ _) -> print f >> exitFailure
      d@(DecodeFailure _ _) -> print d >> exitFailure
      uct@(UnsupportedContentType _ _) -> print uct >> exitFailure
      icth@(InvalidContentTypeHeader _) -> print icth >> exitFailure
      cerr@(ConnectionError _) -> print cerr >> exitFailure
    Right _ -> print ("successfully started channel with channelID: " <> (show . spChannelId $ openParams))
