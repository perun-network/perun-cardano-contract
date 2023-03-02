{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Client where

import Cardano.Api hiding (Value)
import Cardano.Api.Shelley hiding (Value)
import Cardano.Wallet.Api.Client
import qualified Cardano.Wallet.Api.Types as AT (ApiAddress (id), ApiT (..))
import qualified Cardano.Wallet.Primitive.Types as Types
import qualified Cardano.Wallet.Primitive.Types.Address as Types
import Cardano.Wallet.Shelley
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Aeson (Result (..), fromJSON, toJSON)
import qualified Data.ByteString as BS
import Data.Monoid (Last (..))
import Data.Proxy
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Ledger.Address (PaymentPrivateKey (..), PaymentPubKey (..))
import Ledger.Crypto (Passphrase, PubKeyHash, generateFromSeed, toPublicKey)
import Network.HTTP.Client hiding (Proxy)
import PAB
import Perun.Onchain
import Plutus.Contract.Oracle
import Plutus.PAB.Webserver.Client
import Plutus.PAB.Webserver.Types
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (..))
import Servant.Client
import Wallet.Emulator.Wallet
import Wallet.Types (ContractInstanceId)
import Prelude hiding (id)
import Plutus.PAB.Webserver.Client (InstanceClient(getInstanceStatus))
import Plutus.PAB.Webserver.Types (ContractInstanceClientState(cicCurrentState))

data PerunClientState = PerunClientState
  { _perunClientStateAddress :: !(Address ShelleyAddr),
    _perunClientStatePubKeyHash :: !PubKeyHash,
    _perunClientStateSigningPubKey :: !PaymentPubKey,
    _perunClientStatePrivateKey :: !(PaymentPrivateKey, Passphrase),
    _perunClientStateWallet :: !Wallet,
    _perunClientStateInstClientId :: !ContractInstanceId,
    _perunClientStateInstClient :: !(InstanceClient StarterContracts),
    _perunClientStatePabClientEnv :: !ClientEnv,
    _perunClientStateWalletClientEnv :: !ClientEnv
  }

makeFields ''PerunClientState

type PerunClient a = StateT PerunClientState IO a

type PerunClientInitializer a = ExceptT PerunClientInitError IO a

runPerunClientInitializer :: PerunClientInitializer a -> IO (Either PerunClientInitError a)
runPerunClientInitializer = runExceptT

data PerunClientInitError
  = PerunClientInitErr !Text
  | PerunClientInitDecodingErr !Text
  | PerunClientInitAddressDeserializationErr
  | PerunClientInitAddrToPubKHashErr
  | PerunClientInitContractActivationErr !Text
  deriving (Show)

instance Exception PerunClientInitError

-- | runPerunClient runs the given `PerunClient` action.
runPerunClient :: PerunClientState -> PerunClient a -> IO a
runPerunClient pcs action = evalStateT action pcs

-- | evalPerunClient evaluates the given `PerunClient` action.
evalPerunClient :: PerunClientState -> PerunClient a -> IO (a, PerunClientState)
evalPerunClient pcs action = runStateT action pcs

withWallet ::
  SomeNetworkDiscriminant ->
  BaseUrl ->
  BaseUrl ->
  Wallet ->
  ([Text], Passphrase) ->
  PerunClientInitializer PerunClientState
withWallet (SomeNetworkDiscriminant (Proxy :: Proxy n)) walletURL pabURL wlt (mnemonic, pp) = do
  mgr <- liftIO $ newManager defaultManagerSettings
  let AddressClient {listAddresses} = addressClient
      wce = mkClientEnv mgr walletURL
      pce = mkClientEnv mgr pabURL
      cl = listAddresses (AT.ApiT . walletIdFromWallet $ wlt) Nothing
  res <-
    runClientInit cl wce >>= \case
      Left err -> throwError $ PerunClientInitErr . pack . show $ err
      Right [] -> throwError $ PerunClientInitErr "no address for walletid returned by endpoint"
      Right (v : _) -> return . fromJSON @(AT.ApiAddress n) $ v
  maddr <- case res of
    Error s -> throwError $ PerunClientInitDecodingErr . pack $ s
    Success addr ->
      return
        . deserialiseFromRawBytes AsShelleyAddress
        . Types.unAddress
        . addressFromApi
        $ addr
  addr <- case maddr of
    Nothing -> throwError PerunClientInitAddressDeserializationErr
    Just addr -> return addr
  pkh <- case shelleyPayAddrToPlutusPubKHash addr of
    Nothing -> throwError PerunClientInitAddrToPubKHashErr
    Just pkh -> return pkh

  let PabClient
        { activateContract,
          instanceClient
        } = pabClient @StarterContracts @Integer
      ca =
        ContractActivationArgs
          { caID = PerunContract,
            caWallet = Just wlt
          }
  cid <-
    runClientInit (activateContract ca) pce >>= \case
      Left err -> throwError $ PerunClientInitContractActivationErr . pack . show $ err
      Right cid -> return cid

  let pk = privateKeyFromMnemonic (mnemonic, pp)
      ppk = signingPubKeyFromMnemonic (mnemonic, pp)

  return
    PerunClientState
      { _perunClientStateAddress = addr,
        _perunClientStatePubKeyHash = pkh,
        _perunClientStatePrivateKey = (pk, pp),
        _perunClientStateWallet = wlt,
        _perunClientStateInstClientId = cid,
        _perunClientStateInstClient = instanceClient cid,
        _perunClientStatePabClientEnv = pce,
        _perunClientStateWalletClientEnv = wce,
        _perunClientStateSigningPubKey = ppk
      }

-- | runClientInit runs the `Servant.ClientM` in the `PerunClientInitializer`
-- context.
runClientInit :: (MonadIO m) => ClientM a -> ClientEnv -> m (Either ClientError a)
runClientInit cm = liftIO . runClientM cm

-- | runClient runs the `Servant.ClientM` in the `PerunClient` context.
runClient ::
  (MonadIO m, MonadState PerunClientState m) =>
  ClientM a ->
  ClientEnv ->
  m (Either ClientError a)
runClient cm = liftIO . runClientM cm

callEndpoint ::
  (MonadIO m, MonadState PerunClientState m, ToJSON a) =>
  String ->
  a ->
  m (Either ClientError ())
callEndpoint ep v = do
  inst <- gets (^. instClient)
  env <- gets (^. pabClientEnv)
  runClient (callInstanceEndpoint inst ep (toJSON v)) env

signState :: ChannelState -> PerunClient (SignedMessage ChannelState)
signState newState = do
  (signingKey, passphrase) <- gets (^. privateKey)
  return $ signMessage newState signingKey passphrase

-- | delay lets the `PerunClient` sleep for the given number of microseconds.
delay :: Int -> PerunClient ()
delay = liftIO . threadDelay

signingPubKeyFromMnemonic :: ([Text], Passphrase) -> PaymentPubKey
signingPubKeyFromMnemonic (mnemonic, passphrase) =
  PaymentPubKey
    . toPublicKey
    . generateFromSeed (BS.concat $ map encodeUtf8 mnemonic)
    $ passphrase

privateKeyFromMnemonic :: ([Text], Passphrase) -> PaymentPrivateKey
privateKeyFromMnemonic (mnemonic, passphrase) =
  PaymentPrivateKey . generateFromSeed (BS.concat $ map encodeUtf8 mnemonic) $
    passphrase

walletIdFromWallet :: Wallet -> Types.WalletId
walletIdFromWallet (Wallet _ (WalletId wid)) = wid

addressFromApi :: AT.ApiAddress n -> Types.Address
addressFromApi = AT.getApiT . fst . AT.id

getObservableState :: PerunClient (Maybe ChannelToken)
getObservableState = do
  InstanceClient {getInstanceStatus} <- gets (^. instClient)
  env <- gets (^. pabClientEnv)
  contractState <-
    runClient getInstanceStatus env >>= \case
      Left err -> error $ show err -- FIXME: Add PeruncClientError
      Right r -> return r
  let currentState = cicCurrentState contractState
      (Success lst) = fromJSON @(Last ChannelToken) $ observableState currentState 
  return $ getLast lst