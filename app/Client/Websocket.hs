{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Websocket (runContractSubscription) where

import Control.Exception
import Control.Logger.Simple
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Text
import Data.Text.Encoding
import Network.WebSockets
import Plutus.PAB.Webserver.Types
import Servant.Client.Core
import Wallet.Types

runContractSubscription :: BaseUrl -> ContractInstanceId -> IO ()
runContractSubscription (BaseUrl _ host port _) cid = do
  withGlobalLogging
    (LogConfig Nothing True)
    (runClient host port ("ws/" <> (show . unContractInstanceId $ cid)) contractSub `catch` (logError . pack . show @ConnectionException))

data SubscriptionError
  = UnexpectedBinaryMessageErr
  | UnexpectedMessageTypeErr
  deriving (Show)

instance Exception SubscriptionError

contractSub :: Connection -> IO ()
contractSub conn =
  catch
    ( go `catch` \err -> do
        case err of
          UnexpectedBinaryMessageErr -> logError . pack . show $ UnexpectedBinaryMessageErr
          UnexpectedMessageTypeErr -> logError . pack . show $ UnexpectedMessageTypeErr
        go
    )
    $ \err -> do
      case err of
        ParseException s -> logError . pack $ s
        -- Rethrow broken connection exceptions.
        exception -> throw exception
      logInfo "Continuing reading from stream"
      go
  where
    -- Continue reading from subscription.

    go = do
      mmsg <-
        receiveDataMessage conn >>= \case
          Text raw _ -> return . decode @InstanceStatusToClient $ raw
          _otherwise -> throw UnexpectedBinaryMessageErr
      msg <- case mmsg of
        Just r -> return r
        Nothing -> throw UnexpectedMessageTypeErr
      logInfo . decodeUtf8 . toStrict . encode $ msg
      go
