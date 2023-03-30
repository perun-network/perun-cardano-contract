{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Perun.Client.Adjudicator where

import Perun.Client.Client
import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Text (Text, pack)
import Perun.PAB
import Perun.Onchain
import Plutus.PAB.Webserver.Client
import Plutus.PAB.Webserver.Types
import Servant.Client
import Wallet.Types (ContractInstanceId)
import Perun.Websocket

data AdjudicatorState = AdjudicatorState
  { _adjudicatorStateInstClientId :: !ContractInstanceId,
    _adjudicatorStatePabClientEnv :: !ClientEnv,
    _adjudicatorStateInstClient :: !(InstanceClient StarterContracts)
  }

makeFields ''AdjudicatorState

type AdjudicatorClient a = StateT AdjudicatorState IO a

newtype AdjudicatorError = AdjudicatorInitContractActivationErr Text
  deriving (Show)

runAdjudicatorForClient ::
  (MonadIO m, Monad m) =>
  PerunClientState ->
  ChannelID ->
  m ()
runAdjudicatorForClient ps cID = do
  let PabClient
        { activateContract,
          instanceClient
        } = pabClient @StarterContracts @Integer
      ca =
        ContractActivationArgs
          { caID = AdjudicatorContract cID,
            caWallet = Just $ ps ^. wallet
          }
  aid <-
    runClientInit (activateContract ca) (ps ^. pabClientEnv) >>= \case
      Left err -> error . show $ AdjudicatorInitContractActivationErr . pack . show $ err
      Right aid -> return aid
  let ss = AdjudicatorState aid (ps ^. pabClientEnv) (instanceClient aid)
  void . liftIO $ evalStateT watchAdjudicator ss

watchAdjudicator :: AdjudicatorClient ()
watchAdjudicator = do
  cid <- gets (^. instClientId)
  apiUrl <- gets (baseUrl . (^. pabClientEnv))
  liftIO $ runContractSubscription apiUrl cid
