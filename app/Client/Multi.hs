{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Multi is a multi client environment which allows interleaving multiple
-- `PerunClient`s.
module Multi
  ( MultiClientState (..),
    actors,
    MultiClientError (..),
    runMultiClientWith,
    delayAll,
    callEndpointFor,
    actionBy,
    subscribeToContractEvents,
    update,
    mapAllClients,
  )
where

import Client
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception (Exception)
import Control.Lens
import Control.Logger.Simple
import Control.Monad.Except
import Control.Monad.State
import Data.Aeson (ToJSON)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Text (pack)
import GHC.TypeLits
import Perun.Onchain
import Servant.Client
import Websocket

newtype MultiClientState = MultiClientState
  { _multiClientStateActors :: Map.Map String PerunClientState
  }

makeFields ''MultiClientState

data MultiClientError
  = MultiClientImpossibleLookupErr
  | MultiClientEndpointErr !ClientError
  deriving (Show)

instance Exception MultiClientError

-- NOTE: For typelevel tracking to work, this cannot be a simple `type alias`
-- but HAS to be a `newtype/datatype`. Otherwise the `actors` tag gets lost
-- since it is not "carried" around by the underlying MTL stack.
newtype MultiClient (actors :: [Symbol]) a = MultiClient
  { unMultiClient :: ExceptT MultiClientError (StateT MultiClientState IO) a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError MultiClientError, MonadState MultiClientState)

runMultiClientWith ::
  forall actors a. (SymbolList actors) => [PerunClientState] -> MultiClient actors a -> IO (Either MultiClientError a)
runMultiClientWith creds action = do
  withGlobalLogging (LogConfig Nothing True) $ do
    let actorsList = symbolList @actors
        actorStates = Map.fromList . zip actorsList $ creds
    logInfo $ "Running MultiClient with set of participants: " <> (pack . unwords $ actorsList)
    res <- flip evalStateT (MultiClientState actorStates) . runExceptT . unMultiClient $ action
    logInfo "MultiClient trace done."
    return res

delayAll :: Int -> MultiClient actors ()
delayAll ms = do
  logInfo . pack . unwords $ ["Sleeping for", show $ fromIntegral @_ @Double ms / (1000 * 1000), "s"]
  liftIO . threadDelay $ ms

callEndpointFor :: forall actor actors a. (Show a, ToJSON a, HasActor actor actors) => String -> a -> MultiClient actors ()
callEndpointFor ep v = do
  logInfo . pack . unwords $ ["Calling endpoint:", ep, "with participant", symbolVal (Proxy @actor)]
  logInfo . pack . unwords $ ["Endpoint parameter:", show v]
  (actionBy @actor $ callEndpoint ep v) >>= \case
    Left err -> do
      logError . pack . unwords $ ["Endpoint call", ep, "failed!", "Message:", show err]
      throwError $ MultiClientEndpointErr err
    Right r -> return r

actionBy :: forall actor actors a. (HasActor actor actors) => PerunClient a -> MultiClient actors a
actionBy action = do
  let actorKey = symbolVal (Proxy @actor)
  actor <-
    gets (Map.lookup actorKey . _multiClientStateActors) >>= \case
      Nothing -> throwError MultiClientImpossibleLookupErr
      Just actor -> return actor
  logInfo . pack . unwords $ [actorKey, "executing..."]
  (res, newState) <- liftIO (evalPerunClient actor action)
  modify $ actors %~ Map.insert actorKey newState
  return res

subscribeToContractEvents :: forall actor actors. (HasActor actor actors) => MultiClient actors ()
subscribeToContractEvents = actionBy @actor $ do
  apiUrl <- gets (baseUrl . (^. pabClientEnv))
  cid <- gets (^. instClientId)
  void . liftIO . async $ runContractSubscription apiUrl cid

update :: forall actors. (SymbolList actors) => ChannelState -> MultiClient actors SignedState
update newState = SignedState <$> (mapAllClients @actors $ signState newState)

mapAllClients :: forall actors a. (SymbolList actors) => PerunClient a -> MultiClient actors [a]
mapAllClients action = do
  let actresses = symbolList @actors
  mapM (`actionBy'` action) actresses

-- | actionBy' is a constraintfree alternative for `actionBy`. This is only for
-- INTERNAL use where the user of the environment is guaranteed to have
-- satisfied the constraints of `actionBy`.
actionBy' :: String -> PerunClient a -> MultiClient actors a
actionBy' actor action = do
  s <-
    gets (Map.lookup actor . _multiClientStateActors) >>= \case
      Nothing -> throwError MultiClientImpossibleLookupErr
      Just s -> return s
  logInfo . pack . unwords $ [actor, "executing..."]
  (res, newState) <- liftIO (evalPerunClient s action)
  modify $ actors %~ Map.insert actor newState
  return res

-- | We define a class to turn our `typefamily FindActor` function into a
-- constraint. We CANNOT forget to declare an `instance` for this class though,
-- otherwise GHC will know about the constraint, but not that every instance
-- where `FindActor` finds the actor is REALLY an instance of this class.
class (FindActor actor actors ~ 'True, KnownSymbol actor) => HasActor actor actors

instance (FindActor actor actors ~ 'True, KnownSymbol actor) => HasActor actor actors

-- | SymbolList allows constructing a value level list of `String`s from a type
-- level list of `Symbol`s.
class SymbolList (ss :: [Symbol]) where
  symbolList :: [String]

instance SymbolList '[] where
  symbolList = []

instance (SymbolList ss, KnownSymbol s) => SymbolList (s ': ss) where
  symbolList = symbolVal (Proxy @s) : symbolList @ss

-- | FindActor looks up an actor in the given type level list of actors.
type family FindActor (actor :: Symbol) (actors :: [Symbol]) where
-- We use an explicit `TypeError` message here instead of keeping this
-- function generic, since it is only meant to be used in this specific
-- context.
  FindActor actor '[] =
    TypeError
      ( 'ShowType actor
          ':<>: 'Text " not defined in type-level list of MultiClient environment"
      )
  FindActor actor (actor ': ks) = 'True
  FindActor actor (k ': ks) = FindActor actor ks
