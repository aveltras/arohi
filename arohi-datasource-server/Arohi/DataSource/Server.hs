{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
module Arohi.DataSource.Server where

import Control.Monad (forever)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, Result(..), encode, fromJSON)
import Data.Constraint.Extras (Has, has)
import Data.Dependent.Map (Some(..))
import Data.Functor.Identity (Identity)
import Network.WebSockets (ServerApp, acceptRequest, forkPingThread, receiveData, sendBinaryData)
import Arohi.DataSource (WithDataSource, decodeTag)
import Reflex.Dom hiding (Error, Value)

wsApp ::
  ( Has ToJSON req
  , FromJSON (Some req)
  ) => (forall x. req x -> IO (Identity x)) -> ServerApp
wsApp handler pending_conn = do
    conn <- acceptRequest pending_conn
    forever $ do
      bsReq <- receiveData conn
      forkPingThread conn 30

      case decodeTag bsReq of
        Nothing -> return ()
        Just (int, val) ->
          case fromJSON val of
            Error _ -> return ()
            Success (Some req) -> do
              resp <- handler req
              sendBinaryData conn $ has @ToJSON req $ encode (int, resp)
      return ()

runIOSource ::
  ( MonadFix m
  , PerformEvent t m
  , MonadIO (Performable m)
  ) => (forall x. req x -> IO (Identity x))
  -> WithDataSource t req m a
  -> m a
runIOSource h w = mdo
  (val, eRequest) <- runRequesterT w eResponse
  eResponse <- performEvent $ liftIO . traverseRequesterData h <$> eRequest
  return val
