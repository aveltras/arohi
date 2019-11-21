{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
module Reflex.DataSource.Server where

import Control.Monad (forever)
import Control.Monad.Fix
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import qualified Data.ByteString as BS
import Data.Dependent.Map (Some(..))
import Network.WebSockets
import Reflex.DataSource
import Data.Functor.Identity
import Data.Constraint.Extras
import Reflex.Dom hiding (Error)

wsApp ::
  ( Has ToJSON req
  , FromJSON (Some req)
  ) => (forall x. req x -> IO (Identity x)) -> ServerApp
wsApp handler pending_conn = do
    conn <- acceptRequest pending_conn
    forever $ do
      bsReq <- receiveData conn :: IO BS.ByteString
      forkPingThread conn 30

      case decodeTag bsReq of
        Nothing -> error "error decoding request"
        Just (int, val) ->
          case fromJSON val of
            Error s -> error s
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
