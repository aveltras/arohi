{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Arohi.Server where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as LBS
import Data.Constraint.Extras (Has)
import Data.Dependent.Map (Some(..))
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)
import Reflex.Dom hiding (run)
import System.Environment (lookupEnv)
import Arohi.Route
import Arohi.Route.Server
import Arohi.DataSource
import Arohi.DataSource.Server

#if defined(MIN_VERSION_jsaddle_warp)
import Control.Concurrent.Async (concurrently_)
import Network.Wai.Middleware.Cors ( cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))
import Language.Javascript.JSaddle (JSM)
import Language.Javascript.JSaddle.Run (syncPoint)
import Language.Javascript.JSaddle.Warp (jsaddleOr, jsaddleJs')

runApp ::
  ( t ~ DomTimeline
  , Semigroup r
  , Show r
  , w ~ RouteT t r (WithDataSource t req (PostBuildT t (StaticDomBuilderT t (PerformEventT t DomHost))))
  , Has ToJSON req
  , FromJSON (Some req)
  )
  => JSM () -> w ()-> w () -> (forall x. req x -> IO (Identity x)) -> (r -> Text) -> (Text -> Maybe r) -> IO ()
runApp fullWidget headWidget bodyWidget handler enc dec = do
  appPort <- maybe 8080 read <$> lookupEnv "AROHI_PORT"
  jsaddlePort <- maybe 3003 read <$> lookupEnv "JSADDLE_PORT"
  let wsServer = run appPort (websocketsOr defaultConnectionOptions (wsApp handler) $ httpApp "//localhost:3003/jsaddle.js" headWidget bodyWidget handler enc dec)
      jsaddleApp = runSettings (setPort jsaddlePort (setTimeout 3600 defaultSettings)) =<<
        (cors (const (Just $ simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ] } )) <$> jsaddleOr defaultConnectionOptions (fullWidget >> syncPoint) app)
  concurrently_ jsaddleApp wsServer

app :: Application
app req res = case (requestMethod req, pathInfo req) of
  ("GET", ["jsaddle.js"]) -> res $ responseLBS status200 [("Content-Type", "application/javascript")] $ jsaddleJs' (Just "http://localhost:3003") False
  _ -> res $ responseLBS status400 [] "Not a valid JSaddleWarp request"

#else

runApp ::
  ( t ~ DomTimeline
  , Semigroup r
  , Show r
  , w ~ RouteT t r (WithDataSource t req (PostBuildT t (StaticDomBuilderT t (PerformEventT t DomHost))))
  , Has ToJSON req
  , FromJSON (Some req)
  )
  => a -> w ()-> w () -> (forall x. req x -> IO (Identity x)) -> (r -> Text) -> (Text -> Maybe r) -> IO ()
runApp _ headWidget bodyWidget handler enc dec = do
  appPort <- maybe 8080 read <$> lookupEnv "AROHI_PORT"
  run appPort (websocketsOr defaultConnectionOptions (wsApp handler) $ httpApp "//localhost:3003/jsaddle.js" headWidget bodyWidget handler enc dec)

#endif

httpApp ::
  ( t ~ DomTimeline
  , Semigroup r
  , Show r
  , w ~ RouteT t r (WithDataSource t req (PostBuildT t (StaticDomBuilderT t (PerformEventT t DomHost))))
  -- , DomBuilder t w
  -- , MonadHold t w
  -- , MonadFix w
  -- , PerformEvent t w
  -- , PostBuild t w
  -- , Prerender js t w
  -- , TriggerEvent t w
  -- , HasDataSource t req w
  -- , Route t r w
  )
  => Text
  -> w ()
  -> w ()
  -> (forall x. req x -> IO (Identity x))
  -> (r -> Text)
  -> (Text -> Maybe r)
  -> Application
httpApp scriptPath headWidget bodyWidget handler enc dec request respond = do
  let prefix = decodeUtf8 $ (<>) "//" $ fromMaybe "" $ requestHeaderHost request
  (_, bs) <- renderStatic $ runIOSource handler $ runServerRoute request enc dec $
    el "html" $ do
      el "head" headWidget
      elAttr "script" ("src" =: scriptPath) blank
      elAttr "body" ("data-ws" =: ("ws:" <> prefix) <> "data-prefix" =: prefix) bodyWidget
  respond $ responseLBS
    status200
    [("Content-Type", "text/html")]
    (LBS.fromStrict $ "<!doctype html>" <> bs)
    
