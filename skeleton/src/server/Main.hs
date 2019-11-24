{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Common.Api
import Common.Frontend
import Common.Sitemap
import Control.Monad.Fix
import qualified Data.ByteString.Lazy as LBS
import Data.Functor.Identity (Identity(..))
import Data.Text
import Network.HTTP.Types
import Network.Wai
import Reflex.DataSource
import Reflex.DataSource.Server
import Reflex.Dom hiding (run)
import Reflex.Route
import Reflex.Route.Server
#if defined(MIN_VERSION_reflex_devserver)
import Reflex.DevServer
#else
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)
#endif
import System.Environment (lookupEnv)

main :: IO ()
main = do
  appPort <- maybe 8080 read <$> lookupEnv "APP_PORT"
  wsHost <- maybe "localhost" pack <$> lookupEnv "WS_URL"
  let wsUrl = "ws://" <> wsHost <> ":" <> (pack . show) appPort
#if defined(MIN_VERSION_reflex_devserver)
  jsaddlePort <- maybe 3003 read <$> lookupEnv "JSADDLE_PORT"
  devServer jsaddlePort appPort (Just "../ghcid.reload") entryPoint (app wsUrl) (wsApp handler)
#else
  run appPort $ websocketsOr defaultConnectionOptions (wsApp handler) (app wsUrl)
#endif

handler :: RequestG a -> IO (Identity a)
handler = \case
  RequestG1 -> return $ Identity False
  RequestG2 i -> return $ Identity (i + 2)

staticW ::
  ( req ~ RequestG
  , r ~ Sitemap
  , DomBuilder t m
  , MonadHold t m
  , MonadFix m
  , PerformEvent t m
  , PostBuild t m
  , Prerender js t m
  , TriggerEvent t m
  , HasDataSource t req m
  , Route t r m
  ) => Text -> m () -> m () -> m ()
staticW wsUrl hW bodyW = do
  prefix <- askPrefix
  el "html" $ do
    el "head" $ do
      hW
      elAttr "script" ("src" =: scriptFile) blank    
    elAttr "body" ("data-ws" =: wsUrl <> "data-prefix" =: prefix) bodyW

scriptFile :: Text
#if defined(MIN_VERSION_reflex_devserver)
scriptFile = "jsaddle.js"
#else
scriptFile = "all.js"
#endif

app :: Text -> Application
app wsUrl request respond = do
  (_, bs) <- renderStatic $ runIOSource handler $ runServerRoute request encoder decoder $ staticW wsUrl headW routeWidget
  respond $ responseLBS
    status200
    [("Content-Type", "text/html")]
    (LBS.fromStrict $ "<!doctype html>" <> bs)
