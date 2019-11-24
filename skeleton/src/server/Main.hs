{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Common.Api
import Common.Frontend
import Common.Sitemap
import Control.Monad.Fix
import qualified Data.ByteString.Lazy as LBS
import Data.Functor.Identity (Identity(..))
import Network.HTTP.Types
import Network.Wai
import Reflex.DataSource
import Reflex.DataSource.Server
import Reflex.DevServer
import Reflex.Dom
import Reflex.Route
import Reflex.Route.Server

main :: IO ()
main = do
  devServer 3003 3004 (Just "../ghcid.reload") entryPoint app (wsApp handler)

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
  ) => m () -> m () -> m ()
staticW hW bodyW = do
  prefix <- askPrefix
  el "html" $ do
    el "head" $ do
      hW
      elAttr "script" ("src" =: "jsaddle.js") blank
    elAttr "body" ("data-ws" =: "test" <> "data-prefix" =: prefix) $ do
      bodyW

app :: Application
app request respond = do
  (_, bs) <- renderStatic $ runIOSource handler $ runServerRoute request encoder decoder $ staticW headW routeWidget
  respond $ responseLBS
    status200
    [("Content-Type", "text/html")]
    (LBS.fromStrict bs)
