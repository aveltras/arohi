{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Monad.Fix
import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import Data.Functor.Identity
import Data.Text
import Reflex.Dom
import qualified Reflex.Dom.Main as Main
import Network.HTTP.Types
import Network.Wai
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Reflex.DataSource
import Reflex.DataSource.Client
import Reflex.DataSource.Server
import Reflex.DevServer

-- #if defined(MIN_VERSION_reflex-devserver)
-- #endif

main :: IO ()
main = do
  devServer 3003 3004 (Just "../ghcid.reload") (Main.mainWidget $ runSourceWS "ws://localhost:3004" widget) app (wsApp handler)

data RequestG :: * -> * where
  RequestG1 :: RequestG Bool
  RequestG2 :: Int -> RequestG Int

widget :: (req ~ RequestG, DomBuilder t m, Monad m, PostBuild t m, MonadHold t m, MonadFix m, HasDataSource t req m) => m ()
widget = do
  el "h1" $ text "title"
  onClick <- button "click"
  dCount <- foldDyn ($) (0 :: Int) ((+1) <$ onClick)
  onResponse <- query $ RequestG2 2 <$ onClick
  el "p" $ dynText $ pack . show <$> dCount
  dCountBis <- foldDyn ($) (0 :: Int) ((+) <$> onResponse)
  el "p" $ dynText $ pack . show <$> dCountBis
  blank

app :: Application
app _req respond = do
  bs <- renderFrontend $ runIOSource handler $ staticW headW widget
  respond $ responseLBS
    status200
    [("Content-Type", "text/html")]
    (LBS.fromStrict bs)

headW :: DomBuilder t m => m ()
headW = do
  el "title" $ text "Title2"
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "all.css") blank

staticW :: (req ~ RequestG, DomBuilder t m, MonadHold t m, MonadFix m, PerformEvent t m, PostBuild t m, Prerender js t m, TriggerEvent t m, HasDataSource t req m) => m () -> m () -> m ()
staticW hW bodyW = do
  el "html" $ do
    el "head" $ do
      hW
      elAttr "script" ("src" =: "jsaddle.js") blank
    el "body" $ do
      ePb <- getPostBuild
      eResp <- query ((RequestG1) <$ ePb)
      _ <- widgetHold (text "Waiting for Loading") ((\(b2) -> text ("Length (prerender) is: " <> (pack . show $ b2))) <$> eResp)
      bodyW

renderFrontend ::
  ( t ~ DomTimeline
  , w ~ PostBuildT t (StaticDomBuilderT t (PerformEventT t DomHost))
  ) => w () -> IO BS.ByteString
renderFrontend w = do 
  (_, bs) <- renderStatic w 
  return bs

handler :: RequestG a -> IO (Identity a)
handler = \case
  RequestG1 -> return $ Identity False
  RequestG2 int -> return $ Identity (int + 2)

deriveJSONGADT ''RequestG
deriveArgDict ''RequestG
