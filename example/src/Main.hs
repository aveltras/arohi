{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import qualified Control.Category as C
import Control.Monad.Fix
-- import Control.Monad.Trans.Class (lift)
import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import Data.Either.Combinators (rightToMaybe)
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import Data.Text
import Reflex.Dom
import qualified Reflex.Dom.Main as Main
import Network.HTTP.Types
import Network.Wai
import qualified Data.ByteString.Lazy as LBS

import Text.Boomerang
import Text.Boomerang.String
import Text.Boomerang.TH    (makeBoomerangs)

import Reflex.DataSource
import Reflex.DataSource.Client
import Reflex.DataSource.Server
import Reflex.DevServer
import Reflex.Route
import Reflex.Route.Client
import Reflex.Route.Server

-- #if defined(MIN_VERSION_reflex-devserver)
-- #endif

data Sitemap
  = Homepage
  | Contact
  deriving (Eq, Show)

instance Semigroup Sitemap where
  (<>) _ b = b

$(makeBoomerangs ''Sitemap)

sitemap :: StringBoomerang () (Sitemap :- ())
sitemap =
      rHomepage
    <>  rContact C.. lit "contact"

main :: IO ()
main = do
  -- devServer 3003 3004 (Just "../ghcid.reload") (Main.mainWidget $ runSourceWS "ws://localhost:3004" widget) app (wsApp handler)
  devServer 3003 3004 (Just "../ghcid.reload") (Main.mainWidget $ runSourceWS "ws://localhost:3004" $ runClientRoute "http://localhost:3003" enc dec routeWidget) app (wsApp handler)
  where
    enc = pack . (<>) "/" . fromMaybe "" . unparseString sitemap
    dec = rightToMaybe . parseString sitemap . Prelude.dropWhile (== '/') . unpack

data RequestG :: * -> * where
  RequestG1 :: RequestG Bool
  RequestG2 :: Int -> RequestG Int

routeWidget ::
  ( r ~ Sitemap
  , req ~ RequestG
  , DomBuilder t m
  , MonadHold t m
  , PostBuild t m
  , MonadFix m
  , Monad m
  -- , Prerender js t m
  , PerformEvent t m
  -- , Prerender js t m
  , HasDataSource t req m
  , Route t r m
  ) => m ()
routeWidget = do
  _onBuild <- getPostBuild
  dRoute <- askRoute
  linkTo Homepage $ text "Home"
  linkTo Contact $ text "Contact"

  let _widget =
        ffor dRoute $ \case
          (Just Homepage) -> do
            el "h1" $ text "Home"
            linkTo Contact $ text "Contact"
            widget
          (Just Contact) -> do
            el "h1" $ text "Contact"
            linkTo Homepage $ text "Home"
          Nothing -> el "h1" $ text "404"
  _onWidget <- dyn _widget
  
  blank


widget ::
  ( req ~ RequestG
  , r ~ Sitemap
  , DomBuilder t m
  , Monad m
  , PostBuild t m
  , MonadHold t m
  , MonadFix m
  , HasDataSource t req m
  , PerformEvent t m
  , Route t r m
  ) => m ()
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
app request respond = do
  (_, bs) <- renderStatic $ runIOSource handler $ runServerRoute "http://localhost:3003" request enc dec $ staticW headW routeWidget
  respond $ responseLBS
    status200
    [("Content-Type", "text/html")]
    (LBS.fromStrict bs)
  where
    enc = pack . (<>) "/" . fromMaybe "" . unparseString sitemap
    dec = rightToMaybe . parseString sitemap . Prelude.dropWhile (== '/') . unpack

headW :: (req ~ RequestG, r ~ Sitemap, MonadHold t m, MonadFix m, DomBuilder t m, PerformEvent t m, Route t r m) => m ()
headW = do
  el "title" $ text "Title2"
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "all.css") blank

staticW :: -- forall t r req m js.
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
  el "html" $ do
    el "head" $ do
      hW
      elAttr "script" ("src" =: "jsaddle.js") blank
    el "body" $ do
      ePb <- getPostBuild
      eResp <- query ((RequestG1) <$ ePb)
      _ <- widgetHold (text "Waiting for Loading") ((\(b2) -> text ("Length (prerender) is: " <> (pack . show $ b2))) <$> eResp)
      bodyW

handler :: RequestG a -> IO (Identity a)
handler = \case
  RequestG1 -> return $ Identity False
  RequestG2 i -> return $ Identity (i + 2)

deriveJSONGADT ''RequestG
deriveArgDict ''RequestG
