{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Frontend where

import Common.Api
import Common.Sitemap
import Control.Monad.Fix
import Data.Text
import Language.Javascript.JSaddle (JSM)
import Reflex.DataSource
import Reflex.DataSource.Client
import Reflex.Dom
import qualified Reflex.Dom.Main as Main
import Reflex.Route
import Reflex.Route.Client

entryPoint :: JSM ()
entryPoint = Main.mainWidget $ runSourceWS "ws://localhost:3004" $ runClientRoute "http://localhost:3003" encoder decoder routeWidget

headW :: (req ~ RequestG, r ~ Sitemap, MonadHold t m, MonadFix m, DomBuilder t m, PerformEvent t m, Route t r m) => m ()
headW = do
  el "title" $ text "Title2"
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "all.css") blank

routeWidget ::
  ( r ~ Sitemap
  , req ~ RequestG
  , DomBuilder t m
  , MonadHold t m
  , PostBuild t m
  , MonadFix m
  , Monad m
  , PerformEvent t m
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
