{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Reflex.Route.Client where

import Control.Monad.Fix (MonadFix)

import Data.Text (Text, pack, unpack)
import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.Window (getLocation)
import GHCJS.DOM.Types (MonadJSM, SerializedScriptValue(..))
import Language.Javascript.JSaddle (jsNull)
import Reflex.Dom
import Network.URI

import Reflex.Route

runClientRoute
  :: forall t r m.
  ( DomBuilder t m
  , MonadHold t m
  , MonadFix m
  , MonadJSM m
  , MonadJSM (Performable m)
  , Reflex t
  , PostBuild t m
  , Semigroup r
  , TriggerEvent t m
  , PerformEvent t m
  , Show r
  )
  => Text
  -> (r -> Text)
  -> (Text -> Maybe r)
  -> RouteT t r m ()
  -> m ()
runClientRoute prefix encoder decoder widget = do
  onBuild <- getPostBuild
  window <- currentWindowUnchecked
  location <- getLocation window
  currentURI <- (\uri -> uri { uriAuthority = Nothing }) <$> getLocationUri location
  dRoute <- holdDyn Nothing ((decoder $ (pack . uriPath) currentURI) <$ onBuild)
  let ri = RouteInfo
          { _routeInfoPrefix = prefix
          , _routeInfoCurrent = traceDynWith (\_mRoute -> "changeRoute") dRoute
          , _routeInfoEncoder = encoder
          , _routeInfoDecoder = decoder
          }
      manager onRoute = do
        dHistory <- manageHistory $ HistoryCommand_PushState . HistoryStateUpdate (SerializedScriptValue  jsNull) "title" . (Just . fromRoute) <$> onRoute
        return $ decoder . pack . uriPath . _historyItem_uri <$> dHistory

      fromRoute r = currentURI { uriPath = unpack $ encoder r }

  runRouteView ri widget manager

