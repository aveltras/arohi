{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Reflex.Route.Server where

import Control.Monad.Fix (MonadFix)

import qualified Data.ByteString.Char8 as C8
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Reflex.Dom hiding (Request)
import Network.URI
import Network.Wai

import Reflex.Route

runServerRoute
  :: forall t r m.
  ( DomBuilder t m
  , MonadHold t m
  , MonadFix m
  , Reflex t
  , PostBuild t m
  , Semigroup r
  , TriggerEvent t m
  , PerformEvent t m
  , Show r
  )
  => Text
  -> Request
  -> (r -> Text)
  -> (Text -> Maybe r)
  -> RouteT t r m ()
  -> m ()
runServerRoute prefix request encoder decoder widget = do
  onBuild <- getPostBuild
  let currentURI = fromMaybe nullURI $ parseURI $ (unpack prefix) <> (C8.unpack $ rawPathInfo request)
  dRoute <- holdDyn Nothing ((decoder $ (pack . uriPath) currentURI) <$ onBuild)
  let ri = RouteInfo
          { _routeInfoPrefix = prefix
          , _routeInfoCurrent = dRoute
          , _routeInfoEncoder = encoder
          , _routeInfoDecoder = decoder
          }
      manager _ = return $ constDyn $ decoder . pack . uriPath $ currentURI

  runRouteView ri widget manager

