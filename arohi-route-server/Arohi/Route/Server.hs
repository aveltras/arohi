{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Arohi.Route.Server where

import Control.Monad.Fix (MonadFix)
import qualified Data.ByteString.Char8 as C8 (unpack)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Network.URI (URI(..), nullURI, parseURI)
import Network.Wai (Request, rawPathInfo, requestHeaderHost)
import Reflex.Dom hiding (Request)
import Arohi.Route (RouteInfo(..), RouteT, runRouteView)

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
  => Request
  -> (r -> Text)
  -> (Text -> Maybe r)
  -> RouteT t r m ()
  -> m ()
runServerRoute request encoder decoder widget = do
  onBuild <- getPostBuild
  let prefix = (<>) "//" $ fromMaybe "" $ requestHeaderHost request
      currentURI = fromMaybe nullURI $ parseURI $ C8.unpack prefix <> C8.unpack (rawPathInfo request)
  dRoute <- holdDyn Nothing (decoder ((pack . uriPath) currentURI) <$ onBuild)
  let ri = RouteInfo
          { _routeInfoPrefix = decodeUtf8 prefix
          , _routeInfoCurrent = dRoute
          , _routeInfoEncoder = encoder
          , _routeInfoDecoder = decoder
          }
      manager _ = return $ constDyn $ decoder . pack . uriPath $ currentURI

  runRouteView ri widget manager

