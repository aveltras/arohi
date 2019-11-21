{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.DataSource.Client where

import Control.Monad.Fix
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Constraint.Extras
import Data.Constraint.Forall
import Data.Functor.Identity
import Data.Map
import Language.Javascript.JSaddle.Types (MonadJSM)
import Reflex.DataSource
import Reflex.Dom hiding (Error, Value)

runSourceWS :: forall t m req a.
  ( MonadFix m
  , MonadHold t m
  , MonadJSM m
  , MonadJSM (Performable m)
  , HasJSContext m
  , PerformEvent t m
  , PostBuild t m
  , TriggerEvent t m
  , ForallF ToJSON req
  , Has FromJSON req
  ) => WithDataSource t req m a -> m a
runSourceWS w = mdo
  let wsConfig = def & webSocketConfig_send .~ onSend
      onSend = (fmap . fmap) (LBS.toStrict . encode) (toList <$> onRawRequests) :: Event t [BS.ByteString]
  ws <- webSocket "wsUrl" wsConfig
  (result, onRequest) <- runRequesterT w onResponse
  (onRawRequests, onResponse) <- matchResponsesWithRequests codec onRequest (fmapMaybe decodeTag (_webSocket_recv ws))
  return result

  where

    codec :: forall b. req b -> (Value, Value -> Identity b)
    codec request = (whichever @ToJSON @req @b toJSON request, f)
      where
        f val = do
          let result = has @FromJSON request fromJSON val
          case result of
            Error _s -> error "boom" -- TODO handle this
            Success a -> Identity a
