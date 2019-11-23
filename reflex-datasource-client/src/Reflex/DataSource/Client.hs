{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.DataSource.Client where

import Control.Monad.Fix (MonadFix)
import Data.Aeson (FromJSON, ToJSON, Result(..), Value, encode, fromJSON, toJSON)
import Data.ByteString.Lazy (toStrict)
import Data.Constraint.Extras (Has, has, whichever)
import Data.Constraint.Forall (ForallF)
import Data.Functor.Identity (Identity(..))
import Data.Map (toList)
import Data.Text (Text)
import Language.Javascript.JSaddle.Types (MonadJSM)
import Reflex.DataSource (WithDataSource, decodeTag)
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
  ) => Text -> WithDataSource t req m a -> m a
runSourceWS wsUrl w = mdo
  let wsConfig = def & webSocketConfig_send .~ onSend
      onSend = (fmap . fmap) (toStrict . encode) (toList <$> onRawRequests)
  ws <- webSocket wsUrl wsConfig
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
