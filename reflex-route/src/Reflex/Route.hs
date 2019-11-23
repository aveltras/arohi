{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Route where

import Control.Lens ((%~))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive
import Control.Monad.Ref
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT(..), ask, runReaderT)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Language.Javascript.JSaddle (MonadJSM)
import Reflex.Dom
import Reflex.Host.Class

newtype RouteT t r m a = RouteT { unRouteT :: ReaderT (RouteInfo t r) (EventWriterT t r m) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadHold t
    , MonadSample t
    , DomBuilder t
    , NotReady t
    , PerformEvent t
    , MonadIO
    , MonadJSM
    -- , Prerender t js
    , HasDocument
    , DomRenderHook t
    , MonadReflexCreateTrigger t
    , PostBuild t
    , TriggerEvent t
    )

instance HasJSContext m => HasJSContext (RouteT t r m) where
  type JSContextPhantom (RouteT t r m) = JSContextPhantom m
  askJSContext = lift askJSContext

-- instance (Prerender js t m, Monad m) => Prerender js t (RouteT t r m) where
--   type Client (RouteT t r m) = RouteT t r (Client m)
--   prerender server client = RouteT $ do
--     r <- ask
--     (a, onWrite) <- lift $ prerender (runRouteT server r) (runRouteT client r)
--     return a

instance HasJS x m => HasJS x (RouteT t r m) where
  type JSX (RouteT t r m) = JSX m
  liftJS = lift . liftJS

instance PrimMonad m => PrimMonad (RouteT t r m ) where
  type PrimState (RouteT t r m) = PrimState m
  primitive = lift . primitive

instance (PerformEvent t m, Prerender js t m, Monad m, Reflex t, Semigroup r) => Prerender js t (RouteT t r m) where
  type Client (RouteT t r m) = RouteT t r (Client m)
  prerender server client = RouteT $ do
    ri <- ask
    d <- lift . lift $ prerender (runRouteT server ri) (runRouteT client ri)
    let (a, r) = splitDynPure d
    lift . tellEvent $ switchPromptlyDyn r
    pure a

-- runRouteT :: (Monad m, Reflex t, Semigroup r) => RouteT t r m a -> RouteInfo t r -> m (a, Event t r)

instance MonadRef m => MonadRef (RouteT t r m) where
  type Ref (RouteT t r m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadTrans (RouteT t r) where
    lift = RouteT . lift . lift

instance Requester t m => Requester t (RouteT t r m) where
  type Request (RouteT t r m) = Request m
  type Response (RouteT t r m) = Response m
  requesting = RouteT . requesting
  requesting_ = RouteT . requesting_
    
instance (Adjustable t m, MonadHold t m, Semigroup r) => Adjustable t (RouteT t r m) where
  runWithReplace a e = RouteT $ runWithReplace (unRouteT a) (unRouteT <$> e)
  traverseDMapWithKeyWithAdjust f m e = RouteT $ traverseDMapWithKeyWithAdjust (\k v -> unRouteT $ f k v) m e
  traverseIntMapWithKeyWithAdjust f m e = RouteT $ traverseIntMapWithKeyWithAdjust (\k v -> unRouteT $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = RouteT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unRouteT $ f k v) m e

data RouteInfo t r = RouteInfo
  { _routeInfoPrefix :: Text
  , _routeInfoCurrent :: Dynamic t (Maybe r)
  , _routeInfoEncoder :: r -> Text
  , _routeInfoDecoder :: Text -> Maybe r
  }

class (Monad m) => Route t r m | m -> r, m -> t where
  
  askRoute :: m (Dynamic t (Maybe r))
  default askRoute :: (Route t r m', m ~ tx m', MonadTrans tx) => m (Dynamic t (Maybe r))
  askRoute = lift askRoute

  setRoute :: Event t r -> m ()
  default setRoute :: (Route t r m', m ~ tx m', MonadTrans tx) => Event t r -> m ()
  setRoute = lift . setRoute

  showRoute :: m (r -> Text)
  default showRoute :: (Route t r m', m ~ tx m', MonadTrans tx) => m (r -> Text)
  showRoute = lift showRoute

instance Route t r m => Route t r (BehaviorWriterT t w m)
instance Route t r m => Route t r (DynamicWriterT t w m)
instance Route t r m => Route t r (EventWriterT t w m)
instance (Route t r m, ReflexHost t, MonadTrans (PerformEventT t)) => Route t r (PerformEventT t m)
instance Route t r m => Route t r (PostBuildT t m)
instance Route t r m => Route t r (QueryT t q m)
instance Route t r m => Route t r (ReaderT r m)
instance Route t r m => Route t r (RequesterT t request response m)
instance Route t r m => Route t r (StaticDomBuilderT t m)
instance Route t r m => Route t r (TriggerEventT t m)

instance (Monad m, Reflex t, Semigroup r) => Route t r (RouteT t r m) where
  askRoute = _routeInfoCurrent <$> RouteT ask
  setRoute =  RouteT . tellEvent
  showRoute = _routeInfoEncoder <$> RouteT ask

runRouteT :: (Monad m, Reflex t, Semigroup r) => RouteT t r m a -> RouteInfo t r -> m (a, Event t r)
runRouteT (RouteT m) ri = runEventWriterT $ runReaderT m ri

linkTo :: forall t m a r. (DomBuilder t m, Route t r m) => r -> m a -> m a
linkTo r w = do
  enc <- showRoute
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (const preventDefault)
        -- & elementConfig_initialAttributes .~ "href" =: ((<>) "http://localhost:3003/" .  pack $ fromMaybe "/" (unparseString sitemap r))
        & elementConfig_initialAttributes .~ "href" =: ("http://localhost:3003" <> enc r)
  (e, a) <- element "a" cfg w
  setRoute $ r <$ domEvent Click e
  return a

runRouteView
  :: forall t m r a.
  ( TriggerEvent t m
  , MonadFix m
  , MonadHold t m
  , PostBuild t m
  , Reflex t
  , Semigroup r
  ) => RouteInfo t r -> RouteT t r m a -> (Event t r -> m (Dynamic t (Maybe r))) -> m a
runRouteView routeInfo widget locationHandler = mdo
  dRoute <- locationHandler eChangeRoute
  let routeInfoUpdated = routeInfo { _routeInfoCurrent = dRoute }
  (result, eChangeRoute) <- runRouteT widget routeInfoUpdated
  return result
