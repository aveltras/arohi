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
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Reflex.Dom

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
    , PostBuild t
    )

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
