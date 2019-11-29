{-# LANGUAGE TypeOperators #-}
module Arohi.Client where

import Arohi.Config
import Reflex.Dom
import Text.Boomerang
-- import Text.Boomerang.String

data DataSourceConfig
  = NoDataSource
  | WsDataSource

data RouteConfig a = StringBoomerang () (a :- ())

data Config a
  = Config
  { configDataSource :: DataSourceConfig
  , configRoute :: RouteConfig a
  }
