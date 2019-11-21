{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.DataSource where

import Data.Aeson
import qualified Data.ByteString as BS
import Data.Functor.Identity
import Reflex.Dom hiding (Error, Value)
    
decodeTag :: BS.ByteString -> Maybe (Int, Value)
decodeTag mValue =
  case decodeStrict mValue of
    Nothing         -> Nothing :: Maybe (Int, Value)
    Just (val, rst) -> Just (val, rst)

query
  :: (HasDataSource t req m)
  => Event t (req x) -> m (Event t x)
query req = do
  resp <- requesting req
  return $ (\(Identity b) -> b) <$> resp

type HasDataSource t req m = (Requester t m, Request m ~ req, Response m ~ Identity)
type WithDataSource t req m = RequesterT t req Identity m
