{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
module Arohi.DataSource where

import Data.Aeson (Value, decodeStrict)
import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity(..))
import Reflex.Dom hiding (Error, Value)
    
decodeTag :: ByteString -> Maybe (Int, Value)
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
