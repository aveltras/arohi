{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Arohi.Config where

import qualified Data.Aeson as A
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.Document (getBody)
import GHCJS.DOM.DOMStringMap (get)
import GHCJS.DOM.HTMLElement (getDataset)
import GHCJS.DOM.Types (JSM)

class A.FromJSON a => FromDOM a where
  fromDOM :: Text -> JSM a
  default fromDOM :: Text -> JSM a
  fromDOM key =
    do Just doc <- currentDocument
       Just body <- getBody doc
       dataset <- getDataset body
       val <- get dataset key
       let Right b64 = (Base64.decode . LBS.fromStrict . encodeUtf8) val
           Just a = A.decode b64
       return a

class A.ToJSON a => ToDOM a where
  toDOM :: a -> LBS.ByteString
  default toDOM :: a -> LBS.ByteString
  toDOM = Base64.encode . A.encode


configToAttrs :: ToDOM a => Map Text a -> Map Text LBS.ByteString
configToAttrs = fmap toDOM
