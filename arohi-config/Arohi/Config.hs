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

extractFromDOM :: A.FromJSON a => Text -> JSM a
extractFromDOM key = do
  Just doc <- currentDocument
  Just body <- getBody doc
  dataset <- getDataset body
  val <- get dataset key
  let Right b64 = (Base64.decode . LBS.fromStrict . encodeUtf8) val
      Just a = A.decode b64
  return a

toDOMVal :: A.ToJSON a => a -> LBS.ByteString
toDOMVal = Base64.encode . A.encode

configToAttrs :: A.ToJSON a => Map Text a -> Map Text LBS.ByteString
configToAttrs = fmap toDOMVal
