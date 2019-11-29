{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Common.Api
import Common.Frontend
import Common.Sitemap
import Data.Functor.Identity (Identity(..))
import Arohi.Server

main :: IO ()
main = do
  runApp
    entryPoint
    headW
    routeWidget
    handler
    encoder
    decoder

handler :: RequestG a -> IO (Identity a)
handler = \case
  RequestG1 -> return $ Identity False
  RequestG2 i -> return $ Identity (i + 2)
