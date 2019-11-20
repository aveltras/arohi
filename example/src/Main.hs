{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import Data.Text
import Reflex.Dom

import Reflex.DataSource

main :: IO ()
main = do
  mainWidget $ runSourceWS widget

data RequestG :: * -> * where
  RequestG1 :: RequestG Bool
  RequestG2 :: Int -> RequestG Text

widget :: (req ~ RequestG, DomBuilder t m, Monad m, HasDataSource t req m) => m ()
widget = do
  el "h1" $ text "title"
  onClick <- button "click"
  _onResponse <- query $ RequestG2 2 <$ onClick
  blank

deriveJSONGADT ''RequestG
deriveArgDict ''RequestG
