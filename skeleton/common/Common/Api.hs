{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Api where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH

data RequestG :: * -> * where
  RequestG1 :: RequestG Bool
  RequestG2 :: Int -> RequestG Int

deriveJSONGADT ''RequestG
deriveArgDict ''RequestG
