{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Common.Sitemap where

import qualified Control.Category as C
import Data.Either.Combinators (rightToMaybe)
import Data.Maybe
import Data.Text
import Text.Boomerang
import Text.Boomerang.String
import Text.Boomerang.TH (makeBoomerangs)

data Sitemap
  = Homepage
  | Contact
  deriving (Eq, Show)

instance Semigroup Sitemap where
  (<>) _ b = b

$(makeBoomerangs ''Sitemap)

sitemap :: StringBoomerang () (Sitemap :- ())
sitemap =
  rHomepage
  <>  rContact C.. lit "contact"

encoder :: Sitemap -> Text
encoder = pack . (<>) "/" . fromMaybe "" . unparseString sitemap

decoder :: Text -> Maybe Sitemap
decoder = rightToMaybe . parseString sitemap . Prelude.dropWhile (== '/') . unpack
