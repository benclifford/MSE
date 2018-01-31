{-# Language OverloadedStrings #-}

module DigestiveBits where

import Data.String (fromString, IsString)

import qualified Text.Digestive as DF

-- stolen from ocharles 24 days of hackage
nonEmptyString :: (IsString v, Monoid v, Monad m) => Maybe String -> DF.Form v m String
nonEmptyString def =
    (DF.check "This field must not be empty" (/= ""))
  $ DF.string def


