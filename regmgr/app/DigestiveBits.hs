{-# Language OverloadedStrings #-}

module DigestiveBits where

import Data.String (fromString, IsString)

import qualified Text.Digestive as DF

-- stolen from ocharles 24 days of hackage
nonEmptyString :: (IsString v, Monoid v, Monad m) => Maybe String -> DF.Form v m String
nonEmptyString def =
    (DF.check "This field must not be empty" (/= ""))
  $ DF.string def

-- | Allows a string field to exist in the form, but not be modified
--   by submission.
constString :: (IsString v, Monoid v, Monad m) => String -> DF.Form v m String
constString def =
    (DF.check "This field cannot be modified" (== def))
  $ DF.string (Just def)

-- | Allows a string field to optionally exist in the form, but not
--   be modified by submission.
constOptionalStringRead ::
 (
  Eq a, Read a, Show a,
  IsString v, Monoid v,
  Monad m
 )
  => v -> Maybe a -> DF.Form v m (Maybe a)
constOptionalStringRead descr def =
    (DF.check "This field cannot be modified" (== def))
  $ DF.optionalStringRead descr def

