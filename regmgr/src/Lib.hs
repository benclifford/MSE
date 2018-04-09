module Lib
    ( User, escapeDots
    ) where

import qualified Data.Text as T

type User = String

escapeDots :: T.Text -> T.Text
escapeDots = T.pack . concat . (map f) . T.unpack
  where
    f '.' = "\\\\."
    f x = [x]

