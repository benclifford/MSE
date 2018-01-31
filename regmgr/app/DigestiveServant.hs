
module DigestiveServant where

import Data.Text as T
import Servant
import qualified Text.Digestive as DF


-- | [(String, String)] is a req body deserialisation - that might not
--   be the best type - I think servant can deserialise to various
--   forms (so potentially could deserialise directly to an Env
--   without with helper being explicit... instead the same helper
--   code would go in typeclass impl?)
servantPathEnv :: Monad m => [(String, String)] -> DF.FormEncType -> m (DF.Env m)
servantPathEnv reqBody _ = do

  let env :: Monad n => DF.Env n
      env path = do
        let pathAsString = (T.unpack . DF.fromPath) path
        let v' = lookup pathAsString reqBody -- BUG? might be multiple values? the digestive-functor docs suggest some input types (selectors?) do that somehow?
        case v' of Nothing -> return []
                   Just v -> return [DF.TextInput $ T.pack v]
        -- This case could be a maybeToList then a map T.pack?
        -- is that clearer or less clear?

  return env

