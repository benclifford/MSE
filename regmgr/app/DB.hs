{-# Language OverloadedStrings #-}

-- | Abstracted database handling
module DB where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.PostgreSQL.Simple as PG

withDB :: MonadIO m => (PG.Connection -> IO a) -> m a
withDB act = liftIO $ do
  putStrLn "withDB: opening db"

  bracket  
    open
    close
    act
 where
   open = PG.connectPostgreSQL "user='postgres'"
   close conn = do
     putStrLn "withDB: closing db"
     PG.close conn

