{-# Language DeriveGeneric #-}

module Config where

import GHC.Generics as G
import qualified Data.Yaml as Y
import qualified Network.Socket as N

data Config = Config {
    urlbase :: String
  , smtpServer :: String
  , smtpPort :: N.PortNumber
  , smtpUser :: String
  , smtpPassword :: String
  , adminUsername :: String
  , adminPassword :: String
  } deriving Generic

instance Y.FromJSON Config

instance Y.FromJSON N.PortNumber
  where parseJSON v = read <$> Y.parseJSON v
-- orphan instance... but no good story here about it not being orphan.

readConfig :: IO Config
readConfig = do
   c <- Y.decodeFileEither "config.yaml"
   case c of 
     Right v -> return v
     Left e -> error $ "Cannot parse config file: " ++ show e

