{-# Language DeriveGeneric #-}

module Config where

import qualified Data.HashMap.Lazy as HML
import qualified Data.Maybe as M
import Data.String (IsString, fromString)
import qualified Data.Text as T
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

readLabels :: (Y.FromJSON k, Y.FromJSON v, IsString k, IsString v)
           => IO [(k, v)]
readLabels = do
  c <- Y.decodeFileEither "english.yaml"
  case c of
    -- this seems unnecessarily complex for the end result
    Right (Y.Object o) ->
      return
        $ map (\(a,b) -> (fromString $ T.unpack a, M.fromJust $ Y.parseMaybe Y.parseJSON b))
        $ HML.toList o
      -- o is HashMap Text Value
    Right v -> error $ "Expected an Aeson Object but got " ++ show v
    Left e -> error $ "Cannot parse english language literals file: " ++ show e
