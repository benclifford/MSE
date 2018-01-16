{-# Language OverloadedStrings #-}
module Main where

import Control.Lens
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Network.Wreq

import Lib

data Secrets = Secrets {
   _userid :: String,
   _secret :: String,
   _apiId :: String,
   _token :: String
  } deriving (Read, Show)


data Section = Section {
  _sectionid :: String,
  _sectionname :: String,
  _groupid :: String,
  _groupName :: String -- denormalised wrt groupid
} deriving Show

instance FromJSON Section where
  parseJSON (Object v) = do
    sid <- v .: "sectionid"
    sname <- v .: "sectionname"
    gid <- v .: "groupid"
    gname <- v .: "groupname"
    pure $ Section sid sname gid gname
    
  parseJSON other = typeMismatch "Section" other

main :: IO ()
main = do
  putStrLn "osmgateway"


  putStrLn "authorisation secrets:"
  secrets <- read <$> readFile "secrets.dat" :: IO Secrets
  print secrets


  -- this gives a list of terms, but that's not what I want
  -- ... I want section info. I can grab section IDs out of
  -- this at least, to iterate over.

  putStrLn "osmgateway: getting list of terms"
  
  let url = "https://www.onlinescoutmanager.co.uk/api.php"

  let opts = defaults & param "action" .~ ["getTerms"]

  let postData = [ "userid" := _userid secrets
                 , "secret" := _secret secrets  
                 , "apiid" := _apiId secrets
                 , "token" := _token secrets
                 ]

{-
                 , "password" := password
                 , "email" := email
                 ]
-}

  r <- postWith opts url postData

  print r

  print $ r ^.. responseBody

  putStrLn "osmgateway: getting user roles"
  
  let url = "https://www.onlinescoutmanager.co.uk/api.php"

  let opts = defaults & param "action" .~ ["getUserRoles"]

  let postData = [ "userid" := _userid secrets
                 , "secret" := _secret secrets  
                 , "apiid" := _apiId secrets
                 , "token" := _token secrets
                 ]

{-
                 , "password" := password
                 , "email" := email
                 ]
-}

  sectionConfigsJSON <- postWith opts url postData

  print sectionConfigsJSON

  print $ sectionConfigsJSON ^.. responseBody

  -- BUG: head here is discarding potentially some other stuff
  -- (but what in this case?)
  let sectionConfigs = eitherDecode (head $ sectionConfigsJSON ^.. responseBody) :: Either String [Section]

  putStrLn "deserialised sections:"
  print sectionConfigs

  putStrLn "osmgateway finished"
