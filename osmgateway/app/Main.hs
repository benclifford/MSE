{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
module Main where

import Control.Lens
import Data.Aeson
import Data.Aeson.Types (typeMismatch, parseEither)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Traversable (for)
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

data Term = Term {
  _termid :: String,
  _termsectionid :: String,
  _termname :: String,
  _startdate :: String,
  _enddate :: String
} deriving Show

instance FromJSON Term where
  parseJSON (Object hm) = Term <$>
        hm .: "termid"
    <*> hm .: "sectionid"
    <*> hm .: "name"
    <*> hm .: "startdate"
    <*> hm .: "enddate"

  parseJSON other = typeMismatch "Term" other

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

  {-
   what is in the response body is a json object, not a list,
   mapping section ID => [Term]

   what I want is a simple table of terms, and as those terms
   contain the ID, I can probably discard the outer ID
   (or assert that it is equal to the inner one and discard it)
   -}

  let termsMap = eitherDecode (head $ r ^.. responseBody) :: Either String (HashMap String [Term])

  let
   terms :: [Term] = case termsMap of 
    Left err -> error $ "termsmap: " ++ err
    Right v -> concat (HM.elems v)

  putStrLn "terms = "
  mapM_ print terms


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
  let sectionConfigsE = eitherDecode (head $ sectionConfigsJSON ^.. responseBody) :: Either String [Section]

  sectionConfigs <- case sectionConfigsE of
    Right sectionConfigs -> do
      putStrLn "deserialised sections:"
      mapM_ print sectionConfigs
      return sectionConfigs
    Left e -> error $ "Deserialising sections: " ++ e

  for (map _sectionid sectionConfigs) $ \sectionid -> do
    putStrLn $ "Retrieving members for section id " ++ sectionid
    

  putStrLn "osmgateway finished"
