{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
module Main where


import Control.Lens
import Data.Aeson
import Data.Aeson.Types (typeMismatch, parseEither)
import Data.HashMap.Lazy (HashMap)
import Data.Text (pack)
import qualified Data.HashMap.Lazy as HM
import Data.Traversable (for)
import GHC.Generics
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

-- | the format of data coming back from /ext/members/contacts
data ExtMembersContacts = ExtMembersContacts {
    _emcitems :: [EMCMember]
  } deriving (Show)

instance FromJSON ExtMembersContacts where
  parseJSON (Object hm) =
    ExtMembersContacts <$>
      hm .: "items"


-- we'll just grab summary stuff here - further information
-- can come from doing a lookup based on the scoutid
data EMCMember = EMCMember {
    _emcfirstname :: String
  , _emclastname :: String
  , _emcscoutid :: ScoutID
  } deriving Show

instance FromJSON EMCMember where
  parseJSON (Object hm) =
    EMCMember <$> hm .: "firstname"
              <*> hm .: "lastname"
              <*> (ScoutID <$> hm .: "scoutid")


data EMCGetIndividual = EMCGetIndividual {
    _data :: EMCGetIndividualData
  } deriving Show

instance FromJSON EMCGetIndividual where
  parseJSON (Object hm) =
   EMCGetIndividual <$> hm .: "data"

-- although there are a lot of fields in the JSON
-- for this, in the case of Merrow they're mostly
-- empty (which may or may be the case for OSM
-- in general?). There is additional data provided in
-- the getData response.
data EMCGetIndividualData = EMCGetIndividualData {
    _firstname :: String
  , _lastname :: String
  , _dob :: String
  } deriving Show

instance FromJSON EMCGetIndividualData where
  parseJSON (Object hm) =
    EMCGetIndividualData <$> hm .: "firstname"
                         <*> hm .: "lastname"
                         <*> hm .: "dob"

newtype ScoutID = ScoutID Integer
  deriving Show

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

  for sectionConfigs $ \section -> do
    let sectionid = _sectionid section
    putStrLn $ "Retrieving members for section id " ++ sectionid ++ " in relevant term(s)"
    putStrLn $ "Section name: " ++ _sectionname section

    -- we'll pick terms for this section that match a particular
    -- rule - in this case, spring 2018 so that we get scouts that
    -- are in the section near the start of 2018.
    let selectedTerms = filter (\t -> _termsectionid t == sectionid
                                   && _termname t == "Spring 2018")
                               terms

    putStrLn $ "Selected terms: "
    mapM_ (print . _termname) selectedTerms
    -- TODO: what is a relevant term? any term labelled "Spring 2018" ?
    -- Also, given a list of terms we don't actually need to loop over
    -- section ID because we know the section ID from within the Term
    -- object. The section list is useful for human readable display of
    -- section name though.

    for selectedTerms $ \term -> do
      putStrLn $ "process term " ++ (_termid term)
      -- assert denormalised consistency check:
      -- _termsectionid term == _sectionid section

   -- from web form:
   -- https://www.onlinescoutmanager.co.uk/ext/members/contact/?action=getListOfMembers&sort=dob&sectionid=3940&termid=194039&section=scouts 
   -- but looks like we can skip the sort and section titles.

      -- QUESTION is this really the right URL for getting members details?
      -- it doesn't look like 'api.php' at all.
      -- It needs a / on the end too (presumably to hit something like
      -- an index.php)
      -- let url = "https://www.onlinescoutmanager.co.uk/api.php"
      -- let url = "https://www.onlinescoutmanager.co.uk/ext/members/contact"
      let url = "https://www.onlinescoutmanager.co.uk/ext/members/contact/"

      let opts = defaults & param "action" .~ ["getListOfMembers"]
                          & param "termid" .~ [pack $ _termid term]
                          & param "sectionid" .~ [pack $ _sectionid section]

      let postData = [ "userid" := _userid secrets
                     , "secret" := _secret secrets  
                     , "apiid" := _apiId secrets
                     , "token" := _token secrets
{- QUESTION/DISCUSSION: what an awkward separation of parameters
   between URL and post data 
                     , "termid" := _termid term
                     , "sectionid" := _sectionid section
-}
                     ]

      r <- postWith opts url postData

      print r

      print $ r ^.. responseBody

      let ovE :: Either String ExtMembersContacts = eitherDecode $ head $ r ^.. responseBody

      case ovE of
        Left err -> error $ "Decoding response from ext/members/contact/ - " ++ err
        Right obs -> do 
          putStrLn "members/contact object decoded"
          print obs
          let items = _emcitems obs
          mapM_ print items
          let scoutids = map _emcscoutid items
          putStrLn "Scoutids for this section:"
          print scoutids

          for scoutids $ \scoutid -> do
            putStrLn $ "Retrieving individual details for: " ++ show scoutid
            -- from web API:
            -- https://www.onlinescoutmanager.co.uk/ext/members/contact/?action=getIndividual&sectionid=3943&scoutid=1004107&termid=194058&context=members
            -- section and term, although seemingly they wouldn't be needed, are mandatory. context is not. so this works:
            -- https://www.onlinescoutmanager.co.uk/ext/members/contact/?action=getIndividual&sectionid=3943&scoutid=1004107&termid=194058

            -- which means we can only get the information for a member in the context of a Term (the section ID is implied by and contained in the Term) - the data is not identical across sections/terms, although hopefully it is pretty consistent.
            -- note that there is an "Others" text field listing other sections that the person is associated with - for example, in an Adults term, I am also listed as "1st Merrow: Scouts"



            let url = "https://www.onlinescoutmanager.co.uk/ext/members/contact/"

            let (ScoutID scoutid_num) = scoutid
            let opts = defaults & param "action" .~ ["getIndividual"]
                          & param "termid" .~ [pack $ _termid term]
                          & param "sectionid" .~ [pack $ _sectionid section]
                          & param "scoutid" .~ [pack $ show $ scoutid_num]

            let postData = [ "userid" := _userid secrets
                           , "secret" := _secret secrets  
                           , "apiid" := _apiId secrets
                           , "token" := _token secrets
                     ]

            r <- postWith opts url postData

            print r

            print $ r ^.. responseBody

            let ovE :: Either String EMCGetIndividual = eitherDecode $ head $ r ^.. responseBody

            case ovE of
              Left err -> error $ "Decoding individual response from ext/members/contact/ - " ++ err
              Right obs -> do 
                putStrLn "members/contact object decoded"
                putStrLn "Decoded individual response:"
                print obs 

            putStrLn $ "Requesting full data for " ++ show scoutid 

            -- section goes into the URL, but scoutid goes into the
            -- post fields...
            -- https://www.onlinescoutmanager.co.uk/ext/customdata/?action=getData&section_id=3940
            let url = "https://www.onlinescoutmanager.co.uk/ext/customdata/"

            let (ScoutID scoutid_num) = scoutid
            let opts = defaults & param "action" .~ ["getData"]
                          & param "section_id" .~ [pack $ _sectionid section]

            let postData = [ "userid" := _userid secrets
                           , "secret" := _secret secrets  
                           , "apiid" := _apiId secrets
                           , "token" := _token secrets
                           , "associated_id" := (show $ scoutid_num)
                           , "associated_type" := ("member" :: String)
                     ]

            r <- postWith opts url postData

            print r

            print $ r ^.. responseBody


          pure ()

  putStrLn "osmgateway finished"

