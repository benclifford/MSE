{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
module Main where

import Control.Monad (when)
import Control.Lens
import Data.Aeson
import Data.Aeson.Types (typeMismatch, parseEither)
import Data.HashMap.Lazy (HashMap)
import Data.Text (unpack, pack)
import qualified Data.HashMap.Lazy as HM
import Data.Traversable (for)
import Database.PostgreSQL.Simple
import GHC.Generics
import Network.Wreq as WReq
import Network.Wreq.Types (Postable)
import qualified System.Environment as Env

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


-- | data returned for a member from getData
data ExtraData = ExtraData {
  _groups :: [ExtraDataGroup]
} deriving Show

instance FromJSON ExtraData where
  parseJSON (Object hm) = ExtraData <$> hm .: "data"

data ExtraDataGroup = ExtraDataGroup {
    _groupID :: Integer
  , _identifier :: String
  , _name :: String
  , _columns :: [ExtraDataGroupColumn]
} deriving Show

instance FromJSON ExtraDataGroup where
  parseJSON (Object hm) = 
    ExtraDataGroup
    <$> hm .: "group_id"
    <*> hm .: "identifier"
    <*> hm .: "name"
    <*> hm .: "columns"

data ExtraDataGroupColumn = ExtraDataGroupColumn {
    _column_id :: Integer
  , _varname :: String
  , _label :: String
  , _value :: String
} deriving Show

instance FromJSON ExtraDataGroupColumn where
  parseJSON (Object hm) =
   ExtraDataGroupColumn
   <$> hm .: "column_id"
   <*> hm .: "varname"
   <*> hm .: "label"
   <*> hm .: "value"

data EventAttendeeList = EventAttendeeList {
    _eaeventid :: String
  , _eaitems :: [EventAttendee]
  } deriving Show

instance FromJSON EventAttendeeList where
  parseJSON (Object o) =
    EventAttendeeList
    <$> o .: "eventid"
    <*> o .: "items"

data EventAttendee = EventAttendee {
    _eascoutid :: ScoutID
  , _eaattending :: String
  } deriving Show

instance FromJSON EventAttendee where
  parseJSON (Object o) =
    EventAttendee
    <$> o .: "scoutid"
    <*> o .: "attending"


newtype ScoutID = ScoutID { _scoutid :: Integer }
  deriving Show

instance FromJSON ScoutID where
  parseJSON (String t) = return (ScoutID (read (unpack t)))
  parseJSON v = ScoutID <$> (parseJSON v)

secretPostData secrets = 
  [ "userid" := _userid secrets
  , "secret" := _secret secrets  
  , "apiid" := _apiId secrets
  , "token" := _token secrets
  ]

getTerms :: Secrets -> IO [Term]
getTerms secrets = do
  putStrLn "osmgateway: getting list of terms"
  
  let url = "https://www.onlinescoutmanager.co.uk/api.php"

  let opts = defaults & param "action" .~ ["getTerms"]

  let postData = secretPostData secrets

  v :: HashMap String [Term] <- postWithResponse "terms" opts url postData
  let terms = concat (HM.elems v)
  return terms

getSectionConfigs :: Secrets -> IO [Section]
getSectionConfigs secrets = do

  putStrLn "osmgateway: getting sections this user has a role in"
  
  let url = "https://www.onlinescoutmanager.co.uk/api.php"

  let opts = defaults & param "action" .~ ["getUserRoles"]

  let postData = secretPostData secrets

  sectionConfigs :: [Section] <- postWithResponse "sectionConfigs" opts url postData

  return sectionConfigs

getExtMembersContacts :: Secrets -> Term -> Section -> IO ExtMembersContacts
getExtMembersContacts secrets term section = do
  -- QUESTION is this really the right URL for getting members details?
  -- it doesn't look like 'api.php' at all.
  -- It needs a / on the end too (presumably to hit something like
  -- an index.php)
  -- let url = "https://www.onlinescoutmanager.co.uk/api.php"
  -- let url = "https://www.onlinescoutmanager.co.uk/ext/members/contact"
  let url = "https://www.onlinescoutmanager.co.uk/ext/members/contact/"

{- QUESTION/DISCUSSION: what an awkward separation of parameters
   between URL and post data 
-}
  let opts = defaults & param "action" .~ ["getListOfMembers"]
                      & param "termid" .~ [pack $ _termid term]
                      & param "sectionid" .~ [pack $ _sectionid section]

  let postData = secretPostData secrets

  emc :: ExtMembersContacts <- postWithResponse "ExtMembersContacts" opts url postData

  return emc


getEMCIndividual :: Secrets -> Term -> Section -> ScoutID -> IO EMCGetIndividual
getEMCIndividual secrets term section scoutid = do
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

  let postData = secretPostData secrets

  individual :: EMCGetIndividual <- postWithResponse "EMCGetIndividual" opts url postData
  return individual

getExtraData :: Secrets -> Section -> ScoutID -> IO ExtraData
getExtraData secrets section scoutid = do

  putStrLn $ "Requesting full data for " ++ show scoutid 

  -- section goes into the URL, but scoutid goes into the
  -- post fields...
  -- https://www.onlinescoutmanager.co.uk/ext/customdata/?action=getData&section_id=3940
  let url = "https://www.onlinescoutmanager.co.uk/ext/customdata/"

  let (ScoutID scoutid_num) = scoutid
  let opts = defaults & param "action" .~ ["getData"]
                      & param "section_id" .~ [pack $ _sectionid section]

  let postData = secretPostData secrets
              ++ [ "associated_id" := (show $ scoutid_num)
                 , "associated_type" := ("member" :: String)
                 ]


  extraData :: ExtraData <- postWithResponse "extradata" opts url postData
  putStrLn "End of extradata block"

  return extraData

getEventAttendeeList :: Secrets -> String -> String -> String -> IO EventAttendeeList
getEventAttendeeList secrets sectionid eventid termid = do
  putStrLn "osmgateway: getting scout camp event"

  let url = "https://www.onlinescoutmanager.co.uk/ext/events/event/?action=getAttendance&eventid=" ++ eventid ++ "&sectionid=" ++ sectionid ++ "&termid="++termid

  let postData = secretPostData secrets

  let opts = defaults

  v :: EventAttendeeList <- postWithResponse "event attendee list" opts url postData

  return v


main :: IO ()
main = do
  putStrLn "osmgateway"

  putStrLn "connecting to db"
  conn <- connectPostgreSQL "user='postgres'"

  putStrLn "authorisation secrets:"
  secrets <- read <$> readFile "secrets.dat" :: IO Secrets
  print secrets

  args <- Env.getArgs

  when (args == [])
       (importPeople secrets conn)

  importEventAttendees secrets conn

  putStrLn "Closing database"
  close conn
  putStrLn "osmgateway finished"

importPeople :: Secrets -> Connection -> IO ()
importPeople secrets conn = do

  terms <- getTerms secrets

  putStrLn "terms = "
  mapM_ print terms

  sectionConfigs <- getSectionConfigs secrets

  for sectionConfigs $ \section -> do
    let sectionid = _sectionid section

    execute conn "insert into osm_sections (sectionid, sectionname) values (?,?)"
      (
        sectionid
      , _sectionname section
      ) 
    putStrLn "Section has been written to database."

    putStrLn $ "Retrieving members for section id " ++ sectionid ++ " in relevant term(s)"
    putStrLn $ "Section name: " ++ _sectionname section

    -- we'll pick terms for this section that match a particular
    -- rule - in this case, summer 2018 so that we get scouts that
    -- are in the section near the middle of 2018.
    let selectedTerms = filter (\t -> _termsectionid t == sectionid
                                   && _termname t == "Summer 2018")
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


      obs' <- getExtMembersContacts secrets term section

      case obs' of -- TODO: vacuuous case
         obs -> do 
          putStrLn "members/contact object decoded"
          print obs
          let items = _emcitems obs
          mapM_ print items
          let scoutids = map _emcscoutid items
          putStrLn "Scoutids for this section:"
          print scoutids

          for scoutids $ \scoutid -> do
            individual <- getEMCIndividual secrets term section scoutid

            putStrLn "members/contact object decoded"
            putStrLn "Decoded individual response:"
            print individual

            extraData <- getExtraData secrets section scoutid
            -- at this point we have a chunk of data about an individual
            -- that we can stick in an SQL database.

            execute conn "insert into osm_individuals (scoutid, firstname, lastname, dob) values (?, ?, ?, ?)" 
              (
                _scoutid scoutid
              , (_firstname . _data) individual
              , (_lastname . _data) individual
              , (_dob . _data) individual
              ) 
            putStrLn "Individual has been written to database."

            execute conn "insert into osm_individual_section (scoutid, sectionid) values (?,?)" (_scoutid scoutid, sectionid)

            putStrLn "Individual section membership has been written to the database"

            for (_groups extraData) $ \extraDataGroup -> do
              putStrLn $ "Processing an extra data group _groupID: " ++ show (_groupID extraDataGroup)
              putStrLn $ "                            _identifier: " ++ _identifier extraDataGroup
              putStrLn $ "                                  _name: " ++ _name extraDataGroup

              for (_columns extraDataGroup) $ \column -> do
  {-
    _column_id :: Integer
  , _varname :: String
  , _label :: String
  , _value :: String
-}
                execute conn "insert into osm_extradata (scoutid, groupid, group_text_identifier, columnid, varname, label, value) values (?, ?, ?, ?, ?, ?, ?)"
                  (_scoutid scoutid,
                   _groupID extraDataGroup,
                   _identifier extraDataGroup,
                   _column_id column,
                   _varname column,
                   _label column,
                   _value column
                  )
          pure ()

  pure ()

importEventAttendees :: Secrets -> Connection -> IO ()
importEventAttendees secrets conn = do

  for [("3940","381972","194040")] $ \(section,event,term) -> do

    v <- getEventAttendeeList secrets section event term

    print v

    for (_eaitems v) $ \attendee -> do
      putStrLn $ "Scout " ++ (show $ _eascoutid attendee) ++ " is listed on " ++ (_eaeventid v) ++ " as: " ++ (_eaattending attendee)

      execute conn "insert into osm_event_attendee (eventid, scoutid, attending) values (?,?,?)"
        (
          _eaeventid v
        , _scoutid $ _eascoutid attendee
        , _eaattending attendee
        ) 
      putStrLn "Attendee has been written to database."

  pure ()


postWithResponse :: 
  (Show resp, FromJSON resp,
   Postable postable)
  => String -> WReq.Options -> String -> postable -> IO resp
postWithResponse errname opts url postData = do
  r <- postWith opts url postData

  -- useful for debugging: print r

  let bodyL = r ^.. responseBody -- this is a list but I'm going to assume it only has one element BUG
  let valE = eitherDecode (head bodyL)
  case valE of
    Left err -> error $ "postWithResponse: parsing " ++ errname ++ ": " ++ err
    Right val -> return val
