{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language MultiWayIf #-}

module Main where

import Data.List (intersperse)

import Data.Maybe (maybeToList)
import Data.Monoid ( (<>) )
import qualified Data.Text as T
import Data.Traversable (for)
import Data.UUID as UUID
import Data.UUID.V4 as UUID

import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple
import qualified Database.PostgreSQL.Simple.Time as PGT

import DB

main :: IO ()
main = do
  putStrLn "regmgr-from-osm: start"

  withDB process

  putStrLn "regmgr-from-osm: done"

process conn = do
  -- TODO: get a list of everyone registered for this event in OSM
  -- for each one, see if they have a registration record in regmgr
  -- if they do, skip
  -- if they do not, create

  scoutids :: [[Integer]] <- PG.query conn "SELECT scoutid FROM osm_event_attendee WHERE eventid = 323383 AND attending = 'Yes'" ()

  putStrLn $ "There are " ++ show (length scoutids) ++ " attendees who have said yes in OSM"

  for scoutids $ \[scoutid] -> do
    putStrLn $ "Checking scoutid " ++ show scoutid

    rows :: [[Integer]] <- PG.query conn "SELECT osm_scoutid FROM regmgr_attendee WHERE osm_scoutid = ?" [scoutid]

    case length rows of
      0 -> addReg conn scoutid

      1 -> putStrLn "Already in regmgr database - not taking any action"
      n -> error $ "There are " ++ show n ++ " rows for this scout id, which is more than the expected 0 or 1"

addReg conn scoutid = do
  putStrLn $ "Adding new registrant " ++ show scoutid
  reg :: [(String, String, String)] <- PG.query conn "SELECT DISTINCT firstname, lastname, dob FROM osm_individuals WHERE scoutid = ?" [scoutid]

  case reg of 
    [(fn, ln, dob)] -> do
      putStrLn $ "Name: " ++ ln ++ "/" ++ fn
      putStrLn $ "DOB: " ++ dob

      uuid <- UUID.nextRandom
      let auth = UUID.toString uuid

      newDBTime <- dbNow conn

      registrant_address <- getExtraDataAddress conn scoutid "contact_primary_member"
      registrant_telephone <- getExtraDataPhone conn scoutid "contact_primary_member"

      ec_1_address <- getExtraDataAddress conn scoutid "emergency"
      ec_1_telephone <- getExtraDataPhone conn scoutid "emergency"
      ec_1_name <- getExtraDataFullname conn scoutid "emergency"

      doctor_fullname <- getExtraDataFullname conn scoutid "doctor"
      doctor_surgery <- getExtraDataField conn scoutid "doctor" "surgery"
      let doctor_name = commaSeparatedConcat [doctor_fullname, doctor_surgery]
      doctor_address <- getExtraDataAddress conn scoutid "doctor"
      doctor_telephone <- getExtraDataPhone conn scoutid "doctor"

      invite_email <- getExtraDataFirstContactEmail conn scoutid

      PG.execute conn "INSERT INTO regmgr_attendee (authenticator, state, modified, osm_scoutid, firstname, lastname, dob, registrant_address, registrant_telephone, ec_1_name, ec_1_address, ec_1_telephone, doctor_name, doctor_address, doctor_telephone, invite_email) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
        ( (auth, "M" :: String, newDBTime, scoutid, fn, ln, dob)
       :. (registrant_address, registrant_telephone)
       :. (ec_1_name, ec_1_address, ec_1_telephone)
       :. (doctor_name, doctor_address, doctor_telephone)
       :. [invite_email]
        )

      putStrLn "Inserted"


    wrong -> error $ "Bad data from addReq query: " ++ show wrong

getExtraDataField :: PG.Connection -> Integer -> String -> String -> IO String
getExtraDataField conn scoutid group field = do
  mv <- PG.query conn "SELECT DISTINCT value FROM osm_extradata WHERE scoutid = ? AND group_text_identifier = ? AND varname = ?" (scoutid, group, field)
  case mv of
    [[v]] -> return v
    [] -> return ""
    wrong -> error $ "getExtraDataField: unexpected multiple values: " ++ show wrong

getExtraDataAddress :: PG.Connection -> Integer -> String -> IO String
getExtraDataAddress conn scoutid group = do
  a1 <- getExtraDataField conn scoutid group "address1"
  a2 <- getExtraDataField conn scoutid group "address2"
  a3 <- getExtraDataField conn scoutid group "address3"
  a4 <- getExtraDataField conn scoutid group "address4"
  ap <- getExtraDataField conn scoutid group "postcode"
  return $ commaSeparatedConcat $ [a1, a2, a3, a4, ap]

getExtraDataPhone :: PG.Connection -> Integer -> String -> IO String
getExtraDataPhone conn scoutid group = do
  p1 <- getExtraDataField conn scoutid group "phone1"
  p2 <- getExtraDataField conn scoutid group "phone2"
  let telephone = commaSeparatedConcat [p1, p2]
  return telephone

getExtraDataFullname :: PG.Connection -> Integer -> String -> IO String
getExtraDataFullname conn scoutid group = do
  fn <- getExtraDataField conn scoutid group "firstname"
  ln <- getExtraDataField conn scoutid group "lastname"
  return $ fn ++ " " ++ ln

getExtraDataFirstContactEmail :: PG.Connection -> Integer -> IO String
getExtraDataFirstContactEmail conn scoutid = do
  e1 <- getExtraDataEmail conn scoutid "contact_primary_1" "email1"
  e2 <- getExtraDataEmail conn scoutid "contact_primary_2" "email1"
  e3 <- getExtraDataEmail conn scoutid "contact_primary_1" "email2"
  e4 <- getExtraDataEmail conn scoutid "contact_primary_2" "email2"
  let es :: [String] = concat (maybeToList <$> [e1,e2,e3,e4])
  case es :: [String] of
    [] -> return ""
    (e:_) -> return (e :: String)

getExtraDataEmail :: PG.Connection -> Integer -> String -> String -> IO (Maybe String)
getExtraDataEmail conn scoutid group stub = do
  e <- getExtraDataField conn scoutid group stub
  ps <- getExtraDataField conn scoutid group (stub <> "_leaders")
  let p = if ps == "no" then False else True
  if p && e /= ""
    then return (Just e)
    else return Nothing


commaSeparatedConcat :: [String] -> String
commaSeparatedConcat l = concat $ intersperse ", " $ (filter (/= "")) $ map trim l
  where trim s = T.unpack $ T.strip $ T.pack s

