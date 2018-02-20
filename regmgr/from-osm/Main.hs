{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language MultiWayIf #-}

module Main where

import Data.List (intersperse)

import Data.Traversable (for)
import Data.UUID as UUID
import Data.UUID.V4 as UUID

import qualified Database.PostgreSQL.Simple as PG
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

      a1 <- getExtraDataField conn scoutid "contact_primary_member" "address1"
      a2 <- getExtraDataField conn scoutid "contact_primary_member" "address2"
      a3 <- getExtraDataField conn scoutid "contact_primary_member" "address3"
      a4 <- getExtraDataField conn scoutid "contact_primary_member" "address4"
      ap <- getExtraDataField conn scoutid "contact_primary_member" "postcode"

      let registrant_address = concat $ intersperse ", " $ (filter (/= "")) $ [a1, a2, a3, a4, ap]

      p1 <- getExtraDataField conn scoutid "contact_primary_member" "phone1"
      p2 <- getExtraDataField conn scoutid "contact_primary_member" "phone2"
      let registrant_telephone =
            if | p1 /= "" && p2 == "" -> p1
               | p1 == "" && p2 /= "" -> p2
               | p1 /= "" && p2 /= "" -> p1 ++ " / " ++ p2

      PG.execute conn "INSERT INTO regmgr_attendee (authenticator, state, modified, osm_scoutid, firstname, lastname, dob, registrant_address, registrant_telephone) VALUES (?,?,?,?,?,?,?,?,?)"
        (auth, "M" :: String, newDBTime, scoutid, fn, ln, dob, registrant_address, registrant_telephone)
      putStrLn "Inserted"


    wrong -> error $ "Bad data from addReq query: " ++ show wrong

getExtraDataField :: PG.Connection -> Integer -> String -> String -> IO String
getExtraDataField conn scoutid group field = do
  mv <- PG.query conn "SELECT DISTINCT value FROM osm_extradata WHERE scoutid = ? AND group_text_identifier = ? AND varname = ?" (scoutid, group, field)
  case mv of
    [[v]] -> return v
    [] -> return ""
    wrong -> error $ "getExtraDataField: unexpected multiple values: " ++ show wrong

