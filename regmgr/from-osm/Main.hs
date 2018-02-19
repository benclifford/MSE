{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}

module Main where

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

      [[newDBTime]] :: [[PGT.ZonedTimestamp]] <- PG.query conn "SELECT NOW()" ()

      PG.execute conn "INSERT INTO regmgr_attendee (authenticator, state, modified, osm_scoutid, firstname, lastname, dob) VALUES (?,?,?,?,?,?,?)"
        (auth, "M" :: String, newDBTime, scoutid, fn, ln, dob)
      putStrLn "Inserted"


    wrong -> error $ "Bad data from addReq query: " ++ show wrong

