{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class (liftIO)

import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Time as PG

import Network.Wai.Handler.Warp (run)

import Servant
import Servant.API

import Lib

type PingAPI =
  "ping" :> Get '[PlainText] String

type InboundAuthenticatorAPI = "inbound" :> Capture "uuid" String :> Get '[PlainText] String

type API = PingAPI :<|> InboundAuthenticatorAPI

server1 :: Server API
server1 = handlePing :<|> handleInbound

handlePing :: Handler String
handlePing = return "PONG"

handleInbound :: String -> Handler String
handleInbound authenticator = do
  liftIO $ putStrLn $ "handleInbound called, authenticator=" ++ authenticator

  -- we can switch in different ways here:
  -- * if the UUID does not exist or it exists more than once,
  --   we should fail - this will probably be a typo or a bug.
  -- * if it exists, and the form is flagged as I for invited, we
  --   should change the flag to V for visited and continue.
  -- * If the form is not submitted, it should be presented for
  --   editing.
  -- * If the form has been submitted, we should present a link to
  --   download the appropriate PDF. and perhaps links to go back
  --   and edit the form.

  -- should also do some optimistic concurrency control on the
  -- record to check that things haven't been happening in
  -- multiple browser tabs and/or elsewhere.

  -- so when sending a form for modification, include the
  -- occ modification time as a field

  -- for a start, lets grab the database record for this uuid.
  -- remember because of OCC we can only do this in a single
  -- transaction.

  liftIO $ putStrLn "opening db"
  conn <- liftIO $ PG.connectPostgreSQL "user='postgres'" 

  entry :: [(Integer, String, String, PG.ZonedTimestamp, String, String, String)] <- liftIO $ query conn "SELECT * FROM regmgr_attendee WHERE authenticator=?" [authenticator]

  liftIO $ putStrLn $ "got sql result: " ++ show entry

  liftIO $ putStrLn "closing db"
  liftIO $ PG.close conn 

  liftIO $ putStrLn "end of req"
  return $ "authenticator: " ++ authenticator ++ "/ " ++ show entry


regmgrAPI :: Proxy API
regmgrAPI = Proxy

app = serve regmgrAPI server1

main :: IO ()
main = do
  putStrLn "regmgr start"

  run 8080 app

  putStrLn "regmgr end"
