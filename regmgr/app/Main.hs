{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}

-- blaze tutorial here:
-- https://jaspervdj.be/blaze/tutorial.html

-- https://ocharles.org.uk/blog/posts/2012-12-02-digestive-functors.html
-- ocharles talks about using digestive-functors with blaze

-- although I might actually want to end up with heist so that the
-- HTML can be edited in template form by other people in a more
-- familiar fashion?

module Main where

-- QUESTION/DISCUSSION: rules for imports that I should follow but don't:
-- Imports may only be qualified, or import explicitly named things
-- The intention is that looking at any part of the source code, it
-- should be possible to figure out where a particular symbol has
-- come from by observing the import list.
-- Generally operators will need to be explicitly imported so that
-- they can be nicely used as operators without a package name.

import Control.Monad.IO.Class (liftIO)

import Data.String (fromString)

import Data.Monoid ( (<>) )

import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Time as PG

import Network.Wai.Handler.Warp (run)

import Servant
import Servant.API
import Servant.HTML.Blaze as SB

import qualified Text.Blaze.Html5 as B
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5.Attributes as BA

import Lib

type PingAPI =
  "ping" :> Get '[PlainText] String

type HTMLPingAPI =
  "htmlping" :> Get '[SB.HTML] B.Html

type InboundAuthenticatorAPI = "inbound" :> Capture "uuid" String :> Get '[HTML] B.Html

type UpdateFormAPI = "updateForm" :> ReqBody '[FormUrlEncoded] [(String,String)] :> Post '[HTML] B.Html

-- QUESTION/DISCUSSION: note how when updating this API type, eg to
-- add an endpoint or to change an endpoint type, that there will be
-- a type error if we dont' also update server1 to handle the change
-- appropriately. This is part of the niceness of staticly typed APIs.
type API = PingAPI :<|> InboundAuthenticatorAPI
      :<|> HTMLPingAPI
      :<|> UpdateFormAPI

server1 :: Server API
server1 = handlePing :<|> handleInbound :<|> handleHTMLPing
  :<|> handleUpdateForm

handlePing :: Handler String
handlePing = return "PONG"

handleHTMLPing :: Handler B.Html
handleHTMLPing = return $ B.docTypeHtml $ do
  B.head $ do
    B.title "regmgr HTML ping response page"
  B.body $ do
    B.h1 "Ping response"
    B.p "ok"


handleInbound :: String -> Handler B.Html
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

  let (n, authenticator, state, modified, firstname, lastname, dob) = head entry -- assumes exactly one entry matches this authenticator. BUG: there might be none;  there might be more than 1 but that is statically guaranteed not to happen in the SQL schema (so checked by the SQL type system, not the Haskell type system) - so that's an 'error "impossible"' case.

  liftIO $ putStrLn $ "got sql result: " ++ show entry

  liftIO $ putStrLn "closing db"
  liftIO $ PG.close conn 

  let title = "Registration for " <> B.toHtml firstname <> " " <> B.toHtml lastname

  let editableHtml =
        B.docTypeHtml $ do
          B.head $ do
            B.title title
          B.body $ do
            B.h1 title
            B.p "Please fill out this registration form. We have put information that we know already into the form, but please check and correct those if that information is wrong."
            -- QUESTION/DISCUSSION: type_ has to have a different name with an underscore because type is a reserved word.
            B.form ! BA.action "/updateForm"
                   ! BA.method "post"
             $ do

              -- QUESTION/DISCUSSION: the authenticator lets us find the relevant record (it's a combo of identifier and authenticator...) and the modified value lets us do OCC transaction handling.
              B.input
                  ! BA.type_ "hidden"
                  ! BA.name "authenticator"
                  ! BA.value (fromString authenticator)
              B.input
                  ! BA.type_ "hidden"
                  ! BA.name "modified"
                  ! BA.value (fromString (show modified)) -- might need a better data format to be able to deserialise it elsewhere.

              B.p $ do
                "Family name: "
                B.input
                  ! BA.type_ "text" 
                  ! BA.name "firstname" 
                  ! BA.value (fromString firstname)
              B.p $ do
                "Family name: " 
                B.input
                  ! BA.type_ "text"
                  ! BA.name "lastname"
                  ! BA.value (fromString lastname)
              -- QUESTION/DISCUSSION: this could be a date picker on the client side in javascript?
              B.p $ do
                "Date of birth: "
                B.input
                  ! BA.type_ "text"
                  ! BA.name "dob"
                  ! BA.value (fromString dob)
              B.input
                ! BA.type_ "submit"
                ! BA.value "Register for event"
            B.hr
            B.h2 "Internal debugging information"
            B.p $ "Last modified: " <> B.toHtml (show modified)
            B.p $ "State: " <> B.toHtml state
            B.p $ "Record number: " <> B.toHtml n

  liftIO $ putStrLn "end of req"

  return editableHtml

handleUpdateForm :: [(String,String)] -> Handler B.Html
handleUpdateForm reqbody = 
  return $ B.toHtml $ "handleFormFilled stub TODO NOTIMPL BENC: req body is " ++ show reqbody

regmgrAPI :: Proxy API
regmgrAPI = Proxy

app = serve regmgrAPI server1

main :: IO ()
main = do
  putStrLn "regmgr start"

  run 8080 app

  putStrLn "regmgr end"



