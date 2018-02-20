{-# Options_GHC -Werror -W #-}

{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveGeneric #-}

{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}

-- for PDF rendering:
{-# Language MultiParamTypeClasses #-}

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

import Control.Monad (when)

import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Char8 as BSSC
import qualified Data.ByteString.Lazy as BS

import qualified Data.Csv as CSV
import qualified Data.Text as T
import Data.String (fromString)

import Data.Monoid ( (<>) )

import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Time as PG
import Database.PostgreSQL.Simple.SOP as PGS

import Network.Wai.Handler.Warp (run)

import Servant
import Servant.HTML.Blaze as SB

import Servant.CSV.Cassava as SC

import qualified Text.Blaze.Html5 as B
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5.Attributes as BA

import qualified Text.Digestive as DF
import Text.Digestive ( (.:) )

import Text.Digestive.Blaze.Html5 as DB

import Config
import DB
import DBSOP
import DigestiveBits
import DigestiveServant
import Invitation
import InvitationEmail
import Lib
import PDF
import Registration


type PingAPI =
  "ping" :> Get '[PlainText] String

type HTMLPingAPI =
  "htmlping" :> Get '[SB.HTML] B.Html


type InboundAuthenticatorAPI = "register" :> Capture "uuid" String :> Get '[HTML] B.Html

type UpdateFormAPI = "register" :> Capture "uuid" String :> ReqBody '[FormUrlEncoded] [(String,String)] :> Post '[HTML] B.Html

type PDFFormAPI = "pdf" :> Capture "auth" String :> Get '[PDF] BS.ByteString

type UnlockAPI = "unlock" :> Capture "auth" String :> Get '[HTML] B.Html

type AdminAuth = BasicAuth "regmgr" User

type InviteGetAPI = "admin" :> "invite" :> AdminAuth :> Get '[HTML] B.Html
type InvitePostAPI = "admin" :> "invite" :> ReqBody '[FormUrlEncoded] [(String, String)] :> AdminAuth :> Post '[HTML] B.Html

type MailTestAPI = "admin" :> "mailtest" :> AdminAuth :> Get '[HTML] B.Html

type CSVAPI = "admin" :> "csv" :> AdminAuth :> Get '[SC.CSV] (Headers '[Header "Content-Disposition" String] [Registration])

type AdminStaticAPI = "admin" :> AdminAuth :> Get '[HTML] B.Html


-- QUESTION/DISCUSSION: note how when updating this API type, eg to
-- add an endpoint or to change an endpoint type, that there will be
-- a type error if we dont' also update server1 to handle the change
-- appropriately. This is part of the niceness of staticly typed APIs.
type API = PingAPI :<|> InboundAuthenticatorAPI
      :<|> HTMLPingAPI
      :<|> UpdateFormAPI
      :<|> PDFFormAPI
      :<|> UnlockAPI
      :<|> InviteGetAPI
      :<|> InvitePostAPI
      :<|> CSVAPI
      :<|> AdminStaticAPI
      :<|> MailTestAPI

server1 :: Server API
server1 = handlePing :<|> handleRegistrationGet :<|> handleHTMLPing
  :<|> handleRegistrationPost
  :<|> handlePDFForm
  :<|> handleUnlock
  :<|> handleInviteGet
  :<|> handleInvitePost
  :<|> handleCSV
  :<|> handleAdminStatic
  :<|> handleMailTest

handlePing :: Handler String
handlePing = return "PONG"

handleHTMLPing :: Handler B.Html
handleHTMLPing = return $ B.docTypeHtml $ do
  B.head $ do
    B.title "regmgr HTML ping response page"
  B.body $ do
    B.h1 "Ping response"
    B.p "ok"

handleRegistrationGet :: String -> Handler B.Html
handleRegistrationGet auth = do

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

  entry :: [Registration] <- selectByAuthenticator auth

  let val = head entry -- assumes exactly one entry matches this authenticator. BUG: there might be none;  there might be more than 1 but that is statically guaranteed not to happen in the SQL schema (so checked by the SQL type system, not the Haskell type system) - so that's an 'error "impossible"' case.

  liftIO $ putStrLn $ "got sql result: " ++ show entry

  let title = "Registration for " <> B.toHtml (firstname val) <> " " <> B.toHtml (lastname val)

  view :: DF.View B.Html <- DF.getForm "Registration" (registrationDigestiveForm val)

  let editable = entryEditable val

  let outputHtml =
        B.docTypeHtml $ do
          B.head $ do
            B.title title
          B.body $ do
            B.h1 title
            regformHtml auth view editable

  liftIO $ putStrLn "end of req"

  return outputHtml


selectByAuthenticator auth = withDB $ \conn -> PGS.gselectFrom conn "regmgr_attendee where authenticator=?" [auth]

selectAll = withDB $ \conn -> PGS.gselectFrom conn "regmgr_attendee" ()

updateByAuthAndModified conn val' auth oldDBTime = gupdateInto conn "regmgr_attendee" "authenticator = ? AND modified = ?" val' (auth, oldDBTime)


-- | generates an HTML view of this form, which may be editable
--   or fixed text.
regformHtml :: String -> DF.View B.Html -> Bool -> B.Html
regformHtml auth view editable = do
            if editable
              then B.p "Please fill out this registration form. We have put information that we know already into the form, but please check and correct those if that information is wrong."
              else do
                B.p "This is the information held for this attendee. Please edit and re-print this if information is incorrect."
                B.p $ do
                  "If you have not done so already, "
                  (B.a ! BA.href ("/pdf/" <> fromString auth))
                    "please print out and sign the permission form"
                  "."
                B.p $ do
                  (B.a ! BA.href ("/unlock/" <> fromString auth))
                    "Edit this form again - ***REMEMBER YOU WILL NEED TO PRINT AND SIGN A NEW COPY WITH ANY CHANGES***"

            -- QUESTION/DISCUSSION: type_ has to have a different name with an underscore because type is a reserved word.
            B.form ! BA.action ("/register/" <> (fromString auth))
                   ! BA.method "post"
             $ do

              -- QUESTION/DISCUSSION: the authenticator lets us find the relevant record (it's a combo of identifier and authenticator...) and the modified value lets us do OCC transaction handling.
              DB.inputHidden "authenticator" view
              DB.inputHidden "modified" view

              textInputLineParagraph editable "firstname" view "First name"
              textInputLineParagraph editable "lastname" view "Family name"
              textInputLineParagraph editable "registrant_address" view "Address"
              textInputLineParagraph editable "registrant_telephone" view "Telephone"

              -- QUESTION/DISCUSSION: this could be a date picker on the client side in javascript?
              textInputLineParagraph editable "dob" view "Date of Birth"
              B.hr
              B.p "Emergency Contact 1"
              textInputLineParagraph editable "ec_1_name" view "Name"
              textInputLineParagraph editable "ec_1_relationship" view "Relationship"
              textInputLineParagraph editable "ec_1_address" view "Address"
              textInputLineParagraph editable "ec_1_telephone" view "Telephone"
              textInputLineParagraph editable "ec_1_mobile" view "Mobile telephone"

              B.hr
              B.p "Emergency Contact 2"
              textInputLineParagraph editable "ec_2_name" view "Name"
              textInputLineParagraph editable "ec_2_relationship" view "Relationship"
              textInputLineParagraph editable "ec_2_address" view "Address"
              textInputLineParagraph editable "ec_2_telephone" view "Telephone"
              textInputLineParagraph editable "ec_2_mobile" view "Mobile telephone"

              B.hr
              B.p "Doctor / GP"
              textInputLineParagraph editable "doctor_name" view "Name"
              textInputLineParagraph editable "doctor_address" view "Address"
              textInputLineParagraph editable "doctor_telephone" view "Telephone"

              B.hr
              boolInputParagraph editable "swim" view "Can participant swim?"
              boolInputParagraph editable "vegetarian" view "Is participant vegetarian?"
              B.hr
              B.p "Medical information"
              textInputLineParagraph editable "tetanus_date" view "Date of last tetanus"
              textInputAreaParagraph editable "diseases" view "Details of any infections/diseases"
              textInputAreaParagraph editable "allergies" view "Details of any allergies"
              textInputAreaParagraph editable "medication_diet" view "Details of any medication or medical diets"
              textInputAreaParagraph editable "dietary_reqs" view "Details of any dietary requirements"
              textInputAreaParagraph editable "faith_needs" view "Details of any faith/cultural needs (eg dress, diet, holy days, toilet arrangements)"
             

              when editable $ DB.inputSubmit "Register for event"
            B.hr

boolInputParagraph editable fieldName view description = 
  if editable
    then      B.p $ do
                DB.label fieldName view description
                ": "
                DB.errorList fieldName view
                DB.inputCheckbox fieldName view
    else B.p $ do
                DB.label fieldName view description
                ": "
                DB.errorList fieldName view
                readonlyInputBool fieldName view


textInputLineParagraph editable fieldName view description = 
  if editable
    then      B.p $ do
                DB.label fieldName view description
                ": "
                DB.errorList fieldName view
                DB.inputText fieldName view
    else B.p $ do
                DB.label fieldName view description
                ": "
                DB.errorList fieldName view
                readonlyInputText fieldName view

textInputAreaParagraph editable fieldName view description = 
  if editable
    then do B.p $ do
                DB.label fieldName view description
                ": "
                DB.errorList fieldName view
            B.p $ DB.inputTextArea (Just 8) (Just 80) fieldName view
    else do B.p $ do
                DB.label fieldName view description
                ": "
                DB.errorList fieldName view
            B.p $ readonlyInputText fieldName view


-- | loosely based on inputText source
readonlyInputText :: T.Text -> DF.View v -> B.Html
readonlyInputText ref view = B.toHtml $ DF.fieldInputText ref view

readonlyInputBool :: T.Text -> DF.View v -> B.Html
readonlyInputBool ref view = B.toHtml $ DF.fieldInputBool ref view



handleRegistrationPost :: String -> [(String,String)] -> Handler B.Html
handleRegistrationPost auth reqBody = do

  entry :: [Registration] <- selectByAuthenticator auth

  let val = head entry -- assumes exactly one entry matches this authenticator. BUG: there might be none;  there might be more than 1 but that is statically guaranteed not to happen in the SQL schema (so checked by the SQL type system, not the Haskell type system) - so that's an 'error "impossible"' case.

  liftIO $ putStrLn $ "got sql result: " ++ show entry

  let title = "Registration for " <> B.toHtml (firstname val) <> " " <> B.toHtml (lastname val)

  let editable = entryEditable val

-- TODO: end factor
  f <- DF.postForm "Registration" (registrationDigestiveForm val) (servantPathEnv reqBody)

  outputHtml <- case f of
    (_, Just val) -> do

        liftIO $ putStrLn "Updating DB"
        -- write out 'val' to the database
        sqlres <- withDB $ \conn -> do

          newDBTime <- dbNow conn
 
          putStrLn $ "new SQL database time: " ++ show newDBTime
       
          let oldDBTime = modified val 
          putStrLn $ "old SQL database time: " ++ show oldDBTime
          let val' = val { modified = newDBTime, state = "C" }

          r <- updateByAuthAndModified conn val' auth oldDBTime

          putStrLn $ "SQL UPDATE returned: " ++ (show r)
          return r

        if sqlres == 1
          then return $ -- success
            B.docTypeHtml $ do
              B.head $ do
                B.title title
              B.body $ do
                B.h1 title
                B.p $ do
                  "Next steps: "
                  (B.a ! BA.href ("/pdf/" <> fromString auth))
                    "please print out and sign the permission form"
                  ", and then give it to a leader with payment."
                B.p "debug: handleUpdateForm - there are no errors"
                B.p $ do
                  "debug: "
                  B.toHtml $ show val
                B.p $ do
                  "debug: "
                  (B.a ! BA.href ("/unlock/" <> fromString auth))
                    "admin mode: unlock this participant for further edits"
          else -- failure due to database error
            return $ B.docTypeHtml $ do
              B.head $ do
                B.title title
              B.body $ do
                B.h1 title
                B.p "debug: handleUpdateForm - SQL update did not update any rows, but no errors in digestive-functor form"
                B.p "There was a problem submitting your form - perhaps you have already completed the form in another tab, or someone else has edited the form at the same time"
                B.p "Please reload the link that you were sent and start again"
                B.p "Sorry"

    -- TODO: this would be better implemented as a loop-like construct
    -- which only ever spits out the correct value and otherwise
    -- shortcircuits execution and provides the user with another
    -- chance to edit?
    (view, Nothing) -> return $
          B.docTypeHtml $ do
            B.head $ do
              B.title title
            B.body $ do
              B.h1 title
              B.p "debug: handleUpdateForm - there are errors"
              regformHtml auth view editable
               
  return outputHtml

regmgrAPI :: Proxy API
regmgrAPI = Proxy

authcheck :: BasicAuthCheck User
authcheck =
  let check (BasicAuthData username password) = do
        liftIO $ putStrLn "In auth check / BENC"
        config <- readConfig
        if  username == BSSC.pack (adminUsername config)
         && password == BSSC.pack (adminPassword config)
        then return (Authorized (BSSC.unpack username))
        else return Unauthorized
      in BasicAuthCheck check


context :: Context (BasicAuthCheck User ': '[])
context = authcheck Servant.:. EmptyContext

app = serveWithContext regmgrAPI context server1

main :: IO ()
main = do
  putStrLn "regmgr start"

  run 8080 app

  putStrLn "regmgr end"



-- | This is the registration form as a Digestive Functor form
registrationDigestiveForm :: Monad m => Registration -> DF.Form B.Html m Registration
registrationDigestiveForm init = Registration
  <$> "authenticator" .: DF.string (Just $ authenticator init) -- TODO: a validator on this should check that the form value matches up with the value in init - which will, for example, have been read from the db
  <*> "state" .: DF.string (Just $ state init) -- TODO: a validator on this should check that the form value matches up with the value in init, which will for example, have come from the DB. Or is perhaps also nullable to allow creation of entries. Or perhaps to check we are doing the right kind of state progression so that record can't be moved into wrong state by rogue HTTP request?
  <*> "modified" .: (read <$> (DF.string (Just $ show $ modified init))) -- BUG: ignores modified time from client! which means OCC is broken as we always default to the latest version and so mostly don't ever hit a conflict. And the problem here is that we need to serialise it out to a form field and then parse it back in later, which is troublesome.
  <*> "invite_email" .: DF.string (Just $ invite_email init)

  -- security bug here maybe: if I fake a form response, I can send in a new osm_scoutid, and make my record be attached to a different OSM record. Which at present would impede the real owner of that record being invited if they had not already been invited.
  <*> "osm_scoutid" .: DF.optionalStringRead "OSM scout ID" (osm_scoutid init)

  <*> "firstname" .: DF.string (Just $ firstname init)
  <*> "lastname" .: DF.string (Just $ lastname init)
  <*> "dob" .: nonEmptyString (Just $ dob init)
  <*> "registrant_address" .: DF.string (Just $ registrant_address init)
  <*> "registrant_telephone" .: DF.string (Just $ registrant_telephone init)

  <*> "ec_1_name" .: DF.string (Just $ ec_1_name init)
  <*> "ec_1_relationship" .: DF.string (Just $ ec_1_relationship init)
  <*> "ec_1_address" .: DF.string (Just $ ec_1_address init)
  <*> "ec_1_telephone" .: DF.string (Just $ ec_1_telephone init)
  <*> "ec_1_mobile" .: DF.string (Just $ ec_1_mobile init)

  <*> "ec_2_name" .: DF.string (Just $ ec_2_name init)
  <*> "ec_2_relationship" .: DF.string (Just $ ec_2_relationship init)
  <*> "ec_2_address" .: DF.string (Just $ ec_2_address init)
  <*> "ec_2_telephone" .: DF.string (Just $ ec_2_telephone init)
  <*> "ec_2_mobile" .: DF.string (Just $ ec_2_mobile init)

  <*> "doctor_name" .: DF.string (Just $ doctor_name init)
  <*> "doctor_address" .: DF.string (Just $ doctor_address init)
  <*> "doctor_telephone" .: DF.string (Just $ doctor_telephone init)

  <*> "swim" .: DF.bool (Just $ swim init)  
  <*> "vegetarian" .: DF.bool (Just $ vegetarian init)

  <*> "tetanus_date" .: DF.string (Just $ tetanus_date init)
  <*> "diseases" .: DF.string (Just $ diseases init)
  <*> "allergies" .: DF.string (Just $ allergies init)
  <*> "medication_diet" .: DF.string (Just $ medication_diet init)
  <*> "dietary_reqs" .: DF.string (Just $ dietary_reqs init)
  <*> "faith_needs" .: DF.string (Just $ faith_needs init)



entryEditable :: Registration -> Bool
entryEditable registration = state registration `elem` ["M", "N", "I"]

-- debug code to unlock

handleUnlock :: String -> Handler B.Html
handleUnlock auth = do

  withDB $ \conn -> execute conn "UPDATE regmgr_attendee SET state = ? WHERE authenticator = ?" ("N" :: String, auth)

  handleRegistrationGet auth


handleCSV :: User -> Handler (Headers '[Header "Content-Disposition" String] [Registration])
handleCSV _user = do
  
  registrations :: [Registration] <- selectAll

  return $ addHeader "attachment;filename=\"registrations.csv\"" registrations

instance CSV.ToNamedRecord Registration
instance CSV.DefaultOrdered Registration

-- ZonedTimestamp is an alias, so
-- turn on TypeSynonymInstaces for this
-- and FlexibleInstances
instance CSV.ToField PG.ZonedTimestamp
  where
    toField ts = CSV.toField (show ts)
instance CSV.ToField Bool
  where
    toField bool = CSV.toField (show bool)





handleAdminStatic :: User -> Handler B.Html
handleAdminStatic _user = return $ do
  B.p "Admin page"
  B.p $ (B.a ! BA.href "/admin/invite")
      "Invite new participant"
  B.p $ (B.a ! BA.href "/admin/csv")
      "Download CSV of all forms, completed and not-completed"
  


handleMailTest :: User -> Handler B.Html
handleMailTest _user = do
  liftIO $ sendTop
  return $ B.p "mail test response"
