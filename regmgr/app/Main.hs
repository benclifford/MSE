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

import Control.Monad (when, mapM_, void)

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
import OptionalTextForm
import PDF
import Registration
import Medication

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
type SendInviteEmailAPI = "admin" :> "sendInviteEmail" :> Capture "uuid" String :> AdminAuth :> Get '[HTML] B.Html

type CSVAPI = "admin" :> "csv" :> AdminAuth :> Get '[SC.CSV] (Headers '[Header "Content-Disposition" String] [Registration])

type AdminTopAPI = "admin" :> AdminAuth :> Get '[HTML] B.Html

type FilesAPI = "file" :> Raw

type MedicationAddGetAPI = "medication" :> "add" :> Capture "auth" String :> Get '[HTML] B.Html

type MedicationAddPostAPI = "medication" :> "add" :> Capture "auth" String :> ReqBody '[FormUrlEncoded] [(String, String)] :> Post '[HTML] B.Html

type MedicationDeleteGetAPI = "medication" :> "delete" :> Capture "auth" String :> Capture "MedicationID" Integer :> Get '[HTML] B.Html

type MedicationEditGetAPI = "medication" :> "edit" :> Capture "auth" String :> Capture "MedicationID" Integer :> Get '[HTML] B.Html

type MedicationEditPostAPI = "medication" :> "edit" :> Capture "auth" String :> Capture "MedicationID" Integer :> ReqBody '[FormUrlEncoded] [(String, String)] :> Post '[HTML] B.Html

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
      :<|> AdminTopAPI
      :<|> SendInviteEmailAPI
      :<|> FilesAPI
      :<|> MedicationAddGetAPI
      :<|> MedicationAddPostAPI
      :<|> MedicationDeleteGetAPI
      :<|> MedicationEditGetAPI
      :<|> MedicationEditPostAPI

server1 :: Server API
server1 = handlePing :<|> handleRegistrationGet :<|> handleHTMLPing
  :<|> handleRegistrationPost
  :<|> handlePDFForm
  :<|> handleUnlock
  :<|> handleInviteGet
  :<|> handleInvitePost
  :<|> handleCSV
  :<|> handleAdminTop
  :<|> handleSendInviteEmail
  :<|> handleFiles
  :<|> handleMedicationAddGet
  :<|> handleMedicationAddPost
  :<|> handleMedicationDeleteGet
  :<|> handleMedicationEditGet
  :<|> handleMedicationEditPost

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

  meds :: [(Integer,Medication)] <- selectMedicationsIDsByAuthenticator auth

  liftIO $ putStrLn $ "got sql result: " ++ show entry

  let title = "Registration for " <> B.toHtml (firstname val) <> " " <> B.toHtml (lastname val)

  view :: DF.View B.Html <- DF.getForm "Registration" (registrationDigestiveForm val)

  let editable = entryEditable val

  labels <- liftIO $ readLabels

  let outputHtml =
        B.docTypeHtml $ do
          B.head $ do
            B.title title
            jqueryHead
          B.body $ do
            B.h1 title
            regformHtml auth view editable labels meds

  liftIO $ putStrLn "end of req"

  return outputHtml


selectByAuthenticator auth = withDB $ \conn -> PGS.gselectFrom conn "regmgr_attendee where authenticator=?" [auth]

selectAll = withDB $ \conn -> PGS.gselectFrom conn "regmgr_attendee" ()

updateByAuthAndModified conn val' auth oldDBTime = gupdateInto conn "regmgr_attendee" "authenticator = ? AND modified = ?" val' (auth, oldDBTime)


-- | generates an HTML view of this form, which may be editable
--   or fixed text.
regformHtml :: String -> DF.View B.Html -> Bool -> [(String, String)] -> [(Integer, Medication)] -> B.Html
regformHtml auth view editable ls meds = do
            if editable
              then B.p "Please fill out this registration form. We have put information that we know already into the form, but please check and correct that if that information is wrong."
              else do
                B.p $ do
                  "If you need to change any information on the main form, you can "
                  (B.a ! BA.href ("/unlock/" <> fromString auth)) "click here to edit the form again"
                  "."

                B.p "Next, please enter any details of medications here:"
 
                listOfMedications auth meds
                B.p $ do
                  (B.a ! BA.href ("/medication/add/" <> fromString auth))
                    "Click here to add a new medication"

                B.hr

                B.p $ do
                  "When you have entered details of medications, please "
                  (B.a ! BA.href ("/pdf/" <> fromString auth)) "print out a copy of the complete registration pack"
                  ", then sign each page, and return it with payment to your scout leader"

                B.p "If you need to change anything, please come back to this page and edit the information here. You will need to print and sign a new copy of the registration pack"
                  

                B.hr 

            -- QUESTION/DISCUSSION: type_ has to have a different name with an underscore because type is a reserved word.
            B.form ! BA.action ("/register/" <> (fromString auth))
                   ! BA.method "post"
             $ do

              -- it is useful to include modified in the form submission so as to catch OCC bugs (unlike authenticator and state which should be constant and never modified using this form)
              DB.inputHidden "modified" view
              DB.errorList "modified" view

              textInputLineParagraph editable "firstname" view "First name"
              textInputLineParagraph editable "lastname" view "Family name"
              textInputLineParagraph editable "registrant_address" view "Address"
              textInputLineParagraph editable "registrant_telephone" view "Telephone"

              -- QUESTION/DISCUSSION: this is a date picker on the client side in javascript
              dateInputParagraph editable "dob" view "Date of Birth"
              B.hr
              B.p "Emergency Contact 1"
              textInputLineParagraph editable "ec_1_name" view "Name"
              textInputLineParagraph editable "ec_1_relationship" view "Relationship"
              textInputLineParagraph editable "ec_1_address" view "Address"
              textInputLineParagraph editable "ec_1_telephone" view "Telephone"
              textInputLineParagraph editable "ec_1_telephone2" view "Alternative telephone"

              B.hr
              B.p "Emergency Contact 2"
              textInputLineParagraph editable "ec_2_name" view "Name"
              textInputLineParagraph editable "ec_2_relationship" view "Relationship"
              textInputLineParagraph editable "ec_2_address" view "Address"
              textInputLineParagraph editable "ec_2_telephone" view "Telephone"
              textInputLineParagraph editable "ec_2_telephone2" view "Alternative telephone"

              B.hr
              B.p "Doctor / GP"
              textInputLineParagraph editable "doctor_name" view "Name"
              textInputLineParagraph editable "doctor_address" view "Address"
              textInputLineParagraph editable "doctor_telephone" view "Telephone"
              B.hr

              boolInputParagraph editable "vegetarian" view "Is attendee vegetarian?"
              B.hr
              B.p "Medical information"
              dateInputParagraph editable "tetanus_date" view (getLabel "tetanus_date" ls)
              B.hr
              optionalTextInputAreaParagraph editable "diseases" view (getLabel "diseases" ls)
              B.hr
              optionalTextInputAreaParagraph editable "allergies" view (getLabel "allergies" ls)
              B.hr
              optionalTextInputAreaParagraph editable "medication_diet" view (getLabel "medication_diet" ls)
              B.hr
              optionalTextInputAreaParagraph editable "dietary_reqs" view (getLabel "dietary_reqs" ls)
              B.hr
              optionalTextInputAreaParagraph editable "faith_needs" view (getLabel "faith_needs" ls)

              B.hr
              boolInputParagraph editable "remedy_paracetamol" view (getLabel "remedy_paracetamol" ls)
              boolInputParagraph editable "remedy_piriton" view (getLabel "remedy_piriton" ls)
              boolInputParagraph editable "remedy_ibuprofen" view (getLabel "remedy_ibuprofen" ls)
              boolInputParagraph editable "remedy_anthisan" view (getLabel "remedy_anthisan" ls)


{-
              B.hr
              B.p "All activities will be run in accordance with The Scout Association's Safety Rules. Not all the activities listed will be available and attendees do not have to participate in any adventurous activities they do not wish to."

              boolInputParagraph editable "general_activities" view (getLabel "general_activities" ls)
              boolInputParagraph editable "water_activities" view (getLabel "water_activities" ls)
              boolInputParagraph editable "swim" view (getLabel "swim" ls)
              boolInputParagraph editable "firearms" view (getLabel "firearms" ls)
              getLabelHtml "firearms_html" ls
-}

              when editable $ do
                B.hr
                submitButton "submit" "save" "Save partially completed form to finish later"
                " or "
                submitButton "submit" "register" "Submit completed form and register for event"
            B.hr

getLabel :: String -> [(String, String)] -> B.Html
getLabel l ls = let
  ml = lookup ("label_" ++ l) ls
  in case ml of
    Just v -> B.toHtml v
    Nothing -> error $ "Missing label defintion for key " ++ show l

getLabelHtml :: String -> [(String, String)] -> B.Html
getLabelHtml l ls = let
  ml = lookup ("label_" ++ l) ls
  in case ml of
    Just v -> B.preEscapedString v
    Nothing -> error $ "Missing label defintion for key " ++ show l


-- note that this isn't using the name hierarchy created by
-- digestive functors - because (so far) I'm not using digestive
-- functors to extact the value... this isn't part of the
-- form "value"
submitButton :: B.AttributeValue -> B.AttributeValue -> T.Text -> B.Html
submitButton name value description =
  (B.button ! BA.type_ "submit"
            ! BA.name name
            ! BA.value value
  )
    (B.toHtml description)

boolInputParagraph editable fieldName view description = 
  if editable
    then      B.p $ do
                DB.label fieldName view description
                ": "
                DB.inputRadio False fieldName view
                DB.errorList fieldName view
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
                (DB.inputText fieldName view) ! BA.size "80"
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

dateInputParagraph editable fieldName view description = 
  if editable
    then      B.p $ do
                DB.label fieldName view description
                ": "
                DB.errorList fieldName view
                (DB.inputText fieldName view) ! BA.size "80"
                let ar = DF.absoluteRef fieldName view
                B.script $ do
                  "$(\"#"
                  B.toHtml (escapeDots ar)
                  "\").datepicker({dateFormat: \"yy-mm-dd\"});"
-- Make something like <script>$( "#a.b" ).datepicker();</script>

-- TODO: date formats
-- TODO: initialise picker if we have a valid value

    else B.p $ do
                DB.label fieldName view description
                ": "
                DB.errorList fieldName view
                readonlyInputText fieldName view


-- | loosely based on inputText source
readonlyInputText :: T.Text -> DF.View v -> B.Html
readonlyInputText ref view = B.toHtml $ DF.fieldInputText ref view

readonlyInputBool :: T.Text -> DF.View B.Html -> B.Html
readonlyInputBool ref view = B.toHtml $
  if isSelected $ DF.fieldInputChoice ref view 
  then "Yes" :: String
  else "No" :: String
 where isSelected :: [(T.Text, B.Html, Bool)] -> Bool
       isSelected l = let
         selecteds = map (\(v, _, _) -> v) $ filter (\(_, _, s) -> s) l
         [r] = selecteds
         in case r of
           "Y" -> True
           "N" -> False
           ev -> error $ "Missing pattern in isSelected: " ++ (show ev)


handleRegistrationPost :: String -> [(String,String)] -> Handler B.Html
handleRegistrationPost auth reqBody = do

  entry :: [Registration] <- selectByAuthenticator auth
  meds <- selectMedicationsIDsByAuthenticator auth

  let val = head entry -- assumes exactly one entry matches this authenticator. BUG: there might be none;  there might be more than 1 but that is statically guaranteed not to happen in the SQL schema (so checked by the SQL type system, not the Haskell type system) - so that's an 'error "impossible"' case.

  liftIO $ putStrLn $ "got sql result: " ++ show entry

  let title = "Registration for " <> B.toHtml (firstname val) <> " " <> B.toHtml (lastname val)

  let editable = entryEditable val


-- we might change the registrationDigestiveForm here to validate differently depending on which submission option we have chosen? because save validation could be looser than submit validation.


  -- submit type is mandatory - we'll fail (without a beautiful error)
  -- if this isn't the case
  let (Just submitType) = lookup "submit" reqBody

  liftIO $ putStrLn $ "submit type is: " ++ show submitType

-- how can I tell which submit button was clicked?
-- looks like i shoudl switch to <button type=submit> which can
-- separately have a name/value and renderable text.
-- this name won't be a form field, I think - it's not part
-- of the data.

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

          let val' = val { modified = newDBTime }

          -- Note that the submission types are not type safe - we could
          -- try to make those a bit safer?
          let val'' = val' { state = if submitType == "register"
                                     then "C"
                                     else "P"
                           }

          r <- updateByAuthAndModified conn val'' auth oldDBTime

          putStrLn $ "SQL UPDATE returned: " ++ (show r)
          return r

        if sqlres == 1
          then handleRegistrationGet auth
          else -- failure due to database error
            return $ B.docTypeHtml $ do
              B.head $ do
                B.title title
                jqueryHead
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
    (view, Nothing) -> do
      labels <- liftIO $ readLabels
      return $
          B.docTypeHtml $ do
            B.head $ do
              B.title title
              jqueryHead
            B.body $ do
              B.h1 title
              B.p "debug: handleUpdateForm - there are errors"
              regformHtml auth view editable labels meds
               
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

  <$> "authenticator" .: constString (authenticator init)

  <*> "state" .: constString (state init)

  <*> "modified" .: (read <$> (constString (show $ modified init)))

-- we have an opportunity here to discover the the modified token has changed, before we attempt to hit the database. Although the final check will happen at the database, we can get it here as a form validation error first, which might present better scope for handling it. eg still displaying editable form contents. or allowing a force override option to appear in the user interface (which could swap the old modified for the new modified in the UI, for example).

  <*> "invite_email" .: DF.string (Just $ invite_email init)

  -- security bug here maybe: if I fake a form response, I can send in a new osm_scoutid, and make my record be attached to a different OSM record. Which at present would impede the real owner of that record being invited if they had not already been invited.
  <*> "osm_scoutid" .: constOptionalStringRead "OSM scout ID" (osm_scoutid init)

  <*> "firstname" .: DF.string (Just $ firstname init)
  <*> "lastname" .: DF.string (Just $ lastname init)
  <*> "dob" .: nonEmptyString (Just $ dob init)
  <*> "section" .: DF.string (Just $ section init)
  <*> "registrant_address" .: DF.string (Just $ registrant_address init)
  <*> "registrant_telephone" .: DF.string (Just $ registrant_telephone init)
  <*> "registrant_telephone2" .: DF.string (Just $ registrant_telephone2 init)

  <*> "ec_1_name" .: DF.string (Just $ ec_1_name init)
  <*> "ec_1_relationship" .: DF.string (Just $ ec_1_relationship init)
  <*> "ec_1_address" .: DF.string (Just $ ec_1_address init)
  <*> "ec_1_telephone" .: DF.string (Just $ ec_1_telephone init)
  <*> "ec_1_telephone2" .: DF.string (Just $ ec_1_telephone2 init)

  <*> "ec_2_name" .: DF.string (Just $ ec_2_name init)
  <*> "ec_2_relationship" .: DF.string (Just $ ec_2_relationship init)
  <*> "ec_2_address" .: DF.string (Just $ ec_2_address init)
  <*> "ec_2_telephone" .: DF.string (Just $ ec_2_telephone init)
  <*> "ec_2_telephone2" .: DF.string (Just $ ec_2_telephone2 init)

  <*> "doctor_name" .: DF.string (Just $ doctor_name init)
  <*> "doctor_address" .: DF.string (Just $ doctor_address init)
  <*> "doctor_telephone" .: DF.string (Just $ doctor_telephone init)
  <*> pure False
  <*> pure False
  <*> pure False

  <*> "vegetarian" .: DF.choiceWith [("Y", (True, "yes")), ("N", (False, "no"))]  (Just $ vegetarian init)

  <*> "tetanus_date" .: DF.string (Just $ tetanus_date init)
  <*> "diseases" .: optionalTextMaybeForm (Just $ diseases init)
  <*> "allergies" .: optionalTextMaybeForm (Just $ allergies init)
  <*> "medication_diet" .: optionalTextMaybeForm (Just $ medication_diet init)
  <*> "dietary_reqs" .: optionalTextMaybeForm (Just $ dietary_reqs init)
  <*> "faith_needs" .: optionalTextMaybeForm (Just $ faith_needs init)
  <*> "remedy_paracetamol" .: DF.choiceWith [("Y", (True, "yes")), ("N", (False, "no"))]  (Just $ remedy_paracetamol init)
  <*> "remedy_piriton" .: DF.choiceWith [("Y", (True, "yes")), ("N", (False, "no"))]  (Just $ remedy_piriton init)
  <*> "remedy_ibuprofen" .: DF.choiceWith [("Y", (True, "yes")), ("N", (False, "no"))]  (Just $ remedy_ibuprofen init)
  <*> "remedy_anthisan" .: DF.choiceWith [("Y", (True, "yes")), ("N", (False, "no"))]  (Just $ remedy_anthisan init)
  <*> (pure False) -- firearms


entryEditable :: Registration -> Bool
entryEditable registration = state registration `elem` ["M", "N", "I", "P"]
-- New, new from OS[M], Invited, Partially completed

-- debug code to unlock

handleUnlock :: String -> Handler B.Html
handleUnlock auth = do

  -- BUG?: this does not update the OCC modifier. It probably doesn't break things too much / at all, but it's a bit lame that this field is not being handled by the OCC code. There shouldn't be a conflict with OCC here ever, though, because we're always forcing the code to a specific value.
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



humanReadableState :: String -> String
humanReadableState "M" = "Imported from OSM - not invited"
humanReadableState "N" = "Manually added - not invited"
humanReadableState "I" = "Invited"
humanReadableState "P" = "Partially completed"
humanReadableState "C" = "Completed"
humanReadableState s = "UNKNOWN STATE: " ++ s


handleAdminTop :: User -> Handler B.Html
handleAdminTop _user = do

  c <- liftIO $ readConfig

  let ub = (fromString . urlbase) c

  let 
    registrantRow :: Registration -> B.Html
    registrantRow r = do
      let auth = (fromString . authenticator) r
      B.p $ do
        (B.toHtml . lastname) r
        ", "
        (B.toHtml . firstname) r
        ". "
        "  State: "
        (B.toHtml . humanReadableState . state) r
        " "
        (B.a ! BA.href (ub <> "/admin/sendInviteEmail/" <> auth))
            ("[Send invitation email to " <> (fromString . invite_email) r <> "]")
        " "
        (B.a ! BA.href (ub <> "/pdf/" <> auth))

            "[View permission form PDF]"

  registrants <- selectAll

  return $ do
    B.h1 "Admin page"
    B.p $ (B.a ! BA.href "/admin/invite")
        "Invite new attendee"
    B.p $ (B.a ! BA.href "/admin/csv")
        "Download CSV of all forms, completed and not-completed"
    B.hr
    B.h2 "Attendees in DB"
    mapM_ registrantRow registrants :: B.Html

handleSendInviteEmail :: String -> User -> Handler B.Html
handleSendInviteEmail auth _user = do
  sendInviteEmail auth

  [registrant] :: [Registration] <- selectByAuthenticator auth
  let s = state registrant

  when (s == "M" || s == "N") $
    void $ withDB $ \conn -> execute conn "UPDATE regmgr_attendee SET state = ? WHERE authenticator = ?" ("I" :: String, auth)

  return $ do
    B.h1 "Invitation submitted"
    B.p $ "An invitation was generated."
    B.p $ "A personalised invitation link has been included in the email."
    B.p $ do
      "The invitation link is also available here, if you would like to send it manually via a different method: "
      (B.a ! BA.href ("/register/" <> fromString auth))
        "link"


handleFiles :: Server Raw
handleFiles = serveDirectoryWebApp "./files"

jqueryHead :: B.Html
jqueryHead = do
  (B.script ! BA.src "/file/jquery-3.3.1.min.js") (return ())
  B.link ! BA.rel "stylesheet"
         ! BA.href "/file/jquery-ui-1.12.1/jquery-ui.min.css"
  (B.script ! BA.src "/file/jquery-ui-1.12.1/jquery-ui.min.js") (return ())


-- | Add a medication for the specified authenticator. Need to hand
--   over a plain
handleMedicationAddGet :: String -> Handler B.Html
handleMedicationAddGet authenticator = do
  view :: DF.View B.Html <- DF.getForm "Medication" (medicationDigestiveForm (blankMedicationForm authenticator))
  return (medicationHtml authenticator view Nothing)

-- | this should add a medication: save it to the database and then
-- redirect the user back to the summary page (or display the
-- summary page...)
handleMedicationAddPost :: String -> [(String,String)] -> Handler B.Html
handleMedicationAddPost auth reqBody = do

  f <- DF.postForm "Medication" (medicationDigestiveForm (blankMedicationForm auth)) (servantPathEnv reqBody)
  -- TODO ^ blankMedicationForm should be loaded from DB if this is an edit post.
  case f of
    (_, Just m) -> do
      liftIO $ withDB $ \conn -> 
        -- newDBTime <- dbNow conn -- no concurrency control per medication, which is a bit lame...

        execute conn "INSERT INTO regmgr_medication (attendee_authenticator, medication_name, medication_reason, medication_dosage, medication_notes, medication_required_before_breakfast, medication_required_with_breakfast, medication_required_after_breakfast, medication_required_before_lunch, medication_required_after_lunch, medication_required_before_dinner, medication_required_after_dinner, medication_required_bedtime, medication_required_as_required, medication_required_other) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
          (  (auth, medication_name m, medication_reason m, medication_dosage m, medication_notes m) 
          PG.:. (medication_required_before_breakfast m, medication_required_with_breakfast m, medication_required_after_breakfast m, medication_required_before_lunch m, medication_required_after_lunch m, medication_required_before_dinner m, medication_required_after_dinner m)
          PG.:. (medication_required_bedtime m, medication_required_as_required m, medication_required_other m)
          )

      -- if we go this far, we were successful at saving. Now redisplay
      -- the participant page
      handleRegistrationGet auth
    (view, Nothing) -> liftIO $ do
      putStrLn "Medication POST did not validate in digestive functor"
      return (medicationHtml auth view Nothing)


listOfMedications :: String -> [(Integer, Medication)] -> B.Html
listOfMedications auth meds = do
  B.hr
  B.h2 "Medications"
  case meds of
    [] -> B.p "No medications registered"
    _ -> mapM_ (renderMed auth) meds
  
  B.hr

renderMed :: String -> (Integer, Medication) -> B.Html
renderMed auth (i,m) = B.p $ do
  (B.toHtml . medication_name) m
  " - "
  (B.a ! BA.href ("/medication/delete/" <> fromString auth <> "/" <> fromString (show i)))
    "[DELETE]"
  " "
  (B.a ! BA.href ("/medication/edit/" <> fromString auth <> "/" <> fromString (show i)))
    "[EDIT]"

handleMedicationDeleteGet auth key = do
  liftIO $ do
    putStrLn $ "Deleting medication " ++ show auth ++ " / " ++ show key
    n <- withDB $ \conn -> PG.execute conn "DELETE FROM regmgr_medication WHERE attendee_authenticator = ? AND ident = ?" (auth, key)
    putStrLn $ "Medication delete removed " ++ show n ++ " rows."

  handleRegistrationGet auth


-- | c.f. handleMedicationAddGet - but instead of starting with a blank
--   form, we start with a medication read from the DB

handleMedicationEditGet auth key = do
  [medication] <- selectMedicationsByAuthAndKey auth key
  
  view :: DF.View B.Html <- DF.getForm "Medication" (medicationDigestiveForm medication)
  return (medicationHtml auth view (Just key))

handleMedicationEditPost auth key reqBody = do

  [medication] <- selectMedicationsByAuthAndKey auth key

  f <- DF.postForm "Medication" (medicationDigestiveForm medication) (servantPathEnv reqBody)

  case f of
    (_, Just val) -> do
      liftIO $ putStrLn $ "Medication edit submission OK " ++ (show key)

      liftIO $ withDB $ \conn -> 
        gupdateInto conn "regmgr_medication" "attendee_authenticator = ? AND ident = ?" val (auth, key)
      handleRegistrationGet auth
    (view, Nothing) -> do
      liftIO $ putStrLn $ "Medication edit submission error " ++ (show key)
      return (medicationHtml auth view (Just key))

