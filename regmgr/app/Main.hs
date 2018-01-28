{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveGeneric #-}

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

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Data.String (fromString, IsString)

import Data.Monoid ( (<>) )

import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as UUID

import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Time as PG

import GHC.Generics (Generic)

import Network.Wai.Handler.Warp (run)

import Servant
import Servant.API
import Servant.HTML.Blaze as SB

import System.Process (callCommand)

import qualified Text.Blaze.Html5 as B
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5.Attributes as BA

import qualified Text.Digestive as DF
import Text.Digestive ( (.:) )

import Text.Digestive.Blaze.Html5 as DB

import Lib


type PingAPI =
  "ping" :> Get '[PlainText] String

type HTMLPingAPI =
  "htmlping" :> Get '[SB.HTML] B.Html


type InboundAuthenticatorAPI = "register" :> Capture "uuid" String :> Get '[HTML] B.Html

type UpdateFormAPI = "register" :> Capture "uuid" String :> ReqBody '[FormUrlEncoded] [(String,String)] :> Post '[HTML] B.Html

type PDFFormAPI = "pdf" :> Capture "auth" String :> Get '[PDF] BS.ByteString

type UnlockAPI = "unlock" :> Capture "auth" String :> Get '[HTML] B.Html

type InviteGetAPI = "admin" :> "invite" :> Get '[HTML] B.Html
type InvitePostAPI = "admin" :> "invite" :> ReqBody '[FormUrlEncoded] [(String, String)] :> Post '[HTML] B.Html


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

server1 :: Server API
server1 = handlePing :<|> handleInbound :<|> handleHTMLPing
  :<|> handleUpdateForm
  :<|> handlePDFForm
  :<|> handleUnlock
  :<|> handleInviteGet
  :<|> handleInvitePost

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
handleInbound auth = do
  liftIO $ putStrLn $ "handleInbound called, authenticator=" ++ auth

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

  entry :: [Registration] <- liftIO $ query conn "SELECT authenticator, state, modified, firstname, lastname, dob, ec_1_name, ec_1_relationship, ec_1_address, ec_1_telephone, ec_1_mobile, ec_2_name, ec_2_relationship, ec_2_address, ec_2_telephone, ec_2_mobile, doctor_name, doctor_address, doctor_telephone, swim, vegetarian, tetanus_date, diseases, allergies, medication_diet, dietary_reqs, faith_needs FROM regmgr_attendee WHERE authenticator=?" [auth]

  let val = head entry -- assumes exactly one entry matches this authenticator. BUG: there might be none;  there might be more than 1 but that is statically guaranteed not to happen in the SQL schema (so checked by the SQL type system, not the Haskell type system) - so that's an 'error "impossible"' case.

  liftIO $ putStrLn $ "got sql result: " ++ show entry

  liftIO $ putStrLn "closing db"
  liftIO $ PG.close conn 

  let title = "Registration for " <> B.toHtml (firstname val) <> " " <> B.toHtml (lastname val)

  view :: DF.View B.Html <- DF.getForm "Registration" (registrationDigestiveForm val)

  let editable = entryEditable val

  let outputHtml =
        B.docTypeHtml $ do
          B.head $ do
            B.title title
          B.body $ do
            B.h1 title
            B.p "debug: handleInbound"
            regformHtml auth view editable

  liftIO $ putStrLn "end of req"

  return outputHtml

-- | generates an HTML view of this form, which may be editable
--   or fixed text.
regformHtml :: String -> DF.View B.Html -> Bool -> B.Html
regformHtml auth view editable = do
            if editable
              then B.p "Please fill out this registration form. We have put information that we know already into the form, but please check and correct those if that information is wrong."
              else do
                B.p "This is the information held for this attendee. Please contact your section leader if any of this information is incorrect."
                B.p $ do
                  "If you have not done so already, "
                  (B.a ! BA.href ("/pdf/" <> fromString auth))
                    "please print out and sign the permission form"
                  "."
                B.p $ do
                  "debug: "
                  (B.a ! BA.href ("/unlock/" <> fromString auth))
                    "admin mode: unlock this participant for further edits"

            -- QUESTION/DISCUSSION: type_ has to have a different name with an underscore because type is a reserved word.
            B.form ! BA.action ("/register/" <> (fromString auth))
                   ! BA.method "post"
             $ do

              -- QUESTION/DISCUSSION: the authenticator lets us find the relevant record (it's a combo of identifier and authenticator...) and the modified value lets us do OCC transaction handling.
              DB.inputHidden "authenticator" view
              DB.inputHidden "modified" view

              textInputParagraph editable "firstname" view "First name"
              textInputParagraph editable "lastname" view "Family name"

              -- QUESTION/DISCUSSION: this could be a date picker on the client side in javascript?
              textInputParagraph editable "dob" view "Date of Birth"
              B.hr
              B.p "Emergency Contact 1"
              textInputParagraph editable "ec_1_name" view "Name"
              textInputParagraph editable "ec_1_relationship" view "Relationship"
              textInputParagraph editable "ec_1_address" view "Address"
              textInputParagraph editable "ec_1_telephone" view "Telephone"
              textInputParagraph editable "ec_1_mobile" view "Mobile telephone"

              B.hr
              B.p "Emergency Contact 2"
              textInputParagraph editable "ec_2_name" view "Name"
              textInputParagraph editable "ec_2_relationship" view "Relationship"
              textInputParagraph editable "ec_2_address" view "Address"
              textInputParagraph editable "ec_2_telephone" view "Telephone"
              textInputParagraph editable "ec_2_mobile" view "Mobile telephone"

              B.hr
              B.p "Doctor / GP"
              textInputParagraph editable "doctor_name" view "Name"
              textInputParagraph editable "doctor_address" view "Address"
              textInputParagraph editable "doctor_telephone" view "Telephone"

              B.hr
              boolInputParagraph editable "swim" view "Can participant swim?"
              boolInputParagraph editable "vegetarian" view "Is participant vegetarian?"
              B.hr
              B.p "Medical information"
              textInputParagraph editable "tetanus_date" view "Date of last tetanus"
              textInputParagraph editable "diseases" view "Details of any infections/diseases"
              textInputParagraph editable "allergies" view "Details of any allergies"
              textInputParagraph editable "medication_diet" view "Details of any dietary requirements"
              textInputParagraph editable "faith_needs" view "Details of any faith/cultural needs (eg dress, diet, holy days, toilet arrangements)"
             

              when editable $ DB.inputSubmit "Register for event"
            B.hr
{-
            B.h2 "Internal debugging information"
            B.p $ "Last modified: " <> B.toHtml (show (modified val))
            B.p $ "State: " <> B.toHtml (state val)
-}

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


textInputParagraph editable fieldName view description = 
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

-- | loosely based on inputText source
readonlyInputText :: T.Text -> DF.View v -> B.Html
readonlyInputText ref view = B.toHtml $ DF.fieldInputText ref view

readonlyInputBool :: T.Text -> DF.View v -> B.Html
readonlyInputBool ref view = B.toHtml $ DF.fieldInputBool ref view

{-
inputText ref view = H.input
    ! A.type_ "text"
    ! A.id    (H.toValue ref')
    ! A.name  (H.toValue ref')
    ! A.value (H.toValue $ fieldInputText ref view)
  where
    ref' = absoluteRef ref view
-}


-- | [(String, String)] is a req body deserialisation - that might not
--   be the best type - I think servant can deserialise to various
--   forms (so potentially could deserialise directly to an Env
--   without with helper being explicit... instead the same helper
--   code would go in typeclass impl?)
servantPathEnv :: Monad m => [(String, String)] -> DF.FormEncType -> m (DF.Env m)
servantPathEnv reqBody ty = do

  let env :: Monad n => DF.Env n
      env path = do
        let pathAsString = (T.unpack . DF.fromPath) path
        let v' = lookup pathAsString reqBody -- BUG? might be multiple values? the digestive-functor docs suggest some input types (selectors?) do that somehow?
        case v' of Nothing -> return []
                   Just v -> return [DF.TextInput $ T.pack v]
        -- This case could be a maybeToList then a map T.pack?
        -- is that clearer or less clear?

  return env


handleUpdateForm :: String -> [(String,String)] -> Handler B.Html
handleUpdateForm auth reqBody = do
-- TODO: factor
  liftIO $ putStrLn "opening db"
  conn <- liftIO $ PG.connectPostgreSQL "user='postgres'"  -- TODO: this shoudl be in a bracket with close to handle exceptions happening before the close that kill this handler but don't kill the whole process.

  liftIO $ putStrLn $ "Post params: " ++ show reqBody

{-
  -- QUESTION/DISCUSSION this is an ugly way to find the actual DB
  -- record in so much as we have to know about the way in which
  -- digestive-functor names its fields.

  let (Just auth) = lookup "Registration.authenticator" reqBody
  liftIO $ putStrLn $ "Read authenticator from POST: " ++ auth
-}

  entry :: [Registration] <- liftIO $ query conn "SELECT authenticator, state, modified, firstname, lastname, dob, ec_1_name, ec_1_relationship, ec_1_address, ec_1_telephone, ec_1_mobile, ec_2_name, ec_2_relationship, ec_2_address, ec_2_telephone, ec_2_mobile, doctor_name, doctor_address, doctor_telephone, swim, vegetarian, tetanus_date, diseases, allergies, medication_diet, dietary_reqs, faith_needs FROM regmgr_attendee WHERE authenticator=?" [auth]

  let val = head entry -- assumes exactly one entry matches this authenticator. BUG: there might be none;  there might be more than 1 but that is statically guaranteed not to happen in the SQL schema (so checked by the SQL type system, not the Haskell type system) - so that's an 'error "impossible"' case.

  liftIO $ putStrLn $ "got sql result: " ++ show entry
  liftIO $ putStrLn "closing db"
  liftIO $ PG.close conn 
  let title = "Registration for " <> B.toHtml (firstname val) <> " " <> B.toHtml (lastname val)

  let editable = entryEditable val

-- TODO: end factor
  f <- DF.postForm "Registration" (registrationDigestiveForm val) (servantPathEnv reqBody)

  outputHtml <- case f of
    (view, Just val) -> do

        liftIO $ putStrLn "Updating DB"
        -- write out 'val' to the database
        conn <- liftIO $ PG.connectPostgreSQL "user='postgres'" 

        -- we'll ask the database for the current time, rather than
        -- caring about local system time. TODO factor
        [[newDBTime]] :: [[PG.ZonedTimestamp]] <- liftIO $ query conn "SELECT NOW()" ()
 
        liftIO $ putStrLn $ "new SQL database time: " ++ show newDBTime
       
        let oldDBTime = modified val 
        liftIO $ putStrLn $ "old SQL database time: " ++ show oldDBTime
        let val' = val { modified = newDBTime, state = "C" }

        sqlres <- liftIO $ execute conn "UPDATE regmgr_attendee SET authenticator = ?, state = ?, modified = ?, firstname = ?, lastname= ?, dob=?, ec_1_name = ?, ec_1_relationship = ?, ec_1_address = ?, ec_1_telephone = ?, ec_1_mobile = ?, ec_2_name =?, ec_2_relationship = ?, ec_2_address = ?, ec_2_telephone = ?, ec_2_mobile = ?, doctor_name = ?, doctor_address = ?, doctor_telephone = ?, swim = ?, vegetarian = ?, tetanus_date = ?, diseases = ?, allergies = ?, medication_diet = ?, dietary_reqs = ?, faith_needs = ? WHERE authenticator = ? AND modified = ?" (val' PG.:. (auth, oldDBTime))

        liftIO $ putStrLn $ "SQL UPDATE returned: " ++ (show sqlres)

        liftIO $ close conn

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

app = serve regmgrAPI server1

main :: IO ()
main = do
  putStrLn "regmgr start"

  run 8080 app

  putStrLn "regmgr end"


-- | This models the registration form and could have type classes
--   attached for conversions to/from different types.
data Registration = Registration {
    authenticator :: String,
    state :: String,
    modified :: PG.ZonedTimestamp, -- TODO: debate with self about whether modified should be in the Registration or not as it isn't a traditional "editable" field but instead metadata about the record (like the primary key)
    firstname :: String,
    lastname :: String,
    dob :: String,
    ec_1_name :: String,
    ec_1_relationship :: String,
    ec_1_address :: String,
    ec_1_telephone :: String,
    ec_1_mobile :: String,
    ec_2_name :: String,
    ec_2_relationship :: String,
    ec_2_address :: String,
    ec_2_telephone :: String,
    ec_2_mobile :: String,
    doctor_name :: String,
    doctor_address :: String,
    doctor_telephone :: String,
    swim :: Bool,
    vegetarian :: Bool,
    tetanus_date :: String,
    diseases :: String,
    allergies :: String,
    medication_diet :: String,
    dietary_reqs :: String,
    faith_needs :: String
  } deriving (Generic, Show)


instance FromRow Registration
instance ToRow Registration

-- | This is the registration form as a Digestive Functor form
registrationDigestiveForm :: Monad m => Registration -> DF.Form B.Html m Registration
registrationDigestiveForm init = Registration
  <$> "authenticator" .: DF.string (Just $ authenticator init) -- TODO: a validator on this should check that the form value matches up with the value in init - which will, for example, have been read from the db
  <*> "state" .: DF.string (Just $ state init) -- TODO: a validator on this should check that the form value matches up with the value in init, which will for example, have come from the DB. Or is perhaps also nullable to allow creation of entries. Or perhaps to check we are doing the right kind of state progression so that record can't be moved into wrong state by rogue HTTP request?
  <*> "modified" .: (read <$> (DF.string (Just $ show $ modified init))) -- BUG: ignores modified time from client! which means OCC is broken as we always default to the latest version and so mostly don't ever hit a conflict. And the problem here is that we need to serialise it out to a form field and then parse it back in later, which is troublesome.
  <*> "firstname" .: DF.string (Just $ firstname init)
  <*> "lastname" .: DF.string (Just $ lastname init)
  <*> "dob" .: nonEmptyString (Just $ dob init)

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




-- this is DF in general, not specific to registrations so maybe
-- in own module?
-- stolen from ocharles 24 days of hackage
nonEmptyString :: (IsString v, Monoid v, Monad m) => Maybe String -> DF.Form v m String
nonEmptyString def = 
    (DF.check "This field must not be empty" (/= ""))
  $ DF.string def




entryEditable :: Registration -> Bool
entryEditable registration = state registration `elem` ["N", "I"]

-- debug code to unlock

handleUnlock :: String -> Handler B.Html
handleUnlock auth = do
  conn <- liftIO $ PG.connectPostgreSQL "user='postgres'" 

  sqlres <- liftIO $ execute conn "UPDATE regmgr_attendee SET state = ? WHERE authenticator = ?" ("N" :: String, auth)

  liftIO $ close conn

  handleInbound auth


-- code to handle PDFs

handlePDFForm :: String -> Handler BS.ByteString
handlePDFForm auth = do

  -- TODO: factor this read-from-DB

  liftIO $ putStrLn "opening db"
  conn <- liftIO $ PG.connectPostgreSQL "user='postgres'" 

  entry :: [Registration] <- liftIO $ query conn "SELECT authenticator, state, modified, firstname, lastname, dob, ec_1_name, ec_1_relationship, ec_1_address, ec_1_telephone, ec_1_mobile, ec_2_name, ec_2_relationship, ec_2_address, ec_2_telephone, ec_2_mobile, doctor_name, doctor_address, doctor_telephone, swim, vegetarian, tetanus_date, diseases, allergies, medication_diet, dietary_reqs, faith_needs FROM regmgr_attendee WHERE authenticator=?" [auth]

  let val = head entry -- assumes exactly one entry matches this authenticator. BUG: there might be none;  there might be more than 1 but that is statically guaranteed not to happen in the SQL schema (so checked by the SQL type system, not the Haskell type system) - so that's an 'error "impossible"' case.

  liftIO $ putStrLn $ "got sql result: " ++ show entry

  liftIO $ putStrLn "closing db"
  liftIO $ PG.close conn 


  -- SECURITY BUG: sanitation: auth is passed to external programs
  -- but has been read over the wire. Could contain shell commands
  -- or reads of root files.

  -- we could actually use some arbitrary non-network-id here as
  -- it is only used locally within this file. This would avoid a
  -- race condition if we load PDF URL twice at once.

  let tempLatexFilename = auth ++ ".latex"
  let tempPDFFilename = auth ++ ".pdf"

  liftIO $ callCommand $ "cp regform.latex.template " ++ tempLatexFilename

  -- TODO: now some sed in-place commands or something like that?
  -- be aware that there are likely to be text fields with interesting
  -- content (such as medical descriptions) that won't naturally fit
  -- into a short 80-col commandline, and will have symbols that screw
  -- stuff up for substitution commands

  let sed name accessor = liftIO $ callCommand $ "sed -i 's~[+{]" ++ name ++ "[+}]~" ++ accessor val ++ "~' " ++ tempLatexFilename 

  sed "firstname" firstname
  sed "lastname" lastname
  sed "dob" dob

  sed "ec-1-name" ec_1_name
  sed "ec-1-relationship" ec_1_relationship
  sed "ec-1-address" ec_1_address
  sed "ec-1-telephone" ec_1_telephone
  sed "ec-1-mobile" ec_1_mobile

  sed "ec-2-name" ec_2_name
  sed "ec-2-relationship" ec_2_relationship
  sed "ec-2-address" ec_2_address
  sed "ec-2-telephone" ec_2_telephone
  sed "ec-2-mobile" ec_2_mobile

  sed "doctor-name" doctor_name
  sed "doctor-address" doctor_address
  sed "doctor-telephone" doctor_telephone

  -- TODO: this probably needs formatting as Yes/No rather than true/false
  sed "swim" (show . swim)
  sed "vegetarian" (show . vegetarian)

  sed "tetanus-date" tetanus_date
  sed "diseases" diseases
  sed "allergies" allergies
  sed "medication-diet" medication_diet
  sed "dietary-reqs" dietary_reqs
  sed "faith-needs" faith_needs

  {-
  liftIO $ callCommand $ "sed -i 's/{firstname}/" ++ firstname val ++ "/' " ++ tempLatexFilename
  liftIO $ callCommand $ "sed -i 's/{lastname}/" ++ lastname val ++ "/' " ++ tempLatexFilename
  liftIO $ callCommand $ "sed -i 's/{dob}/" ++ dob val ++ "/' " ++ tempLatexFilename
  -}

  liftIO $ callCommand $ "pdflatex " ++ tempLatexFilename

  -- Note this is a lazy read, so can't delete temporary files if they
  -- are then returned.
  content <- liftIO $ BS.readFile tempPDFFilename
  return content

-- PDF content type
-- only "renderable" from a bytestring - on the assumption that we'll
-- only be reading them from a file that is generated separately from
-- the Servant mechanics.

data PDF

instance Accept PDF where
  contentType _ = "application/pdf"

instance MimeRender PDF BS.ByteString
  where
    mimeRender _ bs = bs

-- ======== invitations ============
-- Invitations by name, rather than from OSM.

-- There will be an invitation form for the purposes of
-- digestive-functors; but it won't be stored as such in
-- the database. Instead, it will be used to create a new
-- Registration record in 'N' state.

-- TODO: use haskell modules to get rid of inv_ prefix
data Invitation = Invitation {
    inv_firstname :: String,
    inv_lastname :: String
  }

invitationDigestiveForm :: Monad m => DF.Form B.Html m Invitation
invitationDigestiveForm = Invitation
  <$> "firstname" .: nonEmptyString Nothing
  <*> "lastname" .: nonEmptyString Nothing

invitationHtml :: DF.View B.Html -> B.Html
invitationHtml view = do
  B.h1 "Invite new participant manually"
  B.p "Enter basic details of a new participant here and a registration link will be generated for them to complete"
  B.form
    ! BA.action "/admin/invite"
    ! BA.method "post"
    $ do
      B.p $ do
        DB.label "firstname" view "First name"
        ": "
        DB.errorList "firstname" view
        DB.inputText "firstname" view
      B.p $ do
        DB.label "lastname" view "Family name"
        ": "
        DB.errorList "lastname" view
        DB.inputText "lastname" view
      DB.inputSubmit "Invite participant" 


handleInviteGet :: Handler B.Html
handleInviteGet = do
  view :: DF.View B.Html <- DF.getForm "Invitation" invitationDigestiveForm
  return (invitationHtml view)


handleInvitePost :: [(String, String)] -> Handler B.Html
handleInvitePost reqBody = do
  f <- DF.postForm "Invitation" invitationDigestiveForm (servantPathEnv reqBody)
  case f of
    (view, Just value) -> do
      liftIO $ putStrLn "Invitation validated in digestive functor"
      auth <- liftIO $ invite value
      return $ do
        B.h1 "Invitation submitted"
        B.p $ "An invitation was generated for " <> (B.toHtml $ inv_firstname value) <> " " <> (B.toHtml $ inv_lastname value)
        B.p $ do
          "Please ask the participant to complete the form "
          (B.a ! BA.href ("/register/" <> fromString auth))
            "the form at this URL"

    (view, Nothing) -> do
      liftIO $ putStrLn "Invitation POST did not validate in digestive functor"
      return (invitationHtml view)

invite :: Invitation -> IO String
invite inv = do

  uuid <- nextRandom
  let auth = UUID.toString uuid

  liftIO $ putStrLn "opening db"
  conn <- liftIO $ PG.connectPostgreSQL "user='postgres'" 

  [[newDBTime]] :: [[PG.ZonedTimestamp]] <- liftIO $ query conn "SELECT NOW()" ()

  liftIO $ execute conn "INSERT INTO regmgr_attendee (authenticator, state, modified, firstname, lastname) VALUES (?,?,?,?,?)"
    (auth, "N" :: String, newDBTime, inv_firstname inv, inv_lastname inv)
  close conn

  return auth

