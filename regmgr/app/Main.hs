{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveGeneric #-}

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

import qualified Data.Text as T
import Data.String (fromString, IsString)

import Data.Monoid ( (<>) )

import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Time as PG

import GHC.Generics (Generic)

import Network.Wai.Handler.Warp (run)

import Servant
import Servant.API
import Servant.HTML.Blaze as SB

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


-- TODO: the base URLs for this and updateFormAPI can be the same with
-- one being for GET and one being for POST.
-- That way, it is still bookmarkable, and we can pass the UUID in the
-- URL both times rather than extracting from form data
type InboundAuthenticatorAPI = "register" :> Capture "uuid" String :> Get '[HTML] B.Html

type UpdateFormAPI = "register" :> Capture "uuid" String :> ReqBody '[FormUrlEncoded] [(String,String)] :> Post '[HTML] B.Html

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

  entry :: [Registration] <- liftIO $ query conn "SELECT authenticator, state, modified, firstname, lastname, dob FROM regmgr_attendee WHERE authenticator=?" [auth]

  let val = head entry -- assumes exactly one entry matches this authenticator. BUG: there might be none;  there might be more than 1 but that is statically guaranteed not to happen in the SQL schema (so checked by the SQL type system, not the Haskell type system) - so that's an 'error "impossible"' case.

  liftIO $ putStrLn $ "got sql result: " ++ show entry

  liftIO $ putStrLn "closing db"
  liftIO $ PG.close conn 

  let title = "Registration for " <> B.toHtml (firstname val) <> " " <> B.toHtml (lastname val)

  view :: DF.View B.Html <- DF.getForm "Registration" (registrationDigestiveForm val)

  let outputHtml =
        B.docTypeHtml $ do
          B.head $ do
            B.title title
          B.body $ do
            B.h1 title
            B.p "debug: handleInbound"
            regformHtml auth view

  liftIO $ putStrLn "end of req"

  return outputHtml

regformHtml :: String -> DF.View B.Html -> B.Html
regformHtml auth view = do
            B.p "Please fill out this registration form. We have put information that we know already into the form, but please check and correct those if that information is wrong."
            -- QUESTION/DISCUSSION: type_ has to have a different name with an underscore because type is a reserved word.
            B.form ! BA.action ("/register/" <> (fromString auth))
                   ! BA.method "post"
             $ do

              -- QUESTION/DISCUSSION: the authenticator lets us find the relevant record (it's a combo of identifier and authenticator...) and the modified value lets us do OCC transaction handling.
              DB.inputHidden "authenticator" view
              DB.inputHidden "modified" view

              textInputParagraph "firstname" view "First name"
              textInputParagraph "lastname" view "Family name"

              -- QUESTION/DISCUSSION: this could be a date picker on the client side in javascript?
              textInputParagraph "dob" view "Date of Birth"

              DB.inputSubmit "Register for event"
            B.hr
{-
            B.h2 "Internal debugging information"
            B.p $ "Last modified: " <> B.toHtml (show (modified val))
            B.p $ "State: " <> B.toHtml (state val)
-}

textInputParagraph fieldName view description = 
              B.p $ do
                DB.label fieldName view description
                DB.errorList fieldName view
                DB.inputText fieldName view


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

  entry :: [Registration] <- liftIO $ query conn "SELECT authenticator, state, modified, firstname, lastname, dob FROM regmgr_attendee WHERE authenticator=?" [auth]

  let val = head entry -- assumes exactly one entry matches this authenticator. BUG: there might be none;  there might be more than 1 but that is statically guaranteed not to happen in the SQL schema (so checked by the SQL type system, not the Haskell type system) - so that's an 'error "impossible"' case.

  liftIO $ putStrLn $ "got sql result: " ++ show entry
  liftIO $ putStrLn "closing db"
  liftIO $ PG.close conn 
  let title = "Registration for " <> B.toHtml (firstname val) <> " " <> B.toHtml (lastname val)

-- TODO: end factor
  f <- DF.postForm "Registration" (registrationDigestiveForm val) (servantPathEnv reqBody)

  outputHtml <- case f of
    (view, Just val) -> do

        liftIO $ putStrLn "Updating DB"
        -- write out 'val' to the database
        conn <- liftIO $ PG.connectPostgreSQL "user='postgres'" 

        -- liftIO $ (putStrLn . show) =<< formatQuery conn "UPDATE regmgr_attendee SET authenticator = ?, state = ?, modified = ?, firstname = ?, lastname= ?, dob=? WHERE authenticator = ?" (val PG.:. [auth]) -- holy bracketing
        sqlres <- liftIO $ execute conn "UPDATE regmgr_attendee SET authenticator = ?, state = ?, modified = ?, firstname = ?, lastname= ?, dob=? WHERE authenticator = ?" (val PG.:. [auth])


        liftIO $ putStrLn $ "SQL UPDATE returned: " ++ (show sqlres)

        -- entry :: [Registration] <- liftIO $ query conn "SELECT authenticator, state, modified, firstname, lastname, dob FROM regmgr_attendee WHERE authenticator=?" [auth]

        -- TODO: beware OCC modification bugs here
        liftIO $ close conn

        return $
          B.docTypeHtml $ do
            B.head $ do
              B.title title
            B.body $ do
              B.h1 title
              B.p "debug: handleUpdateForm - there are no errors"
              B.p "next steps: print PDF (link here), sign and give to a leader with payment"
              B.p $ B.toHtml $ show val

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
              regformHtml auth view
               
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
    dob :: String
  } deriving (Generic, Show)


instance FromRow Registration
instance ToRow Registration

-- | This is the registration form as a Digestive Functor form
registrationDigestiveForm :: Monad m => Registration -> DF.Form B.Html m Registration
registrationDigestiveForm init = Registration
  <$> "authenticator" .: DF.string (Just $ authenticator init) -- TODO: a validator on this should check that the form value matches up with the value in init - which will, for example, have been read from the db
  <*> "state" .: DF.string (Just $ state init) -- TODO: a validator on this should check that the form value matches up with the value in init, which will for example, have come from the DB. Or is perhaps also nullable to allow creation of entries.
  <*> "modified" .: ((const (modified init)) <$> (DF.string (Just $ show $ modified init))) -- BUG: ignores modified time from client!
  <*> "firstname" .: DF.string (Just $ firstname init)
  <*> "lastname" .: DF.string (Just $ lastname init)
  <*> "dob" .: nonEmptyString (Just $ dob init)

nonEmptyString :: (IsString v, Monoid v, Monad m) => Maybe String -> DF.Form v m String
nonEmptyString def = 
    (DF.check "This field must not be empty" (/= ""))
  $ DF.string def

