{-# Language ScopedTypeVariables #-}
{-# Language OverloadedStrings #-}

-- ======== invitations ============
-- Invitations by name, rather than from OSM.

-- There will be an invitation form for the purposes of
-- digestive-functors; but it won't be stored as such in
-- the database. Instead, it will be used to create a new
-- Registration record in 'N' state.

module Invitation where

import Control.Monad.IO.Class (liftIO)

import Data.Monoid ( (<>) )
import Data.String (fromString, IsString)
import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as UUID

import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Time as PG

import qualified Text.Blaze.Html5 as B
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5.Attributes as BA

import qualified Text.Digestive as DF
import Text.Digestive ( (.:) )
import Text.Digestive.Blaze.Html5 as DB

import Servant

import DB
import DigestiveBits
import DigestiveServant
import InvitationEmail
import Lib (User)



-- TODO: use haskell modules to get rid of inv_ prefix
data Invitation = Invitation {
    inv_firstname :: String,
    inv_lastname :: String,
    inv_email :: String
  }

invitationDigestiveForm :: Monad m => DF.Form B.Html m Invitation
invitationDigestiveForm = Invitation
  <$> "firstname" .: nonEmptyString Nothing
  <*> "lastname" .: nonEmptyString Nothing
  <*> "email" .: nonEmptyString Nothing

invitationHtml :: DF.View B.Html -> B.Html
invitationHtml view = do
  B.h1 "Invite new participant manually"
  B.p "Enter basic details of a new participant here and an invitation will be emailed to them. Don't use this form to invite people who are already in the system."
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
      B.p $ do
        DB.label "email" view "Email"
        ": "
        DB.errorList "email" view
        DB.inputText "email" view
      DB.inputSubmit "Invite new participant" 


handleInviteGet :: User -> Handler B.Html
handleInviteGet user = do
  view :: DF.View B.Html <- DF.getForm "Invitation" invitationDigestiveForm
  return (invitationHtml view)


handleInvitePost :: [(String, String)] -> User -> Handler B.Html
handleInvitePost reqBody user = do
  f <- DF.postForm "Invitation" invitationDigestiveForm (servantPathEnv reqBody)
  case f of
    (_, Just value) -> do
      liftIO $ putStrLn "Invitation validated in digestive functor"
      auth <- liftIO $ invite value
      sendInviteEmail auth
      return $ do
        B.h1 "Invitation submitted"
        B.p $ "An invitation was generated for " <> (B.toHtml $ inv_firstname value) <> " " <> (B.toHtml $ inv_lastname value)
        B.p $ "A personalised invitation link has been included in the email."
        B.p $ do
          "For debugging purposes, the link is also here:"
          (B.a ! BA.href ("/register/" <> fromString auth))
            "link"

    (view, Nothing) -> do
      liftIO $ putStrLn "Invitation POST did not validate in digestive functor"
      return (invitationHtml view)

invite :: Invitation -> IO String
invite inv = do

  uuid <- nextRandom
  let auth = UUID.toString uuid

  withDB $ \conn -> do
    newDBTime <- dbNow conn

    execute conn "INSERT INTO regmgr_attendee (authenticator, state, modified, firstname, lastname, invite_email) VALUES (?,?,?,?,?,?)"
      (auth, "N" :: String, newDBTime, inv_firstname inv, inv_lastname inv, inv_email inv)

  return auth


