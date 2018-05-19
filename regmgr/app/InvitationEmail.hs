{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | for sending invitation emails.
-- with reference to http://czyzykowski.com/posts/ssl-email-haskell.html

module InvitationEmail where

import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromMaybe)
import Data.Monoid ( (<>) )
import Data.String (fromString)
import Data.Text (unpack, pack)
import Data.Text.Lazy (Text)
import qualified Data.Yaml as Y
import qualified Database.PostgreSQL.Simple.SOP as PGS
import GHC.Generics
import qualified Network.Socket as N
import Network.HaskellNet.Auth
import Network.HaskellNet.SMTP
import Network.HaskellNet.SMTP.SSL
import Network.Mail.Mime

import Config
import DB
import Registration

sendEmail :: Mail -> IO ()
sendEmail msg = do
  config <- readConfig

  rendered   <- renderMail' msg
  doSMTPSTARTTLSWithSettings (smtpServer config)
   defaultSettingsSMTPSTARTTLS { sslPort = smtpPort config,
                                 sslDisableCertificateValidation = True }
   $ \connection -> do
      succeeded  <- authenticate LOGIN
                                 (smtpUser config)
                                 (smtpPassword config)
                                 connection
      when succeeded $
          sendMail ((unpack . addressEmail) (mailFrom msg))
                   (map (unpack . addressEmail) (mailTo msg))
                   (toStrict rendered) connection

sendInviteEmail :: MonadIO io => String -> io ()
sendInviteEmail auth = liftIO $ do
  putStrLn $ "sending invitation email for " ++ auth

  -- THREE OR MORE! factor DB loading for registration.
  [registration] :: [Registration] <- withDB $ \conn -> withDB $ \conn -> PGS.gselectFrom conn "regmgr_attendee where authenticator=?" [auth]

  let fullname = (firstname registration) ++ " " ++ (lastname registration)

  ub <- (fromString . urlbase) <$> readConfig

  let url = ub <> "/register/" <> fromString auth

  let plaintext = plainPart $
          "Hello.\n"
       <> "Please complete this registration form online and then print, sign and return it to your scout leader.\n"
       <> url
 
  let htmltext = htmlPart $ "<p>Please complete this registration form online, and then print, sign and return it to your scout leader. "
                         <> "<a href=\"" <> url <> "\">Click here for registration form</a>.</p>"

  let mail = Mail {
    mailFrom = Address { addressName = Just "1st Merrow Scout Camp Registration"
                       , addressEmail = "scoutcamp@hawaga.org.uk"
                       }
  , mailTo = [Address { addressName = Nothing
                      , addressEmail = (pack $ invite_email registration)
                      }
             ]
  , mailCc = []
  , mailBcc = []
  , mailHeaders = [("Subject", "Scout camp permission form for " <> pack fullname)]
  , mailParts = [[plaintext, htmltext] ] -- should include HTML too with clickable link. double nested list to represent how mime is modelled.
  }

  sendEmail mail
  
  putStrLn $ "end of email send for " ++ auth
