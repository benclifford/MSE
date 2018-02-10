{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | for sending invitation emails.
-- with reference to http://czyzykowski.com/posts/ssl-email-haskell.html

module InvitationEmail where

import Control.Monad
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import Data.Text.Lazy (Text)
import qualified Data.Yaml as Y
import GHC.Generics
import qualified Network.Socket as N
import Network.HaskellNet.Auth
import Network.HaskellNet.SMTP
import Network.HaskellNet.SMTP.SSL
import Network.Mail.Mime

sendTop = sendEmail $
  Mail {
    mailFrom = Address { addressName = Just "1st Merrow Scout Camp"
                       , addressEmail = "scoutcamp@hawaga.org.uk"
                       }
  , mailTo = [Address { addressName = Just "Ben Clifford"
                      , addressEmail = "benc@hawaga.org.uk"
                      }
             ]
  , mailCc = []
  , mailBcc = []
  , mailHeaders = [("Subject", "Scout camp invitation for TODONAME")]
  , mailParts = [[plainPart "HELLO"]] -- should include HTML too with clickable link. double nested list to represent how mime is modelled.
  }

data MailConfig = MailConfig {
    smtpServer :: String
  , smtpPort :: N.PortNumber
  , smtpUser :: String
  , smtpPassword :: String
  } deriving Generic

instance Y.FromJSON MailConfig

instance Y.FromJSON N.PortNumber
  where parseJSON v = read <$> Y.parseJSON v
-- orphan instance... but no good story here about it not being orphan.

readConfig :: IO MailConfig
readConfig = do
   c <- Y.decodeFileEither "config.yaml"
   case c of 
     Right v -> return v
     Left e -> error $ "Cannot parse config file: " ++ show e

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
