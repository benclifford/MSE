{-# Language ScopedTypeVariables #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}

-- | code to handle PDFs
module PDF where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BS 
import Database.PostgreSQL.Simple as PG
import Servant
import System.Process (callCommand)

import DB
import Registration

handlePDFForm :: String -> Handler BS.ByteString
handlePDFForm auth = do

  entry :: [Registration] <- withDB $ \conn -> PG.query conn "SELECT authenticator, state, modified, invite_email, firstname, lastname, dob, ec_1_name, ec_1_relationship, ec_1_address, ec_1_telephone, ec_1_mobile, ec_2_name, ec_2_relationship, ec_2_address, ec_2_telephone, ec_2_mobile, doctor_name, doctor_address, doctor_telephone, swim, vegetarian, tetanus_date, diseases, allergies, medication_diet, dietary_reqs, faith_needs FROM regmgr_attendee WHERE authenticator=?" [auth]

  let val = head entry -- assumes exactly one entry matches this authenticator. BUG: there might be none;  there might be more than 1 but that is statically guaranteed not to happen in the SQL schema (so checked by the SQL type system, not the Haskell type system) - so that's an 'error "impossible"' case.

  liftIO $ putStrLn $ "got sql result: " ++ show entry


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

  sed "authenticator" authenticator
  sed "modified" (show . modified)
  sed "invite_email" invite_email

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

  liftIO $ callCommand $ "pdflatex " ++ tempLatexFilename

  -- because we have to call latex a few times for page numbering
  liftIO $ callCommand $ "pdflatex " ++ tempLatexFilename
  liftIO $ callCommand $ "pdflatex " ++ tempLatexFilename
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

