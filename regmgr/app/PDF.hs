{-# Language ScopedTypeVariables #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language GADTs #-}
{-# Language FlexibleInstances #-}
{-# Language DefaultSignatures #-}

-- | code to handle PDFs
module PDF where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BS 
import qualified Data.Text.Lazy.IO as TIO
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.SOP as PGS
import Database.PostgreSQL.Simple.Time as PGT
import qualified Data.Text as T
import qualified Generics.SOP as GS
import Generics.SOP ( NP( (:*) ) )
import Servant
import System.Process (callCommand)
import qualified Text.EDE as E
import Text.EDE ( (.=) )

import DB
import Registration

handlePDFForm :: String -> Handler BS.ByteString
handlePDFForm auth = do

  -- TODO: factor with selectByAuthenticator in app/Main.hs
  entry <- withDB $ \conn -> PGS.gselectFrom conn "regmgr_attendee where authenticator=?" [auth]

  let (val :: Registration) = head entry -- assumes exactly one entry matches this authenticator. BUG: there might be none;  there might be more than 1 but that is statically guaranteed not to happen in the SQL schema (so checked by the SQL type system, not the Haskell type system) - so that's an 'error "impossible"' case.

  liftIO $ putStrLn $ "got sql result: " ++ show entry


  -- SECURITY BUG: sanitation: auth is passed to external programs
  -- but has been read over the wire. Could contain shell commands
  -- or reads of root files.

  -- we could actually use some arbitrary non-network-id here as
  -- it is only used locally within this file. This would avoid a
  -- race condition if we load PDF URL twice at once.

  let tempLatexFilename = auth ++ ".latex"
  let tempPDFFilename = auth ++ ".pdf"

{-
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

  sed "registrant-address" registrant_address
  sed "registrant-telephone" registrant_telephone

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
-}

  te <- liftIO $ E.parseFileWith E.alternateSyntax "regform.latex.template"
  let template = either (\msg -> error $ "reading template: " ++ msg) (id) (E.eitherResult te)

  -- let object = E.fromPairs [ "authenticator" .= authenticator val ]
  let object = gvals val

  liftIO $ putStrLn $ "template object = " ++ show object

  let re = E.eitherRender template object

  let result = either (\msg -> error $ "rendering template: " ++ msg) (id) re

  liftIO $ TIO.writeFile tempLatexFilename result

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

-- gvals :: Registration -> _object_from_aeson
gvals reg = E.fromPairs l
  where l = map (\(k,v) -> T.pack k .= v) (l2 `zip` l3)
        l2 = names (GS.Proxy :: GS.Proxy Registration) :: [String]
        l3 = values reg

values :: Registration -> [String]
values v =
 case GS.from v of
   GS.SOP (GS.Z xs) -> GS.hcollapse (GS.hcliftA showProxy (GS.K . showForLatex . GS.unI) xs)

  where showProxy = Proxy :: Proxy ShowForLatex

class ShowForLatex a where
  showForLatex :: a -> String

  default showForLatex :: (Show a) => a -> String
  showForLatex = escapeLatex . show

instance ShowForLatex [Char] where
  showForLatex s = escapeLatex s

instance ShowForLatex PGT.ZonedTimestamp

instance ShowForLatex (Maybe Integer)

instance ShowForLatex Bool

escapeLatex :: String -> String
escapeLatex v = concat $ map c v
  where
    c '&' = "\\&"
    c anything = [anything]

-- from postgres-simple-sop
names :: GS.HasDatatypeInfo a => Proxy a -> [String]
names p = case GS.datatypeInfo p of
    GS.ADT     _ _ cs -> fNms cs
    GS.Newtype _ _ c -> fNms $ c :* GS.Nil

fNms :: GS.NP GS.ConstructorInfo a -> [String]
fNms ((GS.Record _ fs) :* _) = fNmsRec fs

fNmsRec :: GS.NP GS.FieldInfo a -> [String]
fNmsRec GS.Nil = []
fNmsRec (GS.FieldInfo nm :* rest) = nm : fNmsRec rest

