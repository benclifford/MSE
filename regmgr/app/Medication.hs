{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}

module Medication where

import qualified GHC.Generics as GG
import qualified Generics.SOP as GS
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.SOP as PGS
import Database.PostgreSQL.Simple.Time as PG

import Data.Monoid ( (<>) )
import Data.String (fromString)

import qualified Text.Blaze.Html5 as B
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5.Attributes as BA

import qualified Text.Digestive as DF
import Text.Digestive ( (.:) )

import Text.Digestive.Blaze.Html5 as DB


import DigestiveBits

data Medication = Medication {

  attendee_authenticator :: String,

  medication_name :: String,
  medication_reason :: String,
  medication_dosage :: String,
  medication_notes :: String,

  medication_required_before_breakfast :: Bool,
  medication_required_with_breakfast :: Bool,
  medication_required_after_breakfast :: Bool,

  medication_required_before_lunch :: Bool,
  medication_required_after_lunch :: Bool,

  medication_required_before_dinner :: Bool,
  medication_required_after_dinner :: Bool,

  medication_required_bedtime :: Bool,

  medication_required_as_required :: Bool,

  medication_required_other :: Bool

} deriving (GG.Generic, Show)

instance PG.FromRow Medication
instance PG.ToRow Medication

instance GS.Generic Medication
instance GS.HasDatatypeInfo Medication

instance PGS.HasFieldNames Medication

blankMedicationForm authenticator = Medication {
  attendee_authenticator = authenticator,
  medication_name = "",
  medication_reason = "",
  medication_dosage = "",
  medication_notes = "",

  medication_required_before_breakfast = False,
  medication_required_with_breakfast = False,
  medication_required_after_breakfast = False,

  medication_required_before_lunch = False,
  medication_required_after_lunch = False,

  medication_required_before_dinner = False,
  medication_required_after_dinner = False,

  medication_required_bedtime = False,
  medication_required_as_required = False,
  medication_required_other = False
}

medicationDigestiveForm init = Medication <$>

      "attendee_authenticator" .: constString (attendee_authenticator init)

  <*> "medication_name" .: DF.string (Just $ medication_name init)
  <*> "medication_reason" .: DF.string (Just $ medication_name init)
  <*> "medication_dosage" .: DF.string (Just $ medication_name init)
  <*> "medication_notes" .: DF.string (Just $ medication_name init)

  <*> "medication_required_before_breakfast" .: DF.bool (Just $ medication_required_before_breakfast init)
  <*> "medication_required_with_breakfast" .: DF.bool (Just $ medication_required_with_breakfast init)
  <*> "medication_required_after_breakfast" .: DF.bool (Just $ medication_required_after_breakfast init)

  <*> "medication_required_before_lunch" .: DF.bool (Just $ medication_required_before_lunch init)
  <*> "medication_required_after_lunch" .: DF.bool (Just $ medication_required_after_lunch init)

  <*> "medication_required_before_dinner" .: DF.bool (Just $ medication_required_before_dinner init)
  <*> "medication_required_after_dinner" .: DF.bool (Just $ medication_required_after_dinner init)

  <*> "medication_required_bedtime" .: DF.bool (Just $ medication_required_bedtime init)

  <*> "medication_required_as_required" .: DF.bool (Just $ medication_required_as_required init)

  <*> "medication_required_other" .: DF.bool (Just $ medication_required_other init)

medicationHtml :: String -> DF.View B.Html -> B.Html
medicationHtml auth view = do
  B.h1 "Medication Information"

  B.p "Enter the details of one medication on this form. You can add another medication later."

  B.form
-- XXX TODO: this can't be /add/ URL when we are updating...
-- or perhaps /add/ URL needs to behave differently
-- Bool param for add vs update so we can render this and
-- add/update button differently?
    ! BA.action ("/medication/add/" <> fromString auth)
    ! BA.method "post"
    $ do
      B.p $ do
        DB.label "medication_name" view "Medication name"
        ": "
        DB.errorList "medication_name" view
        DB.inputText "medication_name" view
      B.p $ do
        DB.label "medication_reason" view "Reason for medication (eg asthma)"
        ": "
        DB.errorList "medication_reason" view
        DB.inputText "medication_reason" view
      B.p $ do
        DB.label "medication_dosage" view "Dosage (eg 1 pill twice daily)"
        ": "
        DB.errorList "medication_dosage" view
        DB.inputText "medication_dosage" view
      B.p $ do
        DB.label "medication_notes" view "Other notes"
        ": "
        DB.errorList "medication_notes" view
        DB.inputText "medication_notes" view
      B.p $ do
        "When should this medication be administered? Please tick all times it is required to be administered."
        B.br
        labelCheckbox "medication_required_before_breakfast" view "Before breakfast"
        B.br
        labelCheckbox "medication_required_with_breakfast" view "With breakfast"
        B.br
        labelCheckbox "medication_required_after_breakfast" view "After breakfast"
        B.br
        labelCheckbox "medication_required_before_lunch" view "Before lunch"
        B.br
        labelCheckbox "medication_required_after_lunch" view "After lunch"
        B.br
        labelCheckbox "medication_required_before_dinner" view "Before dinner"
        B.br
        labelCheckbox "medication_required_after_dinner" view "After dinner"
        B.br
        labelCheckbox "medication_required_bedtime" view "Bedtime"
        B.br
        labelCheckbox "medication_required_as_required" view "As required"
        B.br
        labelCheckbox "medication_required_other" view "Other (please add extra info in notes above)"
      DB.inputSubmit "Save this medication" 

labelCheckbox fieldName view description = do
  DB.label fieldName view description
  ": "
  DB.errorList fieldName view
  DB.inputCheckbox fieldName view
 

{-
invitationHtml :: DF.View B.Html -> B.Html
invitationHtml view = do
  B.h1 "Invite new attendee manually"
  B.p "Enter basic details of a new attendee here and an invitation will be emailed to them. Don't use this form to invite people who are already in the system."
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
      DB.inputSubmit "Invite new attendee" 

-}
