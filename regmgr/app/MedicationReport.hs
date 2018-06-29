{-# Language OverloadedStrings #-}
{-# Language TypeOperators #-}
{-# Language DeriveGeneric #-}

-- | A medication report - this combines parts of both the
--   medication table and participant table and formats things
--   a bit.

module MedicationReport where

import Control.Monad.IO.Class (MonadIO)

import qualified GHC.Generics as GG
import qualified Generics.SOP as GS
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.SOP as PGS

-- this doesn't work with gselectFrom - needs a Generic
-- instance, but I'm not sure it actually *does* need a
-- generic instance. just the ability to get field names.
-- import Database.PostgreSQL.Simple.Types ( (:.) )
-- import Medication
-- import Registration

import DB

data MedicationReport = MedicationReport {
    firstname :: String,
    lastname :: String,
    dob :: String,
    section :: String,

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

    medication_required_other :: Bool,

    medication_self :: Bool
} deriving (GG.Generic, Show)

instance PG.FromRow MedicationReport
instance PG.ToRow MedicationReport

instance GS.Generic MedicationReport
instance GS.HasDatatypeInfo MedicationReport

instance PGS.HasFieldNames MedicationReport

selectAllForMedicationReport :: MonadIO m => m [MedicationReport]
selectAllForMedicationReport = withDB $ \conn ->
  PGS.gselectFrom conn "regmgr_medication left join regmgr_attendee on authenticator = attendee_authenticator" ()

