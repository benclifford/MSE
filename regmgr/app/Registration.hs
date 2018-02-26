{-# Language DeriveGeneric #-}

module Registration where

import qualified GHC.Generics as GG
import qualified Generics.SOP as GS
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.SOP as PGS
import Database.PostgreSQL.Simple.Time as PG

-- | This models the registration form and could have type classes
--   attached for conversions to/from different types.
data Registration = Registration {
    authenticator :: String,
    state :: String,
    modified :: PG.ZonedTimestamp, -- TODO: debate with self about whether modified should be in the Registration or not as it isn't a traditional "editable" field but instead metadata about the record (like the primary key)
    invite_email :: String,
    osm_scoutid :: Maybe Integer,

    firstname :: String,
    lastname :: String,
    dob :: String,
    section :: String,
    registrant_address :: String,
    registrant_telephone :: String,
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
    general_activities :: Bool,
    swim :: Bool,
    vegetarian :: Bool,
    tetanus_date :: String,
    diseases :: String,
    allergies :: String,
    medication_diet :: String,
    dietary_reqs :: String,
    faith_needs :: String
  } deriving (GG.Generic, Show)

instance PG.FromRow Registration
instance PG.ToRow Registration

instance GS.Generic Registration
instance GS.HasDatatypeInfo Registration

instance PGS.HasFieldNames Registration
