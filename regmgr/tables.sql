DROP TABLE IF EXISTS regmgr_attendee;

CREATE TABLE regmgr_attendee (

    id SERIAL PRIMARY KEY,

    authenticator TEXT UNIQUE, -- the secret cookie for finding this record
    state TEXT, -- machine readable: N = nothing -> I = invited -> V = Viewed -> C = attendee completed their part -> F = paper form verified by leader (-> X = cancelled but still in DB)

    -- this will be used as an OCC field as well as a human
    -- readable field, so need to be careful about OCC style
    -- access to it: eg no gratuitous updates to it if someone
    -- could be using it elsewhere.
    -- that's why it's NOT NULL

    modified TIMESTAMP WITH TIME ZONE NOT NULL,

    firstname TEXT DEFAULT '' NOT NULL,
    lastname TEXT DEFAULT '' NOT NULL,
    dob TEXT DEFAULT '' NOT NULL,

    ec_1_name TEXT DEFAULT '' NOT NULL,
    ec_1_relationship TEXT DEFAULT '' NOT NULL,
    ec_1_address TEXT DEFAULT '' NOT NULL,
    ec_1_telephone TEXT DEFAULT '' NOT NULL,
    ec_1_mobile TEXT DEFAULT '' NOT NULL,

    ec_2_name TEXT DEFAULT '' NOT NULL,
    ec_2_relationship TEXT DEFAULT '' NOT NULL,
    ec_2_address TEXT DEFAULT '' NOT NULL,
    ec_2_telephone TEXT DEFAULT '' NOT NULL,
    ec_2_mobile TEXT DEFAULT '' NOT NULL,

    doctor_name TEXT DEFAULT '' NOT NULL,
    doctor_address TEXT DEFAULT '' NOT NULL,
    doctor_telephone TEXT DEFAULT '' NOT NULL,

    swim BOOLEAN DEFAULT FALSE NOT NULL,
    vegetarian BOOLEAN DEFAULT FALSE NOT NULL,

    tetanus_date TEXT DEFAULT '' NOT NULL,
    diseases TEXT DEFAULT '' NOT NULL,
    allergies TEXT DEFAULT '' NOT NULL,
    medication_diet TEXT DEFAULT '' NOT NULL,
    dietary_reqs TEXT DEFAULT '' NOT NULL,
    faith_needs TEXT DEFAULT '' NOT NULL
    
  );
