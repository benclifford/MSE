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

    firstname TEXT,
    lastname TEXT,
    dob TEXT
  );
