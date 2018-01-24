DROP TABLE IF EXISTS regmgr_attendee;

CREATE TABLE regmgr_attendee (
    id SERIAL PRIMARY KEY,
    authenticator UUID UNIQUE, -- the secret cookie for finding this record
    state TEXT, -- machine readable: N = nothing -> I = invited -> V = Viewed -> C = attendee completed their part -> F = paper form verified by leader (-> X = cancelled but still in DB)
    modified TIMESTAMP WITH TIME ZONE,

    firstname TEXT,
    lastname TEXT,
    dob TEXT
  );
