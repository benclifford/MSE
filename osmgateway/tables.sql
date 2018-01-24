DROP TABLE IF EXISTS osm_individuals;

CREATE TABLE osm_individuals (
    id SERIAL PRIMARY KEY,
    scoutid INTEGER,
    firstname TEXT,
    lastname TEXT,
    dob TEXT
  );
