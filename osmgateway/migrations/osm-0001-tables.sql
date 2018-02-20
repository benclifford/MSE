DROP TABLE IF EXISTS osm_individuals;

CREATE TABLE osm_individuals (
    id SERIAL PRIMARY KEY,
    scoutid INTEGER,
    firstname TEXT,
    lastname TEXT,
    dob TEXT
  );

DROP TABLE IF EXISTS osm_individual_section;

-- records that an individual is in a section
CREATE TABLE osm_individual_section (
  id SERIAL PRIMARY KEY,
  scoutid INTEGER,
  sectionid INTEGER
);

DROP TABLE IF EXISTS osm_sections;

CREATE TABLE osm_sections (
    id SERIAL PRIMARY KEY,
    sectionid INTEGER,
    sectionname TEXT
  );

DROP TABLE IF EXISTS osm_extradata;

CREATE TABLE osm_extradata (
    id SERIAL PRIMARY KEY,
    scoutid INTEGER,
    groupid TEXT,
    columnid INTEGER,
    varname TEXT,
    label TEXT,
    value TEXT
  );

DROP TABLE IF EXISTS osm_event_attendee;

CREATE TABLE osm_event_attendee ( 
  id SERIAL PRIMARY KEY,
  eventid INTEGER,
  scoutid INTEGER,
  attending TEXT
);
