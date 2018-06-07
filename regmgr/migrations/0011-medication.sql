CREATE TABLE regmgr_medication (

    id SERIAL PRIMARY KEY,
    attendee_id INTEGER NOT NULL references regmgr_attendee(id),

    medication_name TEXT DEFAULT '' NOT NULL,
    medication_reason TEXT DEFAULT '',
    medication_dosage TEXT DEFAULT '',
    medication_notes TEXT DEFAULT '',

    medication_required_before_breakfast BOOLEAN NOT NULL,
    medication_required_with_breakfast BOOLEAN NOT NULL,
    medication_required_after_breakfast BOOLEAN NOT NULL,
    medication_required_before_lunch BOOLEAN NOT NULL,
    medication_required_after_lunch BOOLEAN NOT NULL,
    medication_required_before_dinner BOOLEAN NOT NULL,
    medication_required_after_dinner BOOLEAN NOT NULL,
    medication_required_bedtime BOOLEAN NOT NULL,
    medication_required_as_required BOOLEAN NOT NULL,
    medication_required_other BOOLEAN NOT NULL

);
