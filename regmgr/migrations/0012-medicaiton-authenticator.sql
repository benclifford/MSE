ALTER TABLE regmgr_medication ADD COLUMN authenticator TEXT NOT NULL references regmgr_attendee(authenticator);
ALTER TABLE regmgr_medication DROP COLUMN attendee_id;
