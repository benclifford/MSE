# regmgr

Non-stack-installed dependencies:
postgres
sendmail

To setup database:

stack build postgresql-simple-migration
stack exec migrate init user=postgres
stack exec migrate migrate user=postgres migrations/

