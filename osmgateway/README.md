# osmgateway

in addition to stack:
  apt-get install postgresql-9.5 postgresql-server-dev-9.5

  echo "local   all             postgres                                trust" | sudo tee /etc/postgresql/9.5/main/pg_hba.conf 

  /etc/init.d/postgresql start

  psql --user=postgres < tables.sql

