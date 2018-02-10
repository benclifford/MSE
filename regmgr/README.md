# regmgr

To setup database:

stack build postgresql-simple-migration
stack exec migrate init user=postgres
stack exec migrate migrate user=postgres migrations/

To get custom dependencies:

git clone git@github.com:openbrainsrc/postgresql-simple-sop
and hack the version bound in postgresql-simple-sop.cabal
-                     , generics-sop >= 0.2 && < 0.3
+                     , generics-sop >= 0.2 && < 0.4


