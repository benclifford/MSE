#!/bin/bash -ex

# for running inside a db container

if [ -h /var/lib/postgresql/9.5 ] ; then
  echo pg directory is a link - not touching
elif [ -d /var/lib/postgresql/9.5 ] ; then
  echo pg directory is a directory - moving aside and linking
  sudo mv /var/lib/postgresql/9.5 /pg-9.5-orig
  sudo ln -vs /home/benc/mse-scoutcamp/9.5 /var/lib/postgresql/9.5
fi

sudo /etc/init.d/postgresql start

pushd osmgateway
stack build osmgateway postgresql-simple-migration
stack exec migrate init user=postgres
stack exec migrate migrate user=postgres migrations/
popd

pushd regmgr
stack build regmgr postgresql-simple-migration
stack exec migrate init user=postgres
stack exec migrate migrate user=postgres migrations/
popd

pushd osmgateway
stack exec osmgateway-import
popd

pushd regmgr
stack exec regmgr-from-osm
popd

pushd regmgr
stack exec regmgr-exe
popd

