#!/usr/bin/env bash
set -e
HERE=$(dirname $0)

. ${HERE}/common
docker network create ${NETWORK}

DB_IMAGE=vimc/montagu-db:master
MIGRATE_IMAGE=vimc/montagu-migrate:master

trap cleanup ERR

docker run --network=${NETWORK} -d \
  --name db \
  -p 5432:5432 \
  ${DB_IMAGE}

docker exec db montagu-wait.sh

docker pull ${MIGRATE_IMAGE}
docker run --rm --network=${NETWORK} \
  ${MIGRATE_IMAGE} \
  migrate

Rscript ${HERE}/generate_fakerbase.R
