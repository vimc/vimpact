#!/usr/bin/env bash
set -e
docker run --rm -d \
    -p 5432:5432 \
    --name vimpact-pg \
    -e POSTGRES_DB=vimpact_test_db \
    postgres:9.2
