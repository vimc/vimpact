#!/usr/bin/env bash
set -e

psql -h localhost -p 5432 -Upostgres vimpact_test_db
