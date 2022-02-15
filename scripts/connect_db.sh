#!/usr/bin/env bash
set -e

PGPASSWORD=changeme psql -h localhost -p 5432 -U vimc montagu
