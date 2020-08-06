## vimpact

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![Build Status](https://travis-ci.com/vimc/vimpact.svg?branch=master)](https://travis-ci.com/vimc/vimpact)
[![codecov.io](https://codecov.io/github/vimc/vimpact/coverage.svg?branch=master)](https://codecov.io/github/vimc/vimpact?branch=master)
<!-- badges: end -->

Support code for impact calculations.

## Running tests which rely on PostgreSQL database

Some require a PostgreSQL database. These tests will be skipped by default when running locally. To enable them, run the `./scripts/test_db_start.sh` script to run up a Postgres db within a docker container. When finished testing locally run `./scripts/test_db_stop.sh` to stop the container.

You can connect directly to the Postgres db by running `./scripts/connect_db.sh`
