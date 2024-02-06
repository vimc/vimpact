## vimpact

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![Build Status](https://travis-ci.com/vimc/vimpact.svg?branch=master)](https://travis-ci.com/vimc/vimpact)
[![codecov.io](https://codecov.io/github/vimc/vimpact/coverage.svg?branch=master)](https://codecov.io/github/vimc/vimpact?branch=master)
<!-- badges: end -->

Support code for impact calculations.

## Installation
Package can be installed via
```
devtools::install_github("vimc/vimpact")
```

## Running tests which rely on PostgreSQL database

Some require a PostgreSQL database. These tests will be skipped by default when running locally. To enable them, run the `./scripts/test_db_start.sh` script to run up a Postgres db within a docker container. When finished testing locally run `./scripts/test_db_stop.sh` to stop the container.

You can connect directly to the Postgres db by running `./scripts/connect_db.sh`

## Running tests which rely on Montagu database
Some tests require access to montagu database. You need to make the montagu science db password available as an env var for these tests to run. You can do this by getting the password from the vault and adding it to `~/.Renviron` as MONTAGU_PASSWORD=<pswd_from_vault>. Tests will be skipped if montagu science db password does not exist. `tests/testthat/helper-vimpact.R` will use `.Renviron` to handle db access. `helper-generate_test_data.R` extracts minimal needed data from montagu, and creates an in-memory database against which vimpact functions are run. `vimpact-test-data` stores test data (copied artefacts from github.com/vimc/montagu-reports/oneoff-collate-vimpact-test-data, and uploaded to github.com/vimc/vimpact-test-data). vimpact outputs must be equivalent to the test data for passing tests.
