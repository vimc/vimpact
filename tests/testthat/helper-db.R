get_db_info <- function() {
  list(
    dbname = "montagu",
    user = "vimc",
    host = "localhost",
    password = "changeme"
  )
}

prepare_example_postgres_db <- function() {
  info <- get_db_info()
  tryCatch({
    con <- retry::retry(
      get_postgres_connection(info$dbname, info$user, info$host, info$password),
      until = function(val, cnd) !is.null(val),
      max_tries = 25,
      interval = 0.2)
    add_dummy_data(con)
    DBI::dbDisconnect(con)
  }, error = function(e) {
    message(sprintf(paste0("Failed to prepare db connection to postgres db",
                           " %s with user %s and host %s.\n    %s"),
                    info$dbname, info$user, info$host, e$message))
  })
  invisible(TRUE)
}

## Use inside of test - calls testthat::skip on error
get_test_connection <- function() {
  info <- get_db_info()
  vimpact_test_postgres_connection(info$dbname, info$user, info$host, info$password)
  con <- get_postgres_connection(info$dbname, info$user, info$host, info$password)
  withr::defer_parent(DBI::dbDisconnect(con))
  con
}

vimpact_test_postgres_connection <- function(dbname, user, host, password) {
  tryCatch(
    get_postgres_connection(dbname, user, host, password),
    error = function(e) testthat::skip(sprintf(
    "Failed to open db connection to postgres db %s with user %s and host %s.",
    dbname, user, host))
  )
}

get_postgres_connection <- function(dbname, user, host, password) {
  DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, user = user,
                 host = host, password = password)
}

add_dummy_data <- function(con) {
  db_name <- DBI::dbGetQuery(con, "SELECT current_database()")
  if (db_name != "montagu") {
    stop(sprintf(
      "Can't add dummy data to db %s expected test db 'montagu'",
      db_name))
  } else {
    message("Adding dummy data to database")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS cross_all CASCADE")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS cross_under5 CASCADE")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS cohort_all CASCADE")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS cohort_under5 CASCADE")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS cross_all_2019 CASCADE")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS cross_under5_2019 CASCADE")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS cohort_all_2019 CASCADE")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS cohort_under5_2019 CASCADE")

    DBI::dbWriteTable(con, "cross_all", create_dummy_data())
    DBI::dbWriteTable(con, "cross_under5", create_dummy_data())
    DBI::dbWriteTable(con, "cohort_all", create_dummy_data())
    DBI::dbWriteTable(con, "cohort_under5", create_dummy_data())
    DBI::dbWriteTable(con, "cross_all_2019", create_dummy_data())
    DBI::dbWriteTable(con, "cross_under5_2019", create_dummy_data())
    DBI::dbWriteTable(con, "cohort_all_2019", create_dummy_data())
    DBI::dbWriteTable(con, "cohort_under5_2019", create_dummy_data())
  }
}

create_dummy_data <- function() {
  get_random_increasing_sequence <- function(n, min_initial = 1000,
                                             max_initial = 3000,
                                             min_increase = 20,
                                             max_increase = 200) {
    sequence <- vector("numeric", length = n)
    sequence[1] <- floor(runif(1, min_initial, max_initial))
    i = 2
    while(i <= length(sequence)) {
      sequence[i] <- sequence[i - 1] +
        floor(runif(1, min_increase, max_increase))
      i = i + 1
    }
    sequence
  }

  dat <- expand.grid(year = 2001:2015,
                     country = c("AFG", "NGA"),
                     disease = c("HepB", "Measles"),
                     run_id = 1:5,
                     stochastic_file_id = 1:2,
                     stringsAsFactors = FALSE)
  dat$deaths_default <- get_random_increasing_sequence(nrow(dat))
  dat$deaths_novac <- dat$deaths_default +
    floor(runif(nrow(dat), 100, 400))
  dat$deaths_impact <- dat$deaths_novac - dat$deaths_default
  dat$dalys_default <- get_random_increasing_sequence(nrow(dat),
                                                      100000,
                                                      300000,
                                                      500,
                                                      1000)
  dat$dalys_novac <- dat$dalys_default +
    floor(runif(nrow(dat), 10000, 40000))
  dat$dalys_impact <- dat$dalys_novac - dat$dalys_default
  dat
}

prepare_example_postgres_db()
