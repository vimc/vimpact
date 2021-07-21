skip_if_no_montagu_password <- function() {
  pw <- Sys.getenv("MONTAGU_PASSWORD")
  if (nzchar(pw)) {
    return()
  }
  testthat::skip("Environment variable 'MONTAGU_PASSWORD' is not defined")
}


test_montagu_readonly_connection <- function() {
  skip_if_no_montagu_password()

  host <- Sys.getenv("MONTAGU_HOST", "support.montagu.dide.ic.ac.uk")
  port <- as.integer(Sys.getenv("MONTAGU_PORT", 5432))
  user <- "readonly"
  password <- Sys.getenv("MONTAGU_PASSWORD", "changeme")
  con <- tryCatch(
    DBI::dbConnect(RPostgres::Postgres(),
                   dbname = "montagu",
                   host = host,
                   port = port,
                   password = password,
                   user = user),
    error = function(e)
      testthat::skip("Connection to montagu was not possible"))

  con
}


skip_if_no_reference_data <- function() {
  if (!file.exists("vimpact-test-data")) {
    testthat::skip("Test data not found - please clone vimc/vimpact-test-data")
  }
  args <- c("git", "-C", "vimpact-test-data", "rev-parse", "HEAD")
  expected <- readLines("test-data-version")
  hash <- system2(args, stdout = TRUE)
  if (!is.null(attr(hash, "status"))) {
    stop("git failed with error code ", attr(hash, "status"))
  }
  if (hash != expected) {
    testthat::skip(sprintf(
      "Please update vimpact-test-data to %s or update test-data-version to %s",
      expected, hash))
  }
}

impact_test_data <- data_frame(
  country = c(rep("ETH", 5), rep("PAK", 5)),
  year = rep(2001:2005, 2),
  age = rep(0, 10),
  value = c(234, 456, 345, 234, 345, 934, 567, 876, 675, 456),
  burden_outcome = rep("deaths", 10)
)

impact_test_data_baseline <- data_frame(
  country = c(rep("ETH", 5), rep("PAK", 5)),
  year = rep(c(2001, 2002, 2001, 2002, 2003), 2),
  age = c(0, 0, 1, 1, 1),
  value = c(234, 456, 345, 234, 345, 934, 567, 876, 675, 456),
  burden_outcome = rep("deaths", 10)
)

impact_test_data_focal <- data_frame(
  country = c(rep("ETH", 5), rep("PAK", 5)),
  year = rep(c(2001, 2002, 2001, 2002, 2003), 2),
  age = c(0, 0, 1, 1, 1),
  value = c(90, 121, 134, 221, 134, 432, 534, 433, 342, 355),
  burden_outcome = rep("deaths", 10)
)

fvp_test_data_10 <- data_frame(
  vaccine = rep("HepB", 10),
  activity_type = rep("routine", 10),
  country = c(rep("ETH", 5), rep("PAK", 5)),
  year = rep(2001:2005, 2),
  age = rep(0, 10),
  fvps = c(34, 54, 34, 54, 23, 65, 78, 98, 78, 98),
  disease = rep("HepB", 10)
)

fvp_test_data_15 <- data_frame(
  vaccine = rep("HepB", 15),
  activity_type = c(rep("routine", 10), rep("campaign", 5)),
  country = c(rep("ETH", 5), rep("PAK", 10)),
  year = rep(2001:2005, 3),
  age = rep(0, 15),
  fvps = c(34, 54, 34, 54, 23, 65, 78, 98, 78, 98, 43, 45, 65, 45, 65),
  disease = rep("HepB", 15)
)
