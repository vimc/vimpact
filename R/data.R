#' Get coverage set table
#'
#' @param con DB connection
#' @return dbplyr lazy db connection to coverage_set table
#' @keywords internal
get_coverage_set_by_touchstone <- function(con, touchstone) {
  gavi_support_level <- NULL
  dplyr::tbl(con, "coverage_set") %>%
    dplyr::filter(touchstone == touchstone & gavi_support_level != "none")
}

#' Get coverage table
#'
#' @param con DB connection
#' @return dbplyr lazy db connection to coverage table
#' @keywords internal
get_coverage <- function(con) {
  dplyr::tbl(con, "coverage")
}

#' Get gender table
#'
#' @param con DB connection
#' @return dbplyr lazy db connection to gender table
#' @keywords internal
get_gender <- function(con) {
  dplyr::tbl(con, "gender")
}

#' Get country table
#'
#' @param con DB connection
#' @return dbplyr lazy db connection to country table
#' @keywords internal
get_country <- function(con) {
  dplyr::tbl(con, "country")
}
