#' Get data from stochastic tables with specified grouping and filters
#'
#' Can query from cross_all, cross_under5, cohort_all and cohort_under5
#'
#' @param annex Connection to annex db
#' @param table One of cross_all, cross_under5, cohort_all or cohort_under5
#' @param groups Categories to group by for aggregating in query
#' @param filters Filters to apply before aggregation
#'
#' @return Mean, 0.025 and 0.975 quantiles for deaths_default, deaths_novac.
#' deaths_impact, dalys_default, dalys_novac, dalys_impact with specified
#' groupings.
#' @export
fetch_stochastic_data <- function(annex, table,
                                  groups = c("disease", "country", "year"),
                                  filters = NULL) {
  if (!(table %in% c("cross_all", "cross_under5", "cohort_all",
                     "cohort_under5", "cross_all_2019", "cross_under5_2019",
                     "cohort_all_2019", "cohort_under5_2019"))) {
    stop(paste0("Table must be one of cross_all, cross_under5, cohort_all or",
                " cohort_under5 got ", table, "."))
  }
  groups_str <- paste(groups, collapse = ", ")
  where_clause <- build_where(filters)
  DBI::dbGetQuery(annex, sprintf("
  SELECT %s,
    avg(deaths_default) as deaths_default_mid,
    avg(deaths_novac) as deaths_novac_mid,
    avg(deaths_impact) as deaths_impact_mid,
    avg(dalys_default) as dalys_default_mid,
    avg(dalys_novac) as dalys_novac_mid,
    avg(dalys_impact) as dalys_impact_mid,
    percentile_cont(0.025) WITHIN GROUP (ORDER BY deaths_default) AS deaths_default_lo,
    percentile_cont(0.025) WITHIN GROUP (ORDER BY deaths_novac) AS deaths_novac_lo,
    percentile_cont(0.025) WITHIN GROUP (ORDER BY deaths_impact) AS deaths_impact_lo,
    percentile_cont(0.025) WITHIN GROUP (ORDER BY dalys_default) AS dalys_default_lo,
    percentile_cont(0.025) WITHIN GROUP (ORDER BY dalys_novac) AS dalys_novac_lo,
    percentile_cont(0.025) WITHIN GROUP (ORDER BY dalys_impact) AS dalys_impact_lo,
    percentile_cont(0.975) WITHIN GROUP (ORDER BY deaths_default) AS deaths_default_hi,
    percentile_cont(0.975) WITHIN GROUP (ORDER BY deaths_novac) AS deaths_novac_hi,
    percentile_cont(0.975) WITHIN GROUP (ORDER BY deaths_impact) AS deaths_impact_hi,
    percentile_cont(0.975) WITHIN GROUP (ORDER BY dalys_default) AS dalys_default_hi,
    percentile_cont(0.975) WITHIN GROUP (ORDER BY dalys_novac) AS dalys_novac_hi,
    percentile_cont(0.975) WITHIN GROUP (ORDER BY dalys_impact) AS dalys_impact_hi
  FROM %s
  %s
  GROUP BY %s;", groups_str, table, where_clause, groups_str))
}

#' Get data from stochastic tables with specified grouping and filters
#'
#' This will retrieve mean, 0.025 and 0.975 quantiles from cross_all_2019,
#' cross_under5_2019, cohort_all_2019 and cohort_under5_2019. You can pass
#' a set of year groups to initially aggregate over a range of years. Pass
#' individual years to get mean and quantiles for a year alone.d
#'
#' @param annex Connection to annex db
#' @param table One of cross_all, cross_under5, cohort_all or cohort_under5
#' @param groups Categories to group by for aggregating in query, can be any
#' combination of disease and/or country
#' @param filters Filters to apply before aggregation
#' @param year_groups List of year groups to sum over before calculating
#' mean and quantiles. This will sum over all years within range from min & max
#' of each year group. Note that passing a range of years wider than
#' the data itself will only aggregate over the years for which there is data
#' available
#' @param include_proportion_averted If TRUE then calculates mean and quantiles
#' for proportion_deaths_averted = deaths_impact / deaths_novac and
#' for proportion_dalys_averted = dalys_impact / dalys_novac
#'
#' @return Mean, 0.025 and 0.975 quantiles for deaths_default, deaths_novac.
#' deaths_impact, dalys_default, dalys_novac, dalys_impact with specified
#' groupings.
#' @export
fetch_stochastic_data_year_groups <- function(
  annex, table, groups = c("disease", "country"), filters = NULL,
  year_groups = list(c(2000:2019)),
  include_proportion_averted = FALSE) {
  ## Really some test of table structure would be better here
  if (!(table %in% c("cross_all_2019", "cross_under5_2019", "cohort_all_2019",
                     "cohort_under5_2019", "intervention_all_2019"))) {
    stop(paste0("Table must be one of cross_all_2019, cross_under5_2019, ",
                "cohort_all_2019 or cohort_under5_2019",
                " got ", table, "."))
  }
  if ("year" %in% names(filters)) {
    stop("Can't filter year as year used in grouping")
  }
  groups_str <- paste(groups, collapse = ", ")
  where_clause <- build_where(filters)
  years_clause <- build_years(year_groups)
  if (include_proportion_averted) {
    averted_avg <- paste0(
      "avg(deaths_impact / NULLIF(deaths_novac, 0)) as proportion_deaths_averted_mean,\n",
      "avg(dalys_impact / NULLIF(dalys_novac, 0)) as proportion_dalys_averted_mean,")
    averted_q1 <- paste0(
      "percentile_cont(0.025) WITHIN GROUP (ORDER BY deaths_impact / NULLIF(deaths_novac, 0)) AS proportion_deaths_averted_q1,\n",
      "percentile_cont(0.025) WITHIN GROUP (ORDER BY dalys_impact / NULLIF(dalys_novac, 0)) AS proportion_dalys_averted_q1,")
    averted_q3 <-  paste0(",\n",
      "percentile_cont(0.975) WITHIN GROUP (ORDER BY deaths_impact / NULLIF(deaths_novac, 0)) AS proportion_deaths_averted_q3,\n",
      "percentile_cont(0.975) WITHIN GROUP (ORDER BY dalys_impact / NULLIF(dalys_novac, 0)) AS proportion_dalys_averted_q3")
  } else {
    averted_avg <- ""
    averted_q1 <- ""
    averted_q3 <- ""
  }
  sql <- readLines(system_file("sql/aggregate_stochastic.sql"))
  DBI::dbGetQuery(annex, glue::glue(paste(sql, collapse = "\n"),
    groups = groups_str, table = table, years = years_clause,
    where = where_clause, averted_avg = averted_avg, averted_q1 = averted_q1,
    averted_q3 = averted_q3))
}

build_where <- function(filters) {
  single_filter_to_string <- function(filter_name, filter_values) {
    filters <- vcapply(filter_values, function(value) {
      if (is.na(value) || is.null(value)) {
        value_text <- "IS NULL"
      } else if (is.character(value)) {
        value_text <- paste0("= '", value, "'")
      } else {
        value_text <- paste0("= ", value)
      }
      paste0("(", filter_name, " ", value_text, ")")
    })
    paste(filters, collapse = " OR ")
  }
  if (!is.null(filters)) {
    where <- Map(single_filter_to_string, names(filters), filters)
    if (length(where) > 1) {
      ## If filter on more than 1 column then separate each column condition
      ## with brackets. This is so e.g. we get (A1 ∪ A2) ∩ (B1 ∪ B2) opposed to
      ## A1 ∪ A2 ∩ B1 ∪ B2
      where <- vcapply(where, function(x) {
        paste0("(", x, ")")
      })
    }
    where_clause <- paste("WHERE", paste(where, collapse = " AND\n"))
  } else {
    where_clause <- ""
  }
  where_clause
}

build_years <- function(year_groups) {
  if (is.null(year_groups)) {
    return("")
  }
  year_clauses <- lapply(year_groups, function(group) {
    sprintf("SELECT %s AS start_year, %s AS end_year", min(group), max(group))
  })
  paste(year_clauses, collapse = " UNION ALL\n")
}
