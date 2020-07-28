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
get_stochastic_data <- function(annex, table,
                                groups = c("disease", "country", "year"),
                                filters = NULL) {
  if (!(table %in% c("cross_all", "cross_under5", "cohort_all",
                     "cohort_under5"))) {
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
