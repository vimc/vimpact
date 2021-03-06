get_raw_impact_details <- function(con, meta1, burden_outcome, is_under5 = FALSE, countries_to_extract = NULL){
  #verify parameters
  stopifnot(nrow(meta1) == 2L)
  stopifnot(burden_outcome %in% c("deaths", "cases", "dalys"))
  stopifnot(is_under5 %in% c(TRUE, FALSE))

  #preparation
  # determine burden outcome, k will be used to determine burden outcome ids
  if(burden_outcome == "deaths"){
    k <- 1
  } else if(burden_outcome == "cases"){
    k <- 2
  } else if(burden_outcome == "dalys"){
    k <- 3
  } else {
    stop("Can only take burden outcome as one of deaths, cases, dalys")
  }

  # determine whether a recipe is for routine or campaign vaccine delivery
  # routine or campaign matters for method2a in terms of the shape of burden estimates to extract
  # if routine, by year of birth
  # if campaign, by calendar
  meta1$vaccine_delivery[meta1$vaccine_delivery == "no-vaccination"] <- ""
  v <- determine_vaccine_delivery(meta1)
  i <- unique(grepl("routine", v))
  j <- unique(grepl("campaign", v))
  if(any(i) && any(j) && meta1$method[1] == "method2a"){
    stop("method2a is vaccine delivery specific, cannot include both routine and campaign impact at the same time.")
  }

  # constain db extraction by country and age

  age_constrain <- ifelse(is_under5, "AND age < 5", "") #applies to all methods

  if (is.null(countries_to_extract)){
    country_constrain <- ""
  } else{
    countries_to_extract <- DBI::dbGetQuery(con, sprintf("SELECT nid FROM country WHERE id IN %s",
                                                         sql_in(countries_to_extract)))
    country_constrain <- sprintf("AND country IN %s", sql_in(countries_to_extract, text_item = FALSE))
  }

  # set up db extraction sql queries
  if((meta1$method[1] == "method0") || (meta1$method[1] == "method2a" && any(j))){
    sql <- paste("SELECT country, year AS time, sum(value) AS value",
                 "FROM burden_estimate",
                 "WHERE burden_estimate_set = %s",
                 "AND burden_outcome IN %s",
                 age_constrain,
                 country_constrain,
                 "GROUP BY country, year")
  } else if((meta1$method[1] == "method1") || (meta1$method[1] == "method2a" && any(i)) || (meta1$method[1] == "method2b")){
    sql <- paste("SELECT country, (year-age) AS time, sum(value)AS value",
                 "FROM burden_estimate",
                 "WHERE burden_estimate_set = %s",
                 "AND burden_outcome IN %s",
                 age_constrain,
                 country_constrain,
                 "GROUP BY country, (year-age)")
  }

  # get burden outcome ids
  i <- meta1$meta_type == "focal"
  ii <- unlist(strsplit(meta1$burden_outcome_id[i], ";"))
  j <- meta1$meta_type == "baseline"
  jj <- unlist(strsplit(meta1$burden_outcome_id[j], ";"))

  # extract burden estimates
  d_baseline <- DBI::dbGetQuery(con, sprintf(sql,
                                             sql_in(meta1$burden_estimate_set[j], text_item = FALSE),
                                             sql_in(jj[k], text_item = FALSE)))

  d_focal <- DBI::dbGetQuery(con, sprintf(sql,
                                          sql_in(meta1$burden_estimate_set[i], text_item = FALSE),
                                          sql_in(ii[k], text_item = FALSE)))
  if(meta1$disease[1] == "HepB" && (nrow(d_focal) == 0L || nrow(d_baseline) == 0L)) {
    # when you are extracting data for HepB - IC and CDA models - for specific countries
    # you may end out with 0 rows for d_baseline or d_focal
    # it is because these two models run scenario specific templates
    # not all countries appear in all scenarios
    return(NULL)
  } else {
    names(d_baseline)[names(d_baseline) == "value"] <- "baseline_value"
    names(d_focal)[names(d_focal) == "value"] <- "focal_value"

    # calculate impact estimates
    d <- merge_by_common_cols(d_baseline, d_focal)
    d$value <- d$baseline_value - d$focal_value
    d$index <- meta1$index[1]
    d$burden_outcome <- burden_outcome

    # return impact estimates
    return(d)
  }

}

# To standardise fvps for impact by year of vaccination and IU
rename_extract_vaccination_history <- function(fvps){
  fvps <- fvps[c("country_nid", "disease", "vaccine", "activity_type", "year", "gavi_support",
                 "gender", "age",  "delivery_population",  "fvps_adjusted", "coverage_adjusted")]
  names(fvps) <- c("country", "disease", "vaccine", "activity_type", "year", "gavi_support",
                   "gender", "age",  "target",  "fvps", "coverage")
  fvps
}

# impact by year of vaccination
# db connection is not allowed
# exportable for non-science users
impact_by_year_of_vaccination <- function(meta1, raw_impact, fvps, fvps_updates = NULL, vaccination_years){
  # for external users, below are minimal standards for the three core input data
  meta_templates <- c("vaccine_delivery", "disease", "meta_type", "index", "method")
  raw_impact_templates <- c("country", "time", "value", "index", "burden_outcome")
  fvps_templates <- c("country", "year", "disease", "vaccine", "activity_type", "age", "fvps")

  assert_has_columns(meta1, meta_templates)
  assert_has_columns(raw_impact, raw_impact_templates)
  assert_has_columns(fvps, fvps_templates)
  if(!is.null(fvps_updates)){
    assert_has_columns(fvps_updates, fvps_templates)
  }
  stopifnot(meta1$method %in% c("method2a", "method2b")) # only calculate impact by year of vaccination
  stopifnot(length(unique(meta1$index)) == 1L) # index has to be unique
  stopifnot(length(unique(meta1$method)) == 1L) # method has to be unique

  # determine method
  method <- meta1$method[1L]
  raw_impact <- raw_impact[raw_impact$index == meta1$index[1], ]
  if(nrow(raw_impact) == 0L){
    return(NULL)
  }

  # determine vaccine delivery
  v <- determine_vaccine_delivery(meta1)
  vaccine_delivery <- data_frame(vaccine = rep(NA, length(v)), activity_type = rep(NA, length(v)))
  for (i in seq_along(v)){
    m <- unlist(strsplit(v[i], "-"))
    vaccine_delivery$vaccine[i] <- m[1]
    vaccine_delivery$activity_type[i] <- m[2]
  }
  if (length(unique(vaccine_delivery$activity_type)) > 1L && method == "method2a"){
    stop("method2a can not accommodate both routine and campaign impact in the same time, as routine and campaign impact are calculated differently.")
  }
  # prepare data
  fvps$time <- fvps$year - fvps$age
  fvps <- merge_by_common_cols(fvps, vaccine_delivery, all.y = TRUE)
  fvps <- fvps[fvps$year %in% vaccination_years, ]

  if(!is.null(fvps_updates)){
    fvps_updates$time <- fvps_updates$year - fvps_updates$age
    fvps_updates <- merge_by_common_cols(fvps_updates, vaccine_delivery, all.y = TRUE)
    fvps_updates <- fvps_updates[fvps_updates$year %in% vaccination_years, ]
  }

  raw_impact$birth_cohort <- raw_impact$time
  if (method == "method2a"){
    d <- impact_by_year_of_vaccination_country_perspective(
      raw_impact, fvps, vaccine_delivery$activity_type[1L], vaccination_years)
  } else if (method == "method2b"){
    d <- impact_by_year_of_vaccination_cohort_perspective(raw_impact, fvps,
                                                          vaccination_years)
  }
  d$time <- d$birth_cohort
  d$birth_cohort <- NULL
  d$value <- NULL
  d$fvps <- NULL
  d_native <- merge_by_common_cols(fvps, d)
  d_native$impact <- d_native$impact_ratio * d_native$fvps
  d_native$index <- meta1$index[1]

  if(!is.null(fvps_updates)){
    d_iu <- merge_by_common_cols(fvps_updates, d)
    d_iu$impact <- d_iu$impact_ratio * d_iu$fvps
    d_iu$index <- meta1$index[1]

    return(list(native_impact = d_native,
                interim_update = d_iu))
  } else {
    return(d_native)
  }

}

determine_vaccine_delivery <- function(meta1){
  setdiff(unlist(strsplit(meta1$vaccine_delivery[meta1$meta_type == "focal"], ",")),
          unlist(strsplit(meta1$vaccine_delivery[meta1$meta_type=="baseline"], ",")))
}

## todo: hepb xili method2 impact needs de-double-counting
# post_processing_hepb_li_method2a <- function(meta, dat, fvps){
#   ids_li <- unique(meta$index[meta$disease == "HepB" & meta$modelling_group == "Li"])
#
#   d_keep <- dat[!(dat$index %in% ids_li), ]
#
#   d1 <- dat[dat$index %in% ids_li, ]
#
#
# }

#' Calculate impact by year of vaccination country perspective
#'
#' This will calculate the impact by year of vaccination by country and
#' burden outcome for a single disease and vaccine.
#'
#' This can take data either by vaccination year and age at vaccination or
#' by birth cohort year.
#'
#' @param raw_impact Data frame of raw impact data this needs to have
#' columns country, value, burden_outcome and either year & age or birth_cohort
#' @param fvps Data frame of fully vaccination person data with columns
#' country, fvps and either year & age or birth_cohort
#' @param activity_type `routine` or `campaign` activity type
#' @param vaccination_years Years of vaccination of interest
#'
#' @return Impact ratio by country and burden outcome
#' @export
impact_by_year_of_vaccination_country_perspective <- function(
  raw_impact, fvps, activity_type, vaccination_years) {
  if (!(activity_type %in% c("routine", "campaign"))) {
    stop(sprintf(
      'Activity type must be "routine" or "campaign" got "%s".', activity_type))
  }

  ## Aggregate FVPs over years of vaccination
  fvps <- fvps[fvps$year %in% vaccination_years, ]
  if (nrow(fvps) == 0) {
    stop("No FVP data for this range of vaccination years")
  }
  tot_fvps <- stats::aggregate(fvps ~ country, fvps, sum, na.rm = TRUE)

  ## Aggregate raw_impact grouped by country & burden_outcome where birth
  ## cohort is in range
  raw_impact$birth_cohort <- get_birth_cohort(raw_impact)
  if (activity_type == "routine"){
    raw_impact <- raw_impact[raw_impact$birth_cohort %in%
                               (vaccination_years - min(fvps$age)), ]
  } else {
    raw_impact <- raw_impact[raw_impact$birth_cohort >= min(vaccination_years), ]
  }
  if (nrow(raw_impact) == 0) {
    stop("No impact data for this range of birth cohort and vaccination years")
  }
  tot_impact <- stats::aggregate(value ~ country + burden_outcome, raw_impact,
                                 sum, na.rm = TRUE)

  ## Calculate impact_ratio from the aggregates
  d <- merge_by_common_cols(tot_impact, tot_fvps, all = TRUE)
  d$impact_ratio <- d$value / d$fvps
  d
}

#' Calculate impact by year of vaccination cohort perspective
#'
#' This will calculate the impact by year of vaccination by country, birth
#' cohort and burden outcome for a single disease and vaccine.
#'
#' This can take data either by vaccination year and age at vaccination or
#' by birth cohort year.
#'
#' @param raw_impact Data frame of raw impact data this needs to have
#' columns country, value, burden_outcome and either year & age or birth_cohort
#' @param fvps Data frame of fully vaccination person data with columns
#' country, fvps and either year & age or birth_cohort
#' @param vaccination_years Years of vaccination of interest
#'
#' @return Impact ratio by country, birth cohort and burden outcome
#' @export
impact_by_year_of_vaccination_cohort_perspective <- function(
  raw_impact, fvps, vaccination_years) {

  ## Aggregate FVPs by birth cohort and country
  fvps <- fvps[fvps$year %in% vaccination_years, ]
  fvps$birth_cohort <- get_birth_cohort(fvps)
  if (nrow(fvps) == 0) {
    stop("No FVP data for this range of vaccination years")
  }
  cohort_fvps <- stats::aggregate(fvps ~ country + birth_cohort, fvps, sum,
                                  na.rm = TRUE)

  ## Filter raw_impact to birth_cohorts within range of FVP cohort data
  raw_impact$birth_cohort <- get_birth_cohort(raw_impact)
  cohort_impact <- raw_impact[
    raw_impact$birth_cohort %in%
      (min(fvps$birth_cohort):max(fvps$birth_cohort)),
    c("country", "birth_cohort", "burden_outcome", "value")]
  if (nrow(cohort_impact) == 0) {
    stop("No impact data for this range of birth cohort and fvp data")
  }

  ## Calculate impact ratio
  d <- merge_by_common_cols(cohort_impact, cohort_fvps, all = TRUE)
  d$impact_ratio <- d$value / d$fvps
  d
}

get_birth_cohort <- function(data) {
  if ("birth_cohort" %in% colnames(data)) {
    return(data$birth_cohort)
  }
  data$year - data$age
}
