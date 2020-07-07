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
  if(i & j & meta1$method[1] == "method2a"){
    stop("method2a is vaccine delivery specific, cannot include both routine and campaign impact at the same time.")
  }
  
  # constain db extraction by country and age
  countries_to_extract <- DBI::dbGetQuery(con, sprintf("SELECT nid FROM country WHERE id IN %s",
                                                       jenner:::sql_in(countries_to_extract)))
  age_constrain <- ifelse(is_under5, "AND age < 5", "") #applies to all methods
  country_constrain <- ifelse(is.null(countries_to_extract), "", 
                              sprintf("AND country IN %s", jenner:::sql_in(countries_to_extract, text_item = FALSE)))
  
  # set up db extraction sql queries
  if((meta1$method[1] == "method0") | (meta1$method[1] == "method2a" & j)){
    sql <- paste("SELECT country, year AS time, sum(value) AS value",
                 "FROM burden_estimate",
                 "WHERE burden_estimate_set = %s",
                 "AND burden_outcome IN %s",
                 age_constrain,
                 country_constrain,
                 "GROUP BY country, year")
  } else if((meta1$method[1] == "method1") | (meta1$method[1] == "method2a" & i) | (meta1$method[1] == "method2b")){
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
  d_focal <- DBI::dbGetQuery(con, sprintf(sql, 
                                          jenner:::sql_in(meta1$burden_estimate_set[i], text_item = FALSE), 
                                          jenner:::sql_in(ii[k], text_item = FALSE)))
  names(d_focal)[names(d_focal) == "value"] <- "focal_value"
  
  d_baseline <- DBI::dbGetQuery(con, sprintf(sql, 
                                             jenner:::sql_in(meta1$burden_estimate_set[j], text_item = FALSE), 
                                             jenner:::sql_in(jj[k], text_item = FALSE)))
  names(d_baseline)[names(d_baseline) == "value"] <- "baseline_value"
  
  # calculate impact estimates
  d <- merge_by_common_cols(d_baseline, d_focal)
  d$value <- d$baseline_value - d$focal_value
  d$index <- meta1$index[1]
  d$burden_outcome <- burden_outcome
  
  # return impact estimates
  d
  
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
  fvps_templates <- c("country", "year", "disease", "vaccine", "activity_type", "age",
                      "gender", "fvps")
  assert_has_columns(meta1, meta_templates)
  assert_has_columns(raw_impact, raw_impact_templates)
  assert_has_columns(fvps, fvps_templates)
  assert_has_columns(fvps_updates, fvps_templates)
  stopifnot(meta1$method %in% c("method2a", "method2b")) # only calculate impact by year of vaccination
  stopifnot(length(unique(meta1$index)) == 1L) # index has to be unique
  stopifnot(length(unique(meta1$method)) == 1L) # method has to be unique
  print(paste("Calculating impact by year of vaccination related to vaccination programmes between",
              min(vaccination_years), "and", max(vaccination_years)))
  
  # determine method
  method <- meta1$method[1L]
  
  # determine vaccine delivery
  v <- determine_vaccine_delivery(meta1)
  vaccine_delivery <- data_frame(vaccine = rep(NA, length(v)), activity_type = rep(NA, length(v)))
  for (i in seq_along(v)){
    m <- unlist(strsplit(v[i], "-"))
    vaccine_delivery$vaccine[i] <- m[1] 
    vaccine_delivery$activity_type[i] <- m[2] 
  }
  if (length(unique(vaccine_delivery$activity_type)) > 1L & method == "method2a"){
    stop("method2a can not accommodate both routine and campaign impact in the same time, as routine and campaign impact are calculated differently.")
  }
  # prepare data
  fvps$time <- fvps$year - fvps$age
  fvps <- merge_by_common_cols(fvps, vaccine_delivery, all.y = TRUE)
  fvps <- fvps[fvps$year %in% vaccination_years, ]
  cohorts_vaccinated <- unique(fvps$time)
  
  if(!is.null(fvps_updates)){
    fvps_updates$time <- fvps_updates$year - fvps_updates$age
    fvps_updates <- merge_by_common_cols(fvps_updates, vaccine_delivery, all.y = TRUE)
    fvps_updates <- fvps_updates[fvps_updates$year %in% vaccination_years, ]
  }
  
  
  if (method == "method2a"){
    if (vaccine_delivery$activity_type[1L] == "routine"){
      raw_impact <- raw_impact[raw_impact$time %in% cohorts_vaccinated, ]
    } else {
      raw_impact <- raw_impact[raw_impact$time >= min(vaccination_years), ]
    }
    tot_impact <- aggregate(value ~ country + burden_outcome, raw_impact, sum, na.rm = TRUE)
    tot_fvps <- aggregate(fvps~country, fvps, sum, na.rm = TRUE)
    d <- merge_by_common_cols(tot_impact, tot_fvps, all = TRUE)
  } else if (method == "method2b"){
    cohort_impact <- raw_impact[raw_impact$time %in% cohorts_vaccinated, c("country", "time", "burden_outcome", "value")]
    cohort_fvps <- aggregate(fvps ~ country + time, fvps, sum, na.rm = TRUE)
    d <- merge_by_common_cols(cohort_impact, cohort_fvps, all = TRUE)
  }
  d$impact_ratio <- d$value / d$fvps
  d$value <- NULL
  d$fvps <- NULL
  d_native <- merge_by_common_cols(fvps, d)
  d_native$impact <- d_native$impact_ratio * d_native$fvps
  
  if(!is.null(fvps_updates)){
    d_iu <- merge_by_common_cols(fvps_updates, d)
    d_iu$impact <- d_iu$impact_ratio * d_iu$fvps
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
