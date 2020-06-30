#################################
### prepare coverage and fvps ###
#################################

##' Find latest touchstone given touchstone_name
##'
##' @title Determine touchstone
##'
##' @param con Database connection.  
##' @param touchstone_name touchstone_name
##' @export
get_touchstone <- function(con, touchstone_name){
  d <- DBI::dbGetQuery(con, "SELECT touchstone_name, MAX(touchstone.version) as version FROM touchstone
                       WHERE touchstone_name = $1 AND version != 42 GROUP BY touchstone_name", touchstone_name)
  if (nrow(d) == 0L) {
    stop("Unknown touchstone specified.")
  }
  paste(d, collapse = "-")
}

##' Replace jenner:::fix_covreage_fvps() function
##' 
##' @title Generate target, fvps, coverage for a touchstone
##' 
##' @param con Datebase connection
##' @touchstone_cov Coverage touchstone
##' @touchstone_pop Demography touchstone
##' @year_min extract data from year_min
##' @year_max extract data to year_max
##' @vaccine_to_ignore Ignore defined vaccines
##' @gavi_support_levels gavi support levels
##' @export
extract_vaccination_history <- function(con, touchstone_cov = "201710gavi", touchstone_pop = NULL, 
                                        year_min = 2000, year_max = 2100,
                                        vaccine_to_ignore = c("DTP3", "HepB_BD_home"),
                                        gavi_support_levels = c("with", "bestminus"),
                                        scenario_type = "default") {
  
  ### This function converts input coverage data to be dis-aggregated by gender and age
  ### i.e. input data by country, year and age
  
  ## 1. touchstone specification 
  ## which coverage touchstone to use - given touchstone name, use the latest version touchstone
  if(grepl("-", touchstone_cov)){
    tmp <- DBI::dbGetQuery(con, "SELECT * FROM touchstone WHERE id = $1", touchstone_cov)
    if(nrow(tmp) == 1L){
      print("user defined touchstone version is used.")
    } else {
      stop("User defined touchstone dose not exist.")
    }
  } else {
    touchstone_cov <- get_touchstone(con, touchstone_cov)
  }
  
  ## which demographic touchstone to use
  if (is.null(touchstone_pop)) {
    touchstone_pop <- touchstone_cov
  } else {
    touchstone_pop <- get_touchstone(con, touchstone_pop)
  }
  
  message("Converting input coverage data......")
  
  # extract interpolated population
  p_int_pop <- get_population(con, touchstone_pop = touchstone_pop, demographic_statistic = 'int_pop', 
                              year_ = year_min:year_max, gender = c('Male', 'Female', 'Both'))
  
  print("Extracted interpolated population.")
  
  ## select minimal needed coverage data from the db
  cov_sets <- DBI::dbGetQuery(con, sprintf("SELECT scenario_type, disease, coverage_set.id AS coverage_set, vaccine, activity_type, gavi_support_level 
                              FROM scenario
                              JOIN scenario_coverage_set
                              ON scenario_coverage_set.scenario = scenario.id
                              JOIN coverage_set
                              ON coverage_set.id = scenario_coverage_set.coverage_set
                              JOIN scenario_description
                              ON scenario_description.id = scenario.scenario_description
                              WHERE scenario.touchstone = $1
                              AND scenario_type IN %s
                              AND gavi_support_level IN %s
                              AND vaccine NOT IN %s",
                                           jenner:::sql_in(scenario_type),
                                           jenner:::sql_in(gavi_support_levels), 
                                           jenner:::sql_in(vaccine_to_ignore)), touchstone_cov)
  
  cov <- DBI::dbGetQuery(con, sprintf("SELECT coverage_set, country, year, age_from, age_to, gender.name AS gender, gavi_support, target, coverage 
                                      FROM coverage 
                                      JOIN gender ON gender.id = gender
                                      WHERE coverage_set IN %s
                                      AND coverage > 0
                                      AND year IN %s", 
                                      jenner:::sql_in(cov_sets$coverage_set, text_item = FALSE),
                                      jenner:::sql_in(year_min:year_max, text_item = FALSE)))
  
  cov <- merge_by_common_cols(cov_sets, cov)
  cov$coverage_set <- NULL
  
  print("Extracted raw coverage data...")
  
  ## transform coverage data
  cov <- unique(cov) # this is needed as we used to create multiple coverage_sets for MCV1 and DTP3 for LiST model
  cov$activity_id <- seq_along(cov$vaccine) # attach an id to avoid combining national target population from multiple sias
  
  i <- cov$age_from > cov$age_to
  if (any(i)){
    message("---> detected age_from > age_to")
    cov$age_from_tmp <- cov$age_from
    cov$age_from[i] <- cov$age_to[i]
    cov$age_to[i] <- cov$age_from_tmp[i]
    cov$age_from_tmp <- NULL
    message("---> swaped age_from and age_to as a correction")
  }
  
  ## spliting coverage data by age groups
  cov1 <- cov[cov$activity_type == "routine", ]
  cov1$age <- cov1$age_from
  cov2 <- cov[cov$activity_type == "campaign", ]
  
  d <- list(NULL)
  for(i in seq_along(cov2$activity_id)){
    t <- cov2[i, ]
    idx <- rep(1, times = t$age_to - t$age_from + 1)
    t <- t[idx, ]
    t$age <- t$age_from + seq_along(t$vaccine) - 1
    d[[i]] <- t
  }
  d <- do.call(rbind, d)
  
  cov2 <- rbind(cov1, d)
  
  ## allocating fvps to each age groups by age distribution, and adjusting coverage
  cov2 <- merge_by_common_cols(cov2, p_int_pop, all.x = TRUE)
  tot_pop <- aggregate(value ~ activity_id, cov2, sum)
  cov2$population <- tot_pop$value[match(cov2$activity_id, tot_pop$activity_id)]
  i <- is.na(cov2$target)
  cov2$target[i] <- cov2$population[i]
  
  cov2$fvps_source <- cov2$coverage * cov2$target * cov2$value / cov2$population
  cov2$fvps_adjusted <- ifelse(cov2$fvps_source > cov2$value, cov2$value, cov2$fvps_source)
  cov2$coverage_adjusted <- cov2$fvps_adjusted / cov2$value
  
  names(cov2) <- c("country", "year", "gender", "age", "scenario_type", "disease", "vaccine", "activity_type",
                   "gavi_support_level", "age_from", "age_to", "gavi_support", "target_source", 
                   "coverage_source", "delivery_id", "cohort_size", "delivery_population", "fvps_source", "fvps_adjusted", 
                   "coverage_adjusted")
  cov2 <- cov2[c("delivery_id", "country", "disease", "scenario_type","vaccine", "activity_type", "gavi_support_level", "year", 
                 "gavi_support", "gender", "age", "target_source", "coverage_source",  "cohort_size", "delivery_population", 
                 "fvps_source", "fvps_adjusted", "coverage_adjusted")]
  print("Transformed coverage data.")
  return(cov2[order(cov2$delivery_id), ])
  ### this function gives differnet fvps_adjusted comparing to fix_coverage_fvps()
  ### I am sure this function is more accurate, as fix_coverage_fvps() is a bit buggy here
  ### https://gitbub.com/vimc/jenner/blob/master/R/impact_method2.R#L419
  ### see measles for example (bdi, 2015)
}



##' Extract demographic data
##' 
##' @title Extract demographic data
##' 
##' @param con Datebase connection
##' @param touchstone_pop Demography touchstone
##' @demographic_statistic Demographic statistic to extract
##' @param gender Gender codes - "Male", "Female", "Both"
##' @param country_ All countries if NULL. Or specify a vector of countries
##' @param year_ All years if NULL. Or specify a vector of years
##' @param age_ All age groups if NULL. Or specify a vector of age groups
##' @export
get_population <- function(con, touchstone_pop = "201710gavi-5", demographic_statistic = "int_pop", gender = "Both",
                           country_ = NULL, year_ = NULL, age_ = NULL) {
  if(!grepl("-", touchstone_pop)){
    touchstone_pop <- get_touchstone(con, touchstone_pop)
    message("touchstone version is not specified. Lateset version is used.")
  }
  ## this table get population data as you wish
  sql <- read_sql("inst/sql/population.sql")
  constrains_ <- sql_constrains(country_, year_, age_)
  sql <- sprintf(sql, jenner:::sql_in(gender), constrains_)
  return(DBI::dbGetQuery(con, sql, list(touchstone_pop, demographic_statistic)))
  
}

## supporting function for get_population()
## flexibally contrain country, year, and age for get_population()
## currently burden_estimate_table parameter is set to be FALSE
## when it is set to be true
## it is supposed to constrain country, year, and age for reading burden estimates
## cannot remember where it's been used
sql_constrains <- function(country_ = NULL, year_ = NULL, age_ = NULL, burden_estimate_table = FALSE) {
  country_id <- ifelse(burden_estimate_table, "country.id", "country")
  age_id <- ifelse(burden_estimate_table, "age", "age_from")
  
  country_ <- ifelse(is.null(country_),
                     "\t",
                     sprintf("AND %s IN %s",country_id, jenner:::sql_in(country_)))
  
  year_ <- ifelse(is.null(year_),
                  "\t",
                  sprintf("AND year IN %s", jenner:::sql_in(year_, text_item = FALSE)))
  
  age_ <- ifelse(is.null(age_),
                 "\t",
                 sprintf("AND %s IN %s", age_id, jenner:::sql_in(age_, text_item = FALSE)))
  
  return(paste(country_, year_, age_, collapse = "\n"))
}

### all cause mortality by birth cohort - for the papert
##' Extract demographic data - all couse mortality, live birth and under5 mortality rate
##' 
##' @title Extract demographic data - all couse mortality, live birth and under5 mortality rate
##' 
##' @param con Database connection
##' @param touchstone_pop Demography touchstone
##' @param cohorts birth cohorts for which to extract data
##' @param under_5 whether constrain to under5 mortality
##' @export
cohort_deaths_all_cause <- function(con, touchstone_pop, cohorts, under_5 = TRUE) {
  if (!under_5){
    stop("The function currently only works for under5 mortality.")
  }
  
  year <- cohorts
  ### population
  P <- get_population(con, touchstone_pop = touchstone_pop, country_ = NULL, year_ = year, age_ = NULL, demographic_statistic = "int_pop")
  
  P <- aggregate(value~country + year, data = P, sum)
  
  ### live birth
  cbr <- get_population(con, country_ = NULL, year_ = year, age_ = NULL, demographic_statistic = "cbr")[c("country","year", "value")]
  d <- merge(P, cbr, by = c("country", "year"), all.x = TRUE)
  d$birth <- d$value.x * d$value.y
  
  ### under5 mortality
  mort_rate <- get_population(con, country_ = NULL, year_ = year, age_ = NULL, demographic_statistic = "unwpp_u5mr")[c("country", "year", "value")]
  
  d <- merge(d, mort_rate, by = c("country", "year"))
  d$all_cause <- d$birth * d$value
  
  d_allcause <- d[c("country", "year", "all_cause", "birth", "value")]
  names(d_allcause) <- c("country", "cohort", "all_cause", "live_birth", "u5mr")
  return(d_allcause)
}

### TODO: add coverage clustering functions here.
