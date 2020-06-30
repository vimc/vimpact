## get burden estimates for each impact recipe
# example d <- get_raw_impact(con, meta1, burden_outcome = "deaths", is_under5 = FALSE, vaccination_years_of_interest = 2000:2030)
get_raw_impact <- function(con, meta1, burden_outcome,is_under5 = FALSE, vaccination_years_of_interest = 2000:2030){
  stopifnot(nrow(meta1) == 2L)
  if(meta1$method[1L] %in% c("method0", "method1")){
    message("parameter vaccination_years_of_interest is not used, as it is related to impact by year of vaccination.")
  }
  if(burden_outcome == "deaths"){
    k <- 1
  } else if(burden_outcome == "cases"){
    k <- 2
  } else if(burden_outcome == "dalys"){
    k <- 3
  } else {
    stop("Can only take burden outcome as one of deaths, cases, dalys")
  }
  
  # routine or campaign matters for method2a in terms of the shape of burden estimates to extract
  # if routine, by year of birth
  # if campaign, by calendar
  
  meta1$vaccine_delivery[meta1$vaccine_delivery == "no-vaccination"] <- ""
  v <- setdiff(unlist(strsplit(meta1$vaccine_delivery[meta1$meta_type == "focal"], ",")), 
               unlist(strsplit(meta1$vaccine_delivery[meta1$meta_type=="baseline"], ",")))
  i <- grepl("routine", v)
  j <- grepl("campaign", v)
  if(i & j & meta1$method[1] == "method2a"){
    stop("method2a is vaccine delivery specific, cannot include both routine and campaign impact at the same time.")
  }
  
  age_constrain <- ifelse(is_under5, "AND age < 5", "") #applies to all methods
  time_constrain <- sprintf("AND (year-age) IN %s", 
                            jenner:::sql_in(vaccination_years_of_interest, text_item = FALSE)) ## applies to method2a and method2b
  
  if(meta1$method[1] == "method0"){
    sql <- paste("SELECT country, year AS time, sum(value) AS value",
                 "FROM burden_estimate",
                 "WHERE burden_estimate_set = %s",
                 "AND burden_outcome IN %s",
                 age_constrain,
                 "GROUP BY country, year")
  } else if(meta1$method[1] == "method1"){
    sql <- paste("SELECT country, (year-age) AS time, sum(value)AS value",
                 "FROM burden_estimate",
                 "WHERE burden_estimate_set = %s",
                 "AND burden_outcome IN %s",
                 age_constrain,
                 "GROUP BY country, (year-age)")
  } else if(meta1$method[1] == "method2a" & i){
    sql <- paste("SELECT country, sum(value) AS value",
                 "FROM burden_estimate",
                 "WHERE burden_estimate_set = %s",
                 "AND burden_outcome IN %s",
                 age_constrain,
                 time_constrain,
                 "GROUP BY country")
  } else if(meta1$method[1] == "method2a" & j){
    sql <- paste("SELECT country, sum(value) AS value",
                 "FROM burden_estimate",
                 "WHERE burden_estimate_set = %s",
                 "AND burden_outcome IN %s",
                 age_constrain,
                 "GROUP BY country")
  } else if(meta1$method[1] == "method2b"){
    sql <- paste("SELECT country, (year-age) AS time, sum(value) AS value",
                 "FROM burden_estimate",
                 "WHERE burden_estimate_set = %s",
                 "AND burden_outcome IN %s",
                 age_constrain,
                 time_constrain,
                 "GROUP BY country, (year-age)")
  }
  
  i <- meta1$meta_type == "focal"
  ii <- unlist(strsplit(meta1$burden_outcome_id[i], ";"))
  j <- meta1$meta_type == "baseline"
  jj <- unlist(strsplit(meta1$burden_outcome_id[j], ";"))
  
  
  d_focal <- DBI::dbGetQuery(con, sprintf(sql, 
                                          jenner:::sql_in(meta1$burden_estimate_set[i], text_item = FALSE), 
                                          jenner:::sql_in(ii[k], text_item = FALSE)))
  names(d_focal)[names(d_focal) == "value"] <- "focal_value"
  
  d_baseline <- DBI::dbGetQuery(con, sprintf(sql, 
                                             jenner:::sql_in(meta1$burden_estimate_set[j], text_item = FALSE), 
                                             jenner:::sql_in(jj[k], text_item = FALSE)))
  names(d_baseline)[names(d_baseline) == "value"] <- "baseline_value"

  d <- merge_by_common_cols(d_baseline, d_focal)
  d$value <- d$baseline_value - d$focal_value
  d
}




## get burden estimates for each impact recipe
## different from the above is that method2a is not showing aggregated impact, but impact by "time"
## the up-side is that this function will output one unique output format
## the down-side is that method2a impact calculation will be slower
## I am inclined to this version of function, as the output will be useful for having a exportable impact by year of vaccination
## function for external users
#d <- get_raw_impact_details(con, meta1, burden_outcome = "deaths", is_under5 = FALSE)
get_raw_impact_details <- function(con, meta1, burden_outcome,is_under5 = FALSE){
  stopifnot(nrow(meta1) == 2L)

  if(burden_outcome == "deaths"){
    k <- 1
  } else if(burden_outcome == "cases"){
    k <- 2
  } else if(burden_outcome == "dalys"){
    k <- 3
  } else {
    stop("Can only take burden outcome as one of deaths, cases, dalys")
  }
  
  # routine or campaign matters for method2a in terms of the shape of burden estimates to extract
  # if routine, by year of birth
  # if campaign, by calendar
  
  meta1$vaccine_delivery[meta1$vaccine_delivery == "no-vaccination"] <- ""
  v <- setdiff(unlist(strsplit(meta1$vaccine_delivery[meta1$meta_type == "focal"], ",")), 
               unlist(strsplit(meta1$vaccine_delivery[meta1$meta_type=="baseline"], ",")))
  i <- grepl("routine", v)
  j <- grepl("campaign", v)
  if(i & j & meta1$method[1] == "method2a"){
    stop("method2a is vaccine delivery specific, cannot include both routine and campaign impact at the same time.")
  }
  
  age_constrain <- ifelse(is_under5, "AND age < 5", "") #applies to all methods

  if((meta1$method[1] == "method0") | (meta1$method[1] == "method2a" & j)){
    sql <- paste("SELECT country, year AS time, sum(value) AS value",
                 "FROM burden_estimate",
                 "WHERE burden_estimate_set = %s",
                 "AND burden_outcome IN %s",
                 age_constrain,
                 "GROUP BY country, year")
  } else if((meta1$method[1] == "method1") | (meta1$method[1] == "method2a" & i) | (meta1$method[1] == "method2b")){
    sql <- paste("SELECT country, (year-age) AS time, sum(value)AS value",
                 "FROM burden_estimate",
                 "WHERE burden_estimate_set = %s",
                 "AND burden_outcome IN %s",
                 age_constrain,
                 "GROUP BY country, (year-age)")
  }
  
  i <- meta1$meta_type == "focal"
  ii <- unlist(strsplit(meta1$burden_outcome_id[i], ";"))
  j <- meta1$meta_type == "baseline"
  jj <- unlist(strsplit(meta1$burden_outcome_id[j], ";"))
  
  
  d_focal <- DBI::dbGetQuery(con, sprintf(sql, 
                                          jenner:::sql_in(meta1$burden_estimate_set[i], text_item = FALSE), 
                                          jenner:::sql_in(ii[k], text_item = FALSE)))
  names(d_focal)[names(d_focal) == "value"] <- "focal_value"
  
  d_baseline <- DBI::dbGetQuery(con, sprintf(sql, 
                                             jenner:::sql_in(meta1$burden_estimate_set[j], text_item = FALSE), 
                                             jenner:::sql_in(jj[k], text_item = FALSE)))
  names(d_baseline)[names(d_baseline) == "value"] <- "baseline_value"
  
  d <- merge_by_common_cols(d_baseline, d_focal)
  d$value <- d$baseline_value - d$focal_value
  d
}
