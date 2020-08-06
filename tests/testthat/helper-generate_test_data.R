### this R script is used to generate test data
### functions to be tested are
# 2.impact_central.R/get_raw_impact_details()
# 3.impact_central.R/impact_by_year_of_vaccination()

### countries to be tested are PINE

### touchstone to be tested is 201710gavi model run

### vaccination years to be tested is 2000:2030

### diseases to be tested include YF, Measles and HepB

### models to be tested are IC-YF, IC-HepB, LSHTM-Measles

### scenario_type to be tested is default

import_test_data_central_estimates <- function(con, con_test){
  # set up parameters
  touchstone <- "201710gavi"
  countries <- c("PAK", "IND", "NGA", "ETH")
  vaccination_years <- 2000:2030
  diseases <- c("YF", "Measles")#, "HepB")
  models <- c("IC-Garske", "LSHTM-Jit")#, "IC-Hallett")
  default_recipe <- TRUE
  recipe_version <- "201710"
  method_suffix <- c("0", "1", "2a", "2b")
  
  # set up output data set
  data <- list(recipe_0 = NA,
               recipe_1 = NA,
               recipe_2a = NA,
               recipe_2b = NA,
               population = NA,
               touchstone = NA,
               scenario = NA,
               scenario_coverage_set = NA,
               coverage_set = NA,
               scenario_description = NA,
               scenario_type = NA,
               coverage = NA,
               burden_estimate = NA,
               country = NA,
               gender = NA)
  
  ###0. prepare impact calculation recipes and record related burden estimate sets
  burden_sets <- NULL
  scenario <- NULL
  scenario_coverage_set <- NULL
  scenario_description <- NULL
  scenario_type <- NULL
  for(m in method_suffix){
    v <- get_meta_from_recipe(default_recipe, method = paste0("method", m), recipe_version, recipe = NULL, con, diseases)
    v <- v[v$modelling_group %in% models, ]
    data[[paste("recipe", m, sep = "_")]] <- v
    burden_sets <- c(burden_sets, v$burden_estimate_set)
    scenario <- c(scenario, v$scenario)
  }
  scenario <- unique(scenario)
  burden_sets <- unique(burden_sets)
  
  ###1. extract meta from database
  data[["scenario"]] <- DBI::dbGetQuery(con, sprintf("SELECT * FROM scenario WHERE id IN %s",
                                                     sql_in(scenario, text_item = FALSE)))
  
  data[["scenario_coverage_set"]] <- DBI::dbGetQuery(con, sprintf("SELECT * FROM scenario_coverage_set WHERE scenario IN %s",
                                                                  sql_in(scenario, text_item = FALSE)))
  
  data[["coverage_set"]] <- DBI::dbGetQuery(con, sprintf("SELECT * FROM coverage_set WHERE id IN %s",
                                                         sql_in(data$scenario_coverage_set$coverage_set, text_item = FALSE)))
  
  data[["scenario_description"]] <- DBI::dbGetQuery(con, sprintf("SELECT * FROM scenario_description WHERE id IN %s",
                                                                 sql_in(data$scenario$scenario_description, text_item = TRUE)))
  
  data[["scenario_type"]] <- DBI::dbGetQuery(con, sprintf("SELECT * FROM scenario_type WHERE id IN %s",
                                                          sql_in(data$scenario_description$scenario_type, text_item = TRUE)))
  
  data[["coverage"]] <- DBI::dbGetQuery(con, sprintf("SELECT * FROM coverage
                                        WHERE coverage_set IN %s
                                                     AND country IN %s", 
                                                     sql_in(data$coverage_set$id, text_item = FALSE),
                                                     sql_in(countries, text_item = TRUE)))
  
  ###2. extract interpolated population estimates
  data[["population"]] <- get_population(con, touchstone_pop = "201710gavi-5", demographic_statistic = "int_pop", gender = c("Male", "Female", "Both"), 
                                         country_ = countries, year_ = vaccination_years)
  
  ###3. extract burden from database
  data[["country"]] <- DBI::dbGetQuery(con, sprintf("SELECT id, nid FROM country WHERE id IN %s",
                                                    sql_in(countries)))
  
  data[["gender"]] <- DBI::dbReadTable(con, "gender")
  
  data[["burden_estimate"]] <- DBI::dbGetQuery(con, sprintf("SELECT burden_estimate_set, country, year, age, burden_outcome, value 
                                                            FROM burden_estimate 
                                                            WHERE burden_estimate_set IN %s
                                                            AND country IN %s AND burden_outcome != 9",
                                                            sql_in(burden_sets, text_item = FALSE),
                                                            sql_in(data$country$nid, text_item = FALSE)))
  
  data[["burden_outcome"]] <- DBI::dbReadTable(con, "burden_outcome")
  
  data[["touchstone"]] <- DBI::dbReadTable(con, "touchstone")
  for(name in names(data)){
   DBI::dbWriteTable(con_test, name, data[[name]], overwrite = TRUE, row.names = FALSE)
  }

}
