### this R script is used to generate test data
### functions to be tested are
# 1.commonly_used_db_data.R/extract_vaccination_history()
# 2.impact_central.R/get_raw_impact_details()
# 3.impact_central.R/impact_by_year_of_vaccination()

### countries to be tested are PINE + China

### touchstone to be tested is 201910gavi model run

### vaccination years to be tested is 2000:2030

### diseases to be tested include YF, Measles and HepB

### models to be tested are IC-YF, IC-HepB, LSHTM-Measles

### scenario_type to be tested is default
extract_test_data <- function(con){
  # set up parameters
  touchstone <- "201710gavi"
  countries <- c("PAK", "IND", "NGA", "ETH", "CHN")
  vaccination_years <- 2000:2030
  diseases <- c("YF", "Measles", "HepB")
  models <- c("IC-Garske", "LSHTM-Jit", "IC-Hallett")
  default_recipe <- TRUE
  recipe_version <- "201710"
  
  # set up output data set
  data <- list(recipe_0 = NA,
               recipe_1 = NA,
               recipe_2a = NA,
               recipe_2b = NA,
               poulation = NA,
               coverage = NA,
               burdens = NA,
               country = NA)
  
  ###0. prepare impact calculation recipes and record related burden estimate sets
  burden_sets <- NA
  for(m in c("0", "1", "2a", "2b")){
    v <- get_meta_from_recipe(default_recipe, method = paste0("method", m), recipe_version, recipe = NULL, con, diseases)
    v <- v[v$modelling_group %in% models, ]
    data[[paste("recipe", m, sep = "_")]] <- v
    burden_sets <- c(burden_sets, v$burden_estimate_set)
  }
  burden_sets <- unique(burden_sets)
  
  ###1. extract fvps from database
  touch <- get_touchstone(con, touchstone)
  cov_sets <- DBI::dbGetQuery(con, "SELECT * FROM coverage_set
                              WHERE touchstone = $1
                              AND gavi_support_level = 'with'
                              AND vaccine NOT IN ('DTP3', 'HepB_BD_home')", 
                              touch)
  i <- grepl("MCV1", cov_sets$name) & !grepl("Measles", cov_sets$name)
  cov_sets <- cov_sets[!i, ]
  
  data[["coverage_set"]] <- cov_sets
  
  data[["coverage"]] <- DBI::dbGetQuery(con, sprintf("SELECT * FROM coverage
                                        WHERE coverage_set IN %s", jenner:::sql_in(cov_sets, text_item = FALSE)))
  
  ###2. extract interpolated population estimates
  data[["population"]] <- get_population(con, touchstone_pop = touch, demographic_statistic = "int_pop", gender = c("Male", "Female", "Both"), 
                                         country_ = countries, year_ = vaccination_years)
  
  ###3. extract burden from database
  data[["burdens"]] <- DBI::dbGetQuery(con, sprintf("SELECT * FROM burden_estimate WHERE burden_estimate_set IN %s",
                                                    jenner:::sql_in(burden_sets, text_item = FALSE)))
  
  data[["country"]] <- DBI::dbGetQuery(con, sprintf("SELECT * id, nid FROM country WHERE id IN %s",
                                                    jenner:::sql_in(countries)))
  
  if(!dir.exists("tests/test_data")){
    dir.create("tests/test_data")
  }
  for(name in names(data)){
    saveRDS(data[[name]], paste0("tests/test_data/", name, ".rds"))
  }
}

transform_test_data <- function(){
  extracted_data <- extract_test_data(con)
  con <- NULL # make sure con is no longer used anywhere else
    
}