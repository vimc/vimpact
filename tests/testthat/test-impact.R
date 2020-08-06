context("Test Impact Calculations")

test_that("test if vimpact functions are working as expected for central estimates", {
  
  skip_if_not_installed("RSQLite")
  con <- test_montagu_readonly_connection()
  con_test <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
  d = import_test_data_central_estimates(con, con_test)
  on.exit({
    DBI::dbDisconnect(con)
    DBI::dbDisconnect(con_test)
  })
  
  vaccination_years <- 2000:2030
  country <- DBI::dbReadTable(con_test, "country")
  fvps <- readRDS("testthat/test_data/fvps.rds")
  fvps$disease[fvps$vaccine %in% c("HepB", "HepB_BD")] <- "HepB"
  fvps$disease[fvps$vaccine %in% c("MCV1", "MCV2", "Measles")] <- "Measles"
  fvps$disease[fvps$vaccine %in% c("YF")] <- "YF"
  
  #message("preparing in-memory database storing test data")
  #import_test_data_central_estimates(con, con_test)

  ### test method 0
  message("test cross-view impact")
  meta <- DBI::dbReadTable(con_test, "recipe_0")
  meta_s <- split(meta, meta$index)
  dat <- lapply(meta_s, function(meta1) get_raw_impact_details(con = con_test, meta1, burden_outcome = "deaths"))
  dat <- do.call(rbind, dat)
  
  dat <- dat[dat$time %in% vaccination_years, ]
  dat <- standardise_impact_output(meta, dat)
  dat$country <- country$id[match(dat$country, country$nid)] 
  
  test_data <- readRDS("testthat/test_data/impact_method0.rds")
  test_data <- test_data[test_data$burden_outcome == "deaths_averted", ]
  test_data <- test_data[test_data$disease %in% unique(dat$disease), ]
  a <- aggregate(impact ~ disease + modelling_group + country, dat, sum, na.rm = TRUE)
  b <- aggregate(impact ~ disease + modelling_group + country, test_data, sum, na.rm = TRUE)
  a$impact <- round(a$impact/100)
  b$impact <- round(b$impact/100)
  
  expect_equal(a, b)


  ### test method 1
  message("test cohort-view impact")
  meta <- DBI::dbReadTable(con_test, "recipe_1")
  meta_s <- split(meta, meta$index)
  dat <- lapply(meta_s, function(meta1) get_raw_impact_details(con = con_test, meta1, burden_outcome = "deaths"))
  dat <- do.call(rbind, dat)
  
  dat <- dat[dat$time %in% vaccination_years, ]
  dat <- standardise_impact_output(meta, dat)
  dat$country <- country$id[match(dat$country, country$nid)] 
  
  test_data <- readRDS("testthat/test_data/impact_method1.rds")
  test_data <- test_data[test_data$burden_outcome == "deaths_averted", ]
  test_data <- test_data[test_data$disease %in% unique(dat$disease) & test_data$cohort %in% vaccination_years, ]
  a <- aggregate(impact ~ disease + modelling_group + country, dat, sum, na.rm = TRUE)
  b <- aggregate(impact ~ disease + modelling_group + country, test_data, sum, na.rm = TRUE)
  a$impact <- round(a$impact/100)
  b$impact <- round(b$impact/100)
  
  expect_equal(a, b)

  ### test method 2a
  #message("test impact_by_year_of_vaccination conventional approach")
  meta <- DBI::dbReadTable(con_test, "recipe_2a")
  meta <- split(meta, meta$index)
  dat <- lapply(meta, function(meta1) get_raw_impact_details(con = con_test, meta1, burden_outcome = "deaths"))
  dat <- do.call(rbind, dat)
  dat$country <- country$id[match(dat$country, country$nid)]
  dat2 <- lapply(meta, function(meta1) impact_by_year_of_vaccination(meta1, raw_impact = dat, fvps = fvps,
                                                                     vaccination_years = vaccination_years))
  dat2 <- do.call(rbind, dat2)
  
  test_data <- readRDS("testthat/test_data/impact_method2a.rds")
  test_data <- test_data[test_data$vaccine %in% unique(dat2$vaccine), ]

  a <- unique(dat2[c("country", "vaccine", "activity_type", "impact_ratio")])
  b <- unique(test_data[test_data$burden_outcome == "deaths_averted_rate",
                        c("country", "vaccine", "activity_type", "impact")])
  d <- merge_by_common_cols(a, b, all = TRUE)
  expect_equal(d$impact_ratio, d$impact, tolerance = 1.e-8)
  
  ### test method 2b
  message("test impact_by_year_of_vaccination cohort-based approach")
  meta <- DBI::dbReadTable(con_test, "recipe_2b")

  meta <- split(meta, meta$index)
  dat <- lapply(meta, function(meta1) get_raw_impact_details(con = con_test, meta1, burden_outcome = "deaths"))
  dat <- do.call(rbind, dat)
  dat$country <- country$id[match(dat$country, country$nid)]
  dat2 <- lapply(meta, function(meta1) impact_by_year_of_vaccination(meta1, raw_impact = dat, fvps = fvps,
                                                                     vaccination_years = vaccination_years))
  dat2 <- do.call(rbind, dat2)
  
  test_data <- readRDS("testthat/test_data/impact_method2b.rds")
  test_data <- test_data[test_data$vaccine %in% unique(dat2$vaccine), ]
  
  a <- aggregate(impact ~ country + vaccine + activity_type, dat2, sum, na.rm = TRUE)
  b <- aggregate(impact ~ country +  vaccine + activity_type, test_data[test_data$burden_outcome == "deaths_averted", ], sum, na.rm = TRUE)
  d <- merge(a, b, by = c("country", "vaccine", "activity_type"), all = TRUE)
  expect_equal(d$impact.x, d$impact.y, tolerance = 1.e-1)

})


