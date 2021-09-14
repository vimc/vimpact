##'Generate impact recipe template
##'
##' @title Impact recipe template
##'
##' @param template_version version can be any VIMC model run - e.g. 201710, 201910
##' @param method method can be any VIMC impact methods - method0, method1, method2a, method2b
##' @export
recipe_template <- function(template_version = "201710", method){
  assert_method(method)
  recipe <- assert_version(method, template_version)
  recipe <- read_csv(system_file("recipe", method, recipe))

  if (!dir.exists("recipe")){
    dir.create("recipe")
  }
  if (!dir.exists(paste0("recipe/", method))){
    dir.create(paste0("recipe/", method))
  }
  utils::write.csv(recipe, file.path("recipe", method,"impact_recipe.csv"), row.names = FALSE)
  man <- file(file.path("recipe", "impact_recipe_man.md"))
  writeLines(paste0("## touchstone \n",
                    "touchstone lists touchstone_name, or touchstone_version if specific version applies. \n \n",
                    "## modelling_group \n",
                    "use montagu modelling_group naming conventions \n \n",
                    "## disease \n",
                    "use montagu disease naming conventions \n \n",
                    "## focal \n",
                    "focal scenario in the form of <scenario_type>:<vaccine1>-<activity_type1>,<vaccine2>-<activity_type2> \n",
                    "NO spacing is allowed \n \n",
                    "## baseline \n",
                    "baseline scenario provided similarly to focal \n \n",
                    "## burden_outcome \n",
                    "burden outcomes. \n",
                    "Use '*' when the model is using simplified deaths and cases definitions. Otherwise, list burden outcomes in the form of\n",
                    "<deaths_outcome1>,<deaths_outcome2>,<deaths_outcome3>;<cases_outcome1>,<cases_outcome2>,<cases_outcome3> \n",
                    "NO spacing is allowed. DO NOT provide dalys, as dalys will be added in automatically."), man)
  close(man)
  message("Generated impact recipe template in directory recipe.")
}


get_meta_from_recipe <- function(default_recipe = TRUE, method = "method0", recipe_version = "201710", recipe = NULL, con, disease = NULL){

  if (default_recipe){
    assert_method(method)
    recipe <- assert_version(method, recipe_version)
    recipe <- read_csv(system_file("recipe", method, recipe))
  } else {
    if (!grepl(".csv", recipe)){
      stop("Individualised recipe shall be presented as a csv file.")
    } else {
      recipe <- read_csv(recipe)
    }
    assert_recipe_format(recipe)

  }
  if(!is.null(disease)){
    recipe <- recipe[recipe$disease %in% disease, ]
  }
  recipe$focal <- gsub(";", ",", recipe$focal)
  recipe$baseline <- gsub(";", ",", recipe$baseline)
  for (i in seq_along(recipe$burden_outcome)){
    if (recipe$burden_outcome[i] == "*"){
      recipe$burden_outcome[i] <- "deaths;cases;dalys"
    } else {
      recipe$burden_outcome[i] <- paste(recipe$burden_outcome[i], "dalys", sep = ";")
    }
  }

  ## expand recipe
  burden_outcomes <- DBI::dbReadTable(con, "burden_outcome")

  meta <- NULL
  for (i in seq_along(recipe[, 1])){
    t <- recipe[i, ]
    t$touchstone <- ifelse(!grepl("-", t$touchstone), get_touchstone(con, t[["touchstone"]]), t$touchstone)

    ## split focal and baseline to multiple rows
    t_focal <- t
    t_focal$baseline <- NULL
    names(t_focal)[which(names(t_focal) == "focal")] <- "meta"
    t_focal$meta_type <- "focal"

    t_baseline <- t
    t_baseline$focal <- NULL
    names(t_baseline)[which(names(t_baseline) == "baseline")] <- "meta"
    t_baseline$meta_type <- "baseline"

    d <- rbind(t_baseline, t_focal)
    d$index <- i
    d$scenario_type <- NA
    d$vaccine_delivery <- NA
    for (j in seq_along(d$meta)){
      v <- unlist(strsplit(d$meta[j], ":"))
      d$scenario_type[j] <- v[1]
      d$vaccine_delivery[j] <- ifelse(length(v) == 1L, "", v[-1])
    }

    ## get all possible meta data
    ## make this a view, eventually
    ## given disease, touchstone, model, find all scenario, coverage_sets and burden_estimate_sets.
    sql <- sprintf(paste("SELECT scenario_type, vaccine, activity_type, gavi_support_level,
                       scenario.id AS scenario_id, coverage_set.id AS coverage_set_id, current_burden_estimate_set",
                         "FROM scenario",
                         "LEFT JOIN scenario_coverage_set",
                         "ON scenario_coverage_set.scenario = scenario.id",
                         "LEFT JOIN coverage_set",
                         "ON coverage_set.id = scenario_coverage_set.coverage_set",
                         "JOIN scenario_description",
                         "ON scenario_description.id = scenario.scenario_description",
                         "JOIN responsibility",
                         "ON responsibility.scenario = scenario.id",
                         "JOIN responsibility_set",
                         "ON responsibility_set.id = responsibility.responsibility_set",
                         "WHERE scenario.touchstone = '%s'",
                         "AND modelling_group = '%s'",
                         "AND disease = '%s'"),
                   t$touchstone,
                   t$modelling_group,
                   t$disease)

    browser()
    meta_all <- DBI::dbGetQuery(con, sql)
    ## remove yf reactive sias - otherwise cannot match with recipe
    j <- meta_all$vaccine == "YF" & meta_all$gavi_support_level == "none"
    meta_all <- meta_all[!j, ]
    meta_all$gavi_support_level <- NULL
    meta_all$vaccine_delivery <- paste(meta_all$vaccine, meta_all$activity_type, sep = "-")
    meta_all$vaccine_delivery[meta_all$vaccine_delivery == "none-none"] <- ""

    scenario_id <- unique(meta_all$scenario_id)
    d2 <- NULL
    for (j in scenario_id){
      v <- meta_all[meta_all$scenario_id == j, ]
      v$vaccine <- NULL
      v$activity_type <- NULL
      d2 <- rbind(d2, data_frame(scenario_type = v$scenario_type[1],
                                 vaccine_delivery = paste(v$vaccine_delivery, collapse = ","),
                                 scenario = v$scenario_id[1],
                                 coverage_set = paste(v$coverage_set_id, collapse = ","),
                                 burden_estimate_set = v$current_burden_estimate_set[1]))
    }
    meta_all <- d2

    ### filtering
    ### now work with meta_all and d
    ### order vaccine_delivery, so that can match
    meta_all$vaccine_delivery <- order_vaccine_delivery(meta_all$vaccine_delivery)
    d$vaccine_delivery <- order_vaccine_delivery(d$vaccine_delivery)
    d2 <- merge_by_common_cols(d, meta_all, all.x = TRUE)
    d2$burden_outcome_id <- replace_burden_outcome(burden_outcomes, d2$burden_outcome)
    ### update burden_outcome to be burden_outcome.id
    meta <- rbind(meta, d2)
  }
  meta$vaccine_delivery[meta$scenario_type == "novac"] <- "no-vaccination"
  stopifnot(all(!is.na(meta$scenario)))
  meta$method <- method
  meta
}

## checking impact calculation method parameter
assert_method <- function(method){
  dirs <- list.dirs(system_file("recipe"), full.names = FALSE)
  if (method %!in% dirs || method == ""){
    stop("Unknown user defined method. method must be one of method0, method1, method2a, method2b.")
  }
}

## checking impact recipe version parameter (if default recipe)
assert_version <- function(method, recipe_version){
  files <- list.files(system_file("recipe", method))
  i <- grepl(recipe_version, files)
  if (!i){
    stop("Unknown user defined recipe_version. Recipe version naming convertion is like 201710, 201810, 201910.")
  } else {
    file <- files[i]
    stopifnot(length(file) == 1L)
  }
  file
}

## checking impact recipe (if individualised recipe)
assert_recipe_format <- function(recipe){
  recipe_cols <- c("touchstone", "modelling_group", "disease", "focal", "baseline",
                   "burden_outcome")
  i <- setdiff(recipe_cols, names(recipe))
  if (length(i) > 0L){
    stop(sprintf("Missing recipe info needed - (%s)", paste(i, collapse = ", ")))
  }
}

## for matching vaccine delivery between the recipe and montagu
order_vaccine_delivery <- function(a){
  t <- rep(NA, length(a))
  for (i in seq_along(a)){
    v <- unlist(strsplit(a[i], ","))
    t[i] <- paste(v[order(v)], collapse = ",")
  }
  t
}

replace_burden_outcome <- function(burden_outcomes, a){
  t <- rep(NA, length(a))
  for (i in seq_along(a)){
    v <- unlist(strsplit(a[i], ";"))
    m <- rep(NA, length(v))
    for (j in seq_along(v)){
      v1 <- unlist(strsplit(v[j], ","))
      v1 <- burden_outcomes$id[match(v1, burden_outcomes$code)]
      m[j] <- paste(v1, collapse = ",")
    }
    m2 <- m
    k <- grepl("deaths", v)
    m2[1] <- m[k]
    k <- grepl("cases", v)
    m2[2] <- m[k]
    k <- grepl("dalys", v)
    m2[3] <- m[k]

    t[i] <- paste(m2, collapse = ";")
  }
  if(any(grepl("NA", t))){
    stop("Burden outcome not defined in Montagu.")
  }
  t
}
