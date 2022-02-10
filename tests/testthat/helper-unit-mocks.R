db <- tryCatch({
  # will work during devtools::check
  fakerbase::fb_load("montagu", package = "vimpact")
}, error = function(cond) {
  # will work during devtools::test
  return(fakerbase::fb_load("montagu", path = "../../inst"))
})

mock_coverage_set <- function(id = 1L:4L,
                              touchstone = rep("t1", 4),
                              vaccine = rep("HepB", 4),
                              gavi_support_level = c(rep("with", 2), rep("without", 2)),
                              activity_type = rep(c("routine", "campaign"), 2)) {

  db$coverage_set(id = id,
                  name = paste(vaccine, activity_type, gavi_support_level, sep = "-"),
                  touchstone = touchstone,
                  vaccine = vaccine,
                  gavi_support_level = gavi_support_level,
                  activity_type = activity_type)
}

mock_coverage <- function(id = 1L:5L,
                          coverage_set = rep(1L, 5),
                          year = 2020L:2024L,
                          country = rep("AFG", 5),
                          age_from = rep(1, 5),
                          age_to = rep(5, 5),
                          coverage = runif(5, max = 10000),
                          target = runif(5, max = 10000),
                          gavi_support = rep(TRUE, 5),
                          gender = rep(1L, 5)) {
  db$coverage(id = id,
              coverage_set = coverage_set,
              year = year,
              country = country,
              age_from = age_from,
              age_to = age_to,
              age_range_verbatim = paste(age_from, age_to, sep = "-"),
              coverage = coverage,
              target = target,
              gavi_support = gavi_support,
              gender = gender,
              proportion_risk = 1)
}

mock_gender <- function() {
  db$gender(id = c(1L, 2L, 3L),
            code = c("male", "female", "both"),
            name = c("Male", "Female", "Both"))
}

mock_country <- function(id = "AFG", nid = 1L, name = "Afghanistan") {
  db$country(id = id, nid = nid, name = name)
}

mock_scenario <- function(id = 1L,
                          touchstone = "t1",
                          scenario_description = "desc1",
                          focal_coverage_set = 1L) {
  db$scenario(id = id,
              touchstone = touchstone,
              scenario_description = scenario_description,
              focal_coverage_set = focal_coverage_set)
}

mock_scenario_description <- function(id = "desc1",
                                      description = "description",
                                      disease = "HepB",
                                      scenario_type = "bestcase") {
  db$scenario_description(id = id,
                          description = description,
                          disease = disease,
                          scenario_type = scenario_type)
}
