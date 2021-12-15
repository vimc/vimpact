mock_coverage_set <- function(id = 1:4,
                              touchstone = rep("t1", 4),
                              vaccine = rep("HepB", 4),
                              gavi_support_level = c(rep("with", 2), rep("without", 2)),
                              activity_type = rep(c("routine", "campaign"), 2)) {
  data.frame(id = id,
             name = paste(vaccine, activity_type, gavi_support_level, sep = "-"),
             touchstone = touchstone,
             vaccine = vaccine,
             gavi_support_level = gavi_support_level,
             activity_type = activity_type,
             stringsAsFactors = FALSE)
}

mock_coverage <- function(id = 1:5,
                          coverage_set = rep(1, 5),
                          year = 2020:2024,
                          country = rep("AFG", 5),
                          age_from = rep(1, 5),
                          age_to = rep(5, 5),
                          coverage = runif(5, max = 10000),
                          target = runif(5, max = 10000),
                          gavi_support = rep("with", 5),
                          gender = rep(1, 5)) {
  data.frame(id = id,
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
             stringsAsFactors = FALSE)
}

mock_gender <- function() {
  data.frame(id = c(1, 2, 3), code = c("male", "female", "both"), name = c("Male", "Female", "Both"), stringsAsFactors = FALSE)
}

mock_country <- function(id = "AFG", nid = 1, name = "Afghanistan") {
  data.frame(id = id, nid = nid, name = name, stringsAsFactors = FALSE)
}
