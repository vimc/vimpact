con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "montagu",
                      host = "localhost",
                      user = "vimc",
                      password = "changeme")

invisible(fakerbase::fb_generate(con, package_path = "."))
