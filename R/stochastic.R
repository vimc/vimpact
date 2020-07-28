get_stochastic_data <- function(annex, table) {
  DBI::dbGetQuery(annex, sprintf("
  SELECT disease, country, year,
    avg(deaths_default) as deaths_default_mid,
    avg(deaths_novac) as deaths_novac_mid,
    avg(deaths_impact) as deaths_impact_mid,
    avg(dalys_default) as dalys_default_mid,
    avg(dalys_novac) as dalys_novac_mid,
    avg(dalys_impact) as dalys_impact_mid,
    percentile_cont(0.025) WITHIN GROUP (ORDER BY deaths_default) AS deaths_default_lo,
    percentile_cont(0.025) WITHIN GROUP (ORDER BY deaths_novac) AS deaths_novac_lo,
    percentile_cont(0.025) WITHIN GROUP (ORDER BY deaths_impact) AS deaths_impact_lo,
    percentile_cont(0.025) WITHIN GROUP (ORDER BY dalys_default) AS dalys_default_lo,
    percentile_cont(0.025) WITHIN GROUP (ORDER BY dalys_novac) AS dalys_novac_lo,
    percentile_cont(0.025) WITHIN GROUP (ORDER BY dalys_impact) AS dalys_impact_lo,
    percentile_cont(0.975) WITHIN GROUP (ORDER BY deaths_default) AS deaths_default_hi,
    percentile_cont(0.975) WITHIN GROUP (ORDER BY deaths_novac) AS deaths_novac_hi,
    percentile_cont(0.975) WITHIN GROUP (ORDER BY deaths_impact) AS deaths_impact_hi,
    percentile_cont(0.975) WITHIN GROUP (ORDER BY dalys_default) AS dalys_default_hi,
    percentile_cont(0.975) WITHIN GROUP (ORDER BY dalys_novac) AS dalys_novac_hi,
    percentile_cont(0.975) WITHIN GROUP (ORDER BY dalys_impact) AS dalys_impact_hi
  FROM %s
  GROUP BY disease, country, year;", table))
}