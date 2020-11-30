with sums AS (
  SELECT
    {groups},
    periods.start_year as start_time,
    periods.end_year as end_time,
    SUM(deaths_default) as deaths_default,
    SUM(deaths_novac) as deaths_novac,
    SUM(deaths_impact) as deaths_impact,
    SUM(dalys_default) as dalys_default,
    SUM(dalys_novac) as dalys_novac,
    SUM(dalys_impact) as dalys_impact
  FROM
    {table}
  JOIN ({years}) as periods
  ON
    {table}.year BETWEEN periods.start_year AND periods.end_year
  {where}
  GROUP BY
    {groups}, run_id, stochastic_file_id,
    periods.start_year, periods.end_year)

SELECT
  {groups},
  start_time,
  end_time,
  avg(deaths_default) as deaths_default_mean,
  avg(deaths_novac) as deaths_novac_mean,
  avg(deaths_impact) as deaths_impact_mean,
  avg(dalys_default) as dalys_default_mean,
  avg(dalys_novac) as dalys_novac_mean,
  avg(dalys_impact) as dalys_impact_mean,
  {averted_avg}
  percentile_cont(0.025) WITHIN GROUP (ORDER BY deaths_default) AS deaths_default_q1,
  percentile_cont(0.025) WITHIN GROUP (ORDER BY deaths_novac) AS deaths_novac_q1,
  percentile_cont(0.025) WITHIN GROUP (ORDER BY deaths_impact) AS deaths_impact_q1,
  percentile_cont(0.025) WITHIN GROUP (ORDER BY dalys_default) AS dalys_default_q1,
  percentile_cont(0.025) WITHIN GROUP (ORDER BY dalys_novac) AS dalys_novac_q1,
  percentile_cont(0.025) WITHIN GROUP (ORDER BY dalys_impact) AS dalys_impact_q1,
  {averted_q1}
  percentile_cont(0.975) WITHIN GROUP (ORDER BY deaths_default) AS deaths_default_q3,
  percentile_cont(0.975) WITHIN GROUP (ORDER BY deaths_novac) AS deaths_novac_q3,
  percentile_cont(0.975) WITHIN GROUP (ORDER BY deaths_impact) AS deaths_impact_q3,
  percentile_cont(0.975) WITHIN GROUP (ORDER BY dalys_default) AS dalys_default_q3,
  percentile_cont(0.975) WITHIN GROUP (ORDER BY dalys_novac) AS dalys_novac_q3,
  percentile_cont(0.975) WITHIN GROUP (ORDER BY dalys_impact) AS dalys_impact_q3
  {averted_q3}
FROM
  sums
GROUP BY
  {groups}, start_time, end_time
