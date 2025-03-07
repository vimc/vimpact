# vimpact 0.1.5

* resolve a previous update that matches burden outcome to burden outcome code which did not capture the special case of rubella outcomes.

# vimpact 0.1.4

* Produce age specific calendar impact.

# vimpact 0.1.3

* Recognise new burden outcomes.

# vimpact 0.1.2

* Add to readme installation instruction.

# vimpact 0.1.1

* Able to recognise different no-vaccination scenario types.

# vimpact 0.1.0

* Increase flexibility - recognize new activity type, compare impact between different scenario types. 

# vimpact 0.0.10

* Include DTP3 in the disease vaccine delivery table to extract the coverage values from the DB

# vimpact 0.0.9

* Build get_population and extract_vaccination_history functions to pull data for specified demographic_source

# vimpact 0.0.8

* Fix impact_meta.R.

# vimpact 0.0.7

* Fix both csv and function interfaces, so that they work for 2021 runs and generate identical results.

# vimpact 0.0.6

* Add wrapper function for running impact calculations for many scenarios

# vimpact 0.0.5

* Replace impact calculation internals with dplyr

# vimpact 0.0.4

* Fix extract_vaccine_history() and cohort_deaths_all_cause(). (VIMC-4444)

# vimpact 0.0.3

* Add helper for retrieving year aggregated summary data

# vimpact 0.0.2 (2020-07-17)

* further constrain read_csv/read_sql to load system files when applicable

* add tests for impact_meta.R, commonly_used_db_data.R, and impact_central.R

# vimpact 0.0.1 (2020-06-18)

* impact calculation for central estimates
