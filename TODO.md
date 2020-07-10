### TODOs

* export get_meta_from_recipe function

* export impact_by_year_of_vaccination
  - also output templates of input data for external users
  - can do this upon testing vimpact_by_year_of_vaccination

* extract aggregated burden by recipe 
 - may be not necessary anymore, as this is output of method0 and method1 already, we can output it
 - but the output will only have counter-factual and best-estimate scenarios
 - if other scenarios are also needed, move burden calculation report here

* add test data (priority)
 - pine countries 
 - central estimates
 - hepb, yf and measles
 
* standardise output to match past data sets - this is only for internal use, no need to export for non-science users

* get_burden_central and get_burden_stochastic functions for easy-to-use
