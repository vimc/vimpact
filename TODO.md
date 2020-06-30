### TODOs

* extract impact by recipe (done, with two versions, need to discuss with science which to take going forward)

* extract fvps by recipe - only need to identify which vaccines to use - by setdiff focal and baseline vaccine_delivery - and only applies to method 2a and 2b

* calculate impact - only export for method 2a and 2b for other users, as 0 and 1 are too simple
INPUT data sets are
impact - country, (time), value - time is not required for method2a, which aggregates impact at country-level
fvps - country, time, fvps

* interim update
INPUT data sets are
impact ratio - country, (time), value
fvps - country, time, fvps

* extract aggregated burden by recipe 
 - may be not necessary anymore, as this is output of method0 and method1 already
 - if needed, move burden calculation report here

* add test data
 - pine countries 
 - median runs for central estimates
 - hepb, yf and measles
 
* output templates for external users
IMPACT
 - recipe
 - impact_raw
 - fvps
 IU
 - impact_ratio_raw -> can be generated from IMPACT
 - fvps
 
 