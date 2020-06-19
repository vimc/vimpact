SELECT country, year, age_from as age, gender.name AS gender, value
FROM demographic_statistic
JOIN touchstone_demographic_dataset
ON touchstone_demographic_dataset.demographic_dataset = demographic_statistic.demographic_dataset
JOIN demographic_statistic_type
ON demographic_statistic_type.id = demographic_statistic.demographic_statistic_type
JOIN gender
ON gender.id = demographic_statistic.gender
WHERE touchstone_demographic_dataset.touchstone = $1
AND demographic_statistic_type.code = $2
AND gender.name IN %s
%s -- country, age, year constrains
