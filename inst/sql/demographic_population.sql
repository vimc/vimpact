SELECT country, year, age_from as age, gender.name AS gender, value
    FROM demographic_statistic
    JOIN demographic_dataset
      ON demographic_statistic.demographic_dataset = demographic_dataset.id
    JOIN demographic_source
      ON demographic_dataset.demographic_source = demographic_source.id
    JOIN demographic_statistic_type
      ON demographic_dataset.demographic_statistic_type = demographic_statistic_type.id
    JOIN gender
      ON gender.id = demographic_statistic.gender
   WHERE demographic_source.code = $1
     AND demographic_statistic_type.code = $2
     AND gender.name IN %s
%s -- country, age, year constrains
