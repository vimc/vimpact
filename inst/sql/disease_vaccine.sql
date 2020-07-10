SELECT DISTINCT disease, vaccine, activity_type
FROM scenario
JOIN scenario_coverage_set
ON scenario_coverage_set.scenario = scenario.id
JOIN coverage_set
ON coverage_set.id = scenario_coverage_set.coverage_set
JOIN scenario_description
ON scenario_description.id = scenario.scenario_description
WHERE scenario.touchstone = $1
AND vaccine != 'none'
AND scenario_description NOT LIKE '%LiST%'
AND scenario_description NOT LIKE '%high%'
AND scenario_description NOT LIKE '%low%'
AND scenario_description NOT LIKE '%best%'
