SELECT disease, country 
FROM country_disease_endemic 
WHERE touchstone = $1
