--Look at unique devices per target country for all Wikipedia project
SELECT
  concat(month,'/',day,'/',year) AS date, country, SUM(uniques_estimate) as unique_devices
FROM 
	wmf.unique_devices_per_project_family_daily
WHERE year =2018 
  AND ( (month = 3) OR (month = 4) OR (month = 5 AND day <=24) )
  AND project_family = 'wikipedia'
  AND country IN ("Bangladesh", "Brunei Darussalam", "Bhutan", "Cocos (Keeling) Islands", "Christmas Island", 
    "Hong Kong", "Indonesia", "India", "Japan", "Cambodia", "Korea, Republic of", "Lao People's Democratic Republic",
    "Sri Lanka", "Mongolia", "Macao", "Maldives", "Malaysia", "Nepal", "Philippines", "Pakistan", "Singapore",
    "Taiwan", "Vietnam", "Australia", "Micronesia, Federated States of", "Guam", "Kiribati",
    "Marshall Islands", "Northern Mariana Islands", "New Caledonia", "Palau", "Tuvalu", "United States Minor Outlying Islands")
GROUP BY year, month, day, country;