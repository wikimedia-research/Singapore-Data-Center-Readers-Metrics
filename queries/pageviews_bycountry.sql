--Pageviews by countries with breakdown by access method. 
--Remotely from Stat5
SELECT 
  concat(month,'/',day,'/',year) AS date, country,  access_method, sum(view_count) AS pageviews
FROM 
  wmf.pageview_hourly
  WHERE year =2018 
  AND ( (month = 3) OR (month = 4) OR (month = 5 AND day <=26) ) 
  AND agent_type = 'user'
  AND country IN ("Bangladesh", "Brunei Darussalam", "Bhutan", "Cocos (Keeling) Islands", "Christmas Island", 
    "Hong Kong", "Indonesia", "India", "Japan", "Cambodia", "Korea, Republic of", "Lao People's Democratic Republic",
    "Sri Lanka", "Mongolia", "Macao", "Maldives", "Malaysia", "Nepal", "Philippines", "Pakistan", "Singapore",
    "Taiwan", "Vietnam", "Australia", "Micronesia, Federated States of", "Guam", "Kiribati",
    "Marshall Islands", "Northern Mariana Islands", "New Caledonia", "Palau", "Tuvalu", "United States Minor Outlying Islands")
GROUP BY year, month, day, country, access_method;

