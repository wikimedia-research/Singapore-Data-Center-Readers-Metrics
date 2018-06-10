--Pageviews by countries with breakdown by access method. 
--Remotely from Stat5
--Pageviews by countries with breakdown by browser familty 
--Remotely from Stat5
SELECT 
concat(month,'/',day,'/',year) AS date, country,  user_agent_map['browser_family'] AS browser_family, sum(view_count) AS pageviews
FROM 
wmf.pageview_hourly
WHERE year =2018 
AND ( (month = 3) OR (month = 4) OR (month = 5 AND day <= 25) ) 
--remove any bots and spiders. Appear to be minimal but removing just in case. 
AND agent_type = 'user'
-- impacted countries per tracking list found here: https://phabricator.wikimedia.org/T189252. Included all countries marked as "Done"
AND country IN ("Bangladesh", "Brunei Darussalam", "Bhutan", "Cocos (Keeling) Islands", "Christmas Island", 
                "Hong Kong", "Indonesia", "India", "Japan", "Cambodia", "Korea, Republic of", "Lao People's Democratic Republic",
                "Sri Lanka", "Mongolia", "Macao", "Maldives", "Malaysia", "Nepal", "Philippines", "Pakistan", "Singapore",
                "Taiwan", "Vietnam", "Australia", "Micronesia, Federated States of", "Guam", "Kiribati",
                "Marshall Islands", "Northern Mariana Islands", "New Caledonia", "Palau", "Tuvalu", "United States Minor Outlying Islands")
GROUP BY year, month, day, country, user_agent_map['browser_family'];


