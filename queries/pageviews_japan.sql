SELECT 
  concat(month,'/',day,'/',year) AS date, access_method, sum(view_count) AS pageviews
FROM 
  wmf.pageview_hourly
  WHERE year =2018 
  AND ( (month = 3) OR (month = 4) OR (month = 5 AND day <=24) ) 
  AND agent_type = 'user'
  AND country = 'Japan'
  AND project = 'ja.wikipedia'
GROUP BY year, month, day, access_method;
