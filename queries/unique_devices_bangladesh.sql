SELECT
  concat(month,'/',day,'/',year) AS date, domain AS access_method, SUM(uniques_estimate) as unique_devices
FROM 
	wmf.unique_devices_per_domain_daily
WHERE year =2018 
  AND ( (month = 3) OR (month = 4) OR (month = 5 AND day <=24) )
  AND country = 'Bangladesh'
AND (domain = 'bn.wikipedia.org' OR  domain = 'bn.m.wikipedia.org')
GROUP BY year, month, day, domain;
