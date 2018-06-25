
-- remotely from Stat5
--User Retention in Japan on ja.wikipedia
--average days until next access, 31 days of returns, mobile vs desktop, 2018

SELECT access_method, wmf_last_access as last_seen_date, 
-- average returns until next access
SUM(((unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400) * view_count)/ SUM(view_count) AS avg_days_till_next_access 
FROM tbayer.webrequest_extract_bak 
WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') IS NOT NULL 
-- accessed between Jan 1 and April 29, 2018
AND unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') >= 1514764800 -- 01/01/2018 @ 12:00am (UTC)
AND unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') < 1524960000 -- 04/29/2018 @ 12:00am (UTC) 
AND year = 2018
AND (access_method = 'desktop' OR access_method = 'mobile web') 
AND project = 'ja'
AND project_class = 'wikipedia' 
--isolate to regional countries with Singapore switchover
AND country_code = 'JP'
-- within 31 days
AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
GROUP BY wmf_last_access, access_method 
ORDER BY last_seen_date, access_method
LIMIT 10000;