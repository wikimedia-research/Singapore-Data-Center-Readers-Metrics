--average days until next access, 7 days of returns, mobile vs desktop, by country 2018
-- for all regional eqsin countries. 

SELECT country_code as country, access_method, wmf_last_access as last_seen_date, 
-- average days until next access
SUM(((unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400) * view_count)/ SUM(view_count) AS avg_days_till_next_access 
FROM tbayer.webrequest_extract_bak 
WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') IS NOT NULL 
-- accessed between Jan 1 and April 29, 2018
AND unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') >= 1514764800 -- 01/01/2018 @ 12:00am (UTC)
AND unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') < 1524960000 -- 04/29/2018 @ 12:00am (UTC) (table has data until May 31 right now)
AND year = 2018
AND (access_method = 'desktop' OR access_method = 'mobile web') 
AND project_class = 'wikipedia' 
--isolate to regional countries with Singapore switchover
AND country_code IN ("BD", "BN", "BT", "CC", "CX", "HK", "ID", "IN", "JP", "KH", "KR", "LA",
    "LK", "MN", "MO", "MV", "MY", "NP", "PH", "PK", "SG", "TW", "VN", "AU", "FM", "GU", "KI",
    "MH", "MP", "NC", "PW", "TV", "UM")
-- avg returns within 7 days
AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 691200) -- 691200 seconds = 8 days
AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
GROUP BY country_code, wmf_last_access, access_method 
ORDER BY last_seen_date, access_method, avg_days_till_next_access 
LIMIT 10000;