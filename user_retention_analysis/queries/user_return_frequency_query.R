#Frequency of Returns for Countries with eqSin service roll-out.

#Remotely from Stat005
#Indonesia Peak on 2018-02-09 on desktop
start_date <- 1517961600  #2018-02-07
end_date <- 1518307200 #2018-02-11

return_frequency_Indonesia_peak<- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  
  query <- paste("
    SELECT '",date,"' AS date, 
    (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') 
    - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400 AS days_till_next_access, 
    COUNT (*) AS returns_each_day
    FROM tbayer.webrequest_extract_bak
    WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') = ", date, "
    AND year = 2018
    AND access_method = 'desktop'
    AND project_class = 'wikipedia' 
    AND country_code = 'ID'
    AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
    AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
    GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400
                 ;") 
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(return_frequency_Indonesia_peak, "return_frequency_Indonesia_peak.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/return_frequency_Indonesia_peak.rds return_frequency_Indonesia_peak.rds")

#Indonesia Drop on 2018-04-21
start_date <- 1524096000  #2018-04-19 
end_date <- 1524441600 #2018-04-23

return_frequency_Indonesia_drop<- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  
  query <- paste("
    SELECT '",date,"' AS date, 
    (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') 
    - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400 AS days_till_next_access, 
    COUNT (*) AS returns_each_day
    FROM tbayer.webrequest_extract_bak
    WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') = ", date, "
    AND year = 2018
    AND access_method = 'desktop' 
    AND project_class = 'wikipedia' 
    AND country_code = 'ID'
    AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
    AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
    GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400
                 ;") 
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(return_frequency_Indonesia_drop, "return_frequency_Indonesia_drop.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/return_frequency_Indonesia_drop.rds return_frequency_Indonesia_drop.rds")

##Bangladesh Trends

#Bangladesh Drop on mobile web between 2018-01-28 and 2018-01-30
start_date <- 1517097600  #2018-01-28
end_date <- 1517270400 #2018-01-30


return_frequency_Bangladesh_drop<- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  
  query <- paste("
                 SELECT '",date,"' AS date, 
                 (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') 
                 - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400 AS days_till_next_access, 
                 COUNT (*) AS returns_each_day
                 FROM tbayer.webrequest_extract_bak
                 WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') = ", date, "
                 AND year = 2018
                 AND access_method = 'mobile web' 
                 AND project_class = 'wikipedia' 
                 AND country_code = 'BD'
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
                 GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400
                 ;") 
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(return_frequency_Bangladesh_drop, "return_frequency_Bangladesh_drop.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/return_frequency_Bangladesh_drop.rds return_frequency_Bangladesh_drop.rds")

#Bangladesh Peak on mobile web between 2018-04-08 and 2018-04-10
start_date <- 1523145600  #2018-04-08
end_date <- 1523318400 #2018-04-10

return_frequency_Bangladesh_peak<- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  
  query <- paste("
                 SELECT '",date,"' AS date, 
                 (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') 
                 - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400 AS days_till_next_access, 
                 COUNT (*) AS returns_each_day
                 FROM tbayer.webrequest_extract_bak
                 WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') = ", date, "
                 AND year = 2018
                 AND access_method = 'mobile web' 
                 AND project_class = 'wikipedia' 
                 AND country_code = 'BD'
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
                 GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400
                 ;") 
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(return_frequency_Bangladesh_peak, "return_frequency_Bangladesh_peak.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/return_frequency_Bangladesh_peak.rds return_frequency_Bangladesh_peak.rds")

## India drop on desktop between 2018-01-25 and 2018-01-28

start_date <- 1516838400  #2018-01-25
end_date <- 1517097600 #2018-01-28

return_frequency_India_drop <- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  
  query <- paste("
                 SELECT '",date,"' AS date, 
                 (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') 
                 - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400 AS days_till_next_access, 
                 COUNT (*) AS returns_each_day
                 FROM tbayer.webrequest_extract_bak
                 WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') = ", date, "
                 AND year = 2018
                 AND access_method = 'desktop' 
                 AND project_class = 'wikipedia' 
                 AND country_code = 'IN'
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
                 GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400
                 ;") 
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(return_frequency_India_drop, "return_frequency_India_drop.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/return_frequency_India_drop.rds return_frequency_India_drop.rds")

## Japan drop on desktop on 2016-12-31

start_date <- 1483056000  #2016-12-30
end_date <- 1483142400 #2016-12-31

return_frequency_Japan_drop <- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  
  query <- paste("
                 SELECT '",date,"' AS date, 
                 (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') 
                 - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400 AS days_till_next_access, 
                 COUNT (*) AS returns_each_day
                 FROM tbayer.webrequest_extract_bak
                 WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') = ", date, "
                 AND year = 2016
                 AND access_method = 'mobile web' 
                 AND project_class = 'wikipedia' 
                 AND country_code = 'JP'
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
                 GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400
                 ;") 
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(return_frequency_Japan_drop, "return_frequency_Japan_drop.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/return_frequency_Japan_drop.rds return_frequency_Japan_drop.rds")


