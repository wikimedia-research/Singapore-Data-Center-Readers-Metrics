library(tidyverse)
library(ggplot2)
library(scales)
library(reshape2)
library(lubridate)
library(zoo)

fig_path <- file.path("figures")
plot_resolution <- 192


##Pageviews by countries for all eqsin target countries identified as "Done" in https://phabricator.wikimedia.org/T189252

pageviews_bycountry <- read.delim("data/pageviews_bycountry.tsv", sep = "\t", stringsAsFactors =FALSE)
pageviews_bycountry$date <- as.Date(pageviews_bycountry$date, format = "%m/%d/%Y")
pageviews_bycountry$access_method[!pageviews_bycountry$access_method == "desktop"] <- "mobile (web + app)"
  
#Find total daily Wikimedia pageviews for all eqsin regional countries
pageviews_daily_allcountries <- pageviews_bycountry %>%
  dplyr::group_by(date, access_method) %>%
  dplyr::summarise(total_views = sum(pageviews)) %>%
  dplyr::arrange(desc(date))

p <- ggplot(pageviews_daily_allcountries, aes(x = date, y = total_views, color = access_method)) +
  geom_line() +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  scale_y_continuous("pageviews (excludes bots)", labels = polloi::compress) +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Daily Wikimedia pageviews for all eqsin regional countries with service rollout") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("daily_pageviews_allcountries.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

#Daily Wikimedia pageviews for Japan

pageviews_japan<- pageviews_bycountry %>%
  dplyr:: filter(country == "Japan" ) %>%
  dplyr::group_by(date, access_method) %>%
  dplyr::summarise(total_views = sum(pageviews))

p <- ggplot(pageviews_japan, aes(x = date, y = total_views, color = access_method)) +
  geom_line() +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-28")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-28'), y=20E6, label="switch to Singapore datacenter (page load time 1.4s --> 1.2s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("pageviews (excludes bots)", labels = polloi::compress) +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Daily Wikimedia pageviews for Japan") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("daily_pageviews_japan.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

#Daily Wikimedia pageviews for Bangladesh

pageviews_bangladesh<- pageviews_bycountry %>%
  ## filter for all EQSIN Target Country List (in "Asia" (as defined by MaxMind)) 
  dplyr::filter(country == "Bangladesh") %>%
  dplyr::group_by(date, access_method) %>%
  dplyr::summarise(total_views = sum(pageviews))

p <- ggplot(pageviews_bangladesh, aes(x = date, y = total_views, color = access_method)) +
  geom_line() +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-28")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-28'), y=6E5, label="switch to Singapore datacenter (page load time 3.2s --> 2.1s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("pageviews (excludes bots)", labels = polloi::compress) +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Daily Wikimedia pageviews for Bangladesh") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("daily_pageviews_bangladesh.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


#Daily Wikimedia pageviews for Indonesia

pageviews_indonesia<- pageviews_bycountry %>%
  ## filter for all EQSIN Target Country List (in "Asia" (as defined by MaxMind)) 
  dplyr::filter(country == "Indonesia") %>%
  dplyr::group_by(date, access_method) %>%
  dplyr::summarise(total_views = sum(pageviews))

p <- ggplot(pageviews_indonesia, aes(x = date, y = total_views, color = access_method)) +
  geom_line() +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-26")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-26'), y=3E6, label="switch to Singapore datacenter (page load time 3s --> 2s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("pageviews (excludes bots)", labels = polloi::compress) +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Daily Wikimedia pageviews from Indonesia") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("daily_pageviews_indonesia.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


#Daily Wikimedia pageviews for India

pageviews_india<- pageviews_bycountry %>%
  dplyr::filter(country == "India")%>%
  dplyr::group_by(date, access_method) %>%
  dplyr::summarise(total_views = sum(pageviews))

p <- ggplot(pageviews_india, aes(x = date, y = total_views, color = access_method)) +
  geom_line() +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-29")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-29'), y=10E6, label="switch to Singapore datacenter"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("pageviews (excludes bots)", labels = polloi::compress) +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Daily Wikimedia pageviews for India") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("daily_pageviews_india.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


##Uniques estimate for eqsin target countries identified as "Done" in https://phabricator.wikimedia.org/T189252

unique_devices_allcountries_wiki <- read.delim("data/unique_devices_allcountries.tsv", sep = "\t", stringsAsFactors =FALSE)
unique_devices_allcountries_wiki$date <- as.Date(unique_devices_allcountries_wiki$date, format = "%m/%d/%Y")

#Find daily unique devices for all eqsin impacted countries for all Wikipedia projects
unique_devices_allcountries <- unique_devices_allcountries_wiki %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(total_uniques = sum(unique_devices)) %>%
  dplyr::arrange(desc(date))

p <- ggplot(unique_devices_allcountries, aes(x = date, y = total_uniques)) +
  geom_line() +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  scale_y_continuous("No of Unique Devices", labels = polloi::compress) +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Daily unique devices estimate for all eqsin regional countries with service rollout (All Wikipedias)",
       subtitle = "Mobile web and Desktop") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("daily_unique_devices_allcountries.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

#Review of daily unique devices for countries with significant load time changes on respective Wikipedia domains.  

#Review of daily unique devices in Japanese Wikipedia (ja.wikipedia)

unique_devices_japan<- read.delim("data/unique_devices_japan.tsv", sep = "\t", stringsAsFactors =FALSE)
unique_devices_japan$date <- as.Date(unique_devices_japan$date, format = "%m/%d/%Y")
unique_devices_japan$access_method<- ifelse(unique_devices_japan$access_method == "ja.wikipedia.org", "desktop", "mobile web")

p <- ggplot(unique_devices_japan, aes(x = date, y = unique_devices, color = access_method)) +
  geom_line() +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-28")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-28'), y=5E6, label="switch to Singapore datacenter (page load time 1.4s --> 1.2s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("No of Unique Devices", labels = polloi::compress) +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Daily unique devices estimate in Japan for ja.wikipedia.org and ja.m.wikipedia.org (without apps)") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("daily_uniques_japan.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


#Review of daily unique devices in Indonesia Wikipedia (id.wikipedia)

unique_devices_indonesia<- read.delim("data/unique_devices_indonesia.tsv", sep = "\t", stringsAsFactors =FALSE)
unique_devices_indonesia$date <- as.Date(unique_devices_indonesia$date, format = "%m/%d/%Y")
unique_devices_indonesia$access_method<- ifelse(unique_devices_indonesia$access_method == "id.wikipedia.org", "desktop", "mobile web")

p <- ggplot(unique_devices_indonesia, aes(x = date, y = unique_devices, color = access_method)) +
  geom_line() +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-26")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-26'), y=1E6, label="switch to Singapore datacenter (page load time 3s --> 2s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("No of Unique Devices (excludes apps)", labels = polloi::compress) +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Unique devices in Indonesia for id.wikipedia.org and id.m.wikipedia.org") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("daily_uniques_indonesia.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


#Review of daily unique devices in Bangladesh on in Bengali Wikipedia (bn.wikipedia)

unique_devices_bangladesh<- read.delim("data/unique_devices_bangladesh.tsv", sep = "\t", stringsAsFactors =FALSE)
unique_devices_bangladesh$date <- as.Date(unique_devices_bangladesh$date, format = "%m/%d/%Y")
unique_devices_bangladesh$access_method<- ifelse(unique_devices_bangladesh$access_method == "bn.wikipedia.org", "desktop", "mobile web")

p <- ggplot(unique_devices_bangladesh, aes(x = date, y = unique_devices, color = access_method)) +
  geom_line() +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-26")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-26'), y=4E4, label="switch to Singapore datacenter (page load time 3.2s --> 2.1s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("No of Unique Devices", labels = polloi::compress) +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Daily unique devices estimate in Bangladesh for bn.wikipedia.org and bn.m.wikipedia.org (without apps)") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("daily_uniques_bangladesh.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


#Review of daily unique devices in India on Hindi Wikipedia (hi.wikipedia)

unique_devices_india<- read.delim("data/unique_devices_india.tsv", sep = "\t", stringsAsFactors =FALSE)
unique_devices_india$date <- as.Date(unique_devices_india$date, format = "%m/%d/%Y")
unique_devices_india$access_method<- ifelse(unique_devices_india$access_method == "hi.wikipedia.org", "desktop", "mobile web")

p <- ggplot(unique_devices_india, aes(x = date, y = unique_devices, color = access_method)) +
  geom_line() +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-26")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-26'), y=30E4, label="switch to Singapore datacenter"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("No of Unique Devices", labels = polloi::compress) +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Daily unique devices estimate in India for hi.wikipedia.org and hi.m.wikipedia.org (without apps)") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("daily_uniques_india.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


## Year-Over-Year Pageview Analysis; 2016-2018

##for all eqsin target countries identified as "Done" in https://phabricator.wikimedia.org/T189252

pageviews_bycountry_2016_18 <- read.delim("data/pageviews_bycountry_2016_2018.tsv", sep = "\t", stringsAsFactors =FALSE)
pageviews_bycountry_2016_18$access_method[!pageviews_bycountry_2016_18$access_method == "desktop"] <- "mobile (web + app)"
pageviews_bycountry_2016_18$date <- as.Date(pageviews_bycountry_2016_18$date, format = "%m/%d/%Y")


#YOY for Japan - Total/Combined mobile and desktop
pageviews_japan_yoy_total<- pageviews_bycountry_2016_18 %>%
  dplyr::filter(country == "Japan") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(total_views = sum(pageviews)) 

#Plot pageviews (7-day moving average) with dates (converted to day of year on x-axis)
p <- ggplot(pageviews_japan_yoy_total, aes(x = as.Date(yday(date),"1970-01-01"), y = total_views, color = as.factor(year(date)))) +
  geom_line(aes(y=rollmean(total_views, 7, fill= NA)), size = 1.5) +
  geom_vline(xintercept = as.Date(yday("2018-03-28")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date(yday('2018-03-28'), "1970-01-01"), y=35E6, label="switch to Singapore datacenter (page load time 1.4s --> 1.2s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("pageviews/day (excludes bots), 7-day average", labels = polloi::compress) +
  scale_x_date(date_breaks = "months", date_labels = "%b %d", name = "Date") +
  labs(title = "Daily Wikimedia pageviews year-over-year comparison for Japan (mobile + desktop combined), 2016-2018") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"),
        legend.title=element_blank())

ggsave("daily_pageviews_japan_yoy_total.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

#YOY for Japan - Mobile
pageviews_japan_yoy_mobile<- pageviews_bycountry_2016_18 %>%
  dplyr::filter(country == "Japan",
                access_method == 'mobile (web + app)') %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(total_views = sum(pageviews))

#Plot pageviews (7-day moving average) with dates
p <- ggplot(pageviews_japan_yoy_mobile, aes(x = as.Date(yday(date),"1970-01-01"), y = total_views, color = as.factor(year(date)))) +
  geom_line(aes(y=rollmean(total_views, 7, fill= NA)), size = 1.5) +
  geom_vline(xintercept = as.Date(yday('2018-03-28')),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date(yday('2018-03-28'), "1970-01-01"), y=22E6, label="switch to Singapore datacenter (page load time 1.4s --> 1.2s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("mobile (web+apps) pageviews/day (excludes bots), 7-day average", labels = polloi::compress) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d", name = "Date") +
  labs(title = "Daily Wikimedia mobile pageviews year-over-year comparison for Japan, 2016-2018 ") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"),
        legend.title=element_blank())

ggsave("daily_pageviews_japan_yoy_mobile.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


#YOY for Japan - Desktop
pageviews_japan_yoy_desktop<- pageviews_bycountry_2016_18 %>%
  dplyr::filter(country == "Japan",
                access_method == 'desktop') %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(total_views = sum(pageviews))

#Plot pageviews (7-day moving average) with dates
p <- ggplot(pageviews_japan_yoy_desktop, aes(x = as.Date(yday(date),"1970-01-01"), y = total_views, color = as.factor(year(date)))) +
  geom_line(aes(y=rollmean(total_views, 7, fill= NA)), size = 1.5) +
  geom_vline(xintercept = as.Date(yday('2018-03-28')),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date(yday('2018-03-28'), "1970-01-01"), y=15E6, label="switch to Singapore datacenter (page load time 1.4s --> 1.2s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("desktop pageviews/day (excludes bots), 7-day average", labels = polloi::compress) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d", name = "Date") +
  labs(title = "Daily desktop Wikimedia pageviews year-over-year comparison for Japan, 2016-2018") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"),
        legend.title=element_blank())

ggsave("daily_pageviews_japan_yoy_desktop.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

##YOY for Bangladesh
### Total/Combined mobile and desktop
pageviews_bangladesh_yoy_total<- pageviews_bycountry_2016_18 %>%
  dplyr::filter(country == "Bangladesh") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(total_views = sum(pageviews))

#Plot pageviews (7-day moving average) with dates
p <- ggplot(pageviews_bangladesh_yoy_total, aes(x = as.Date(yday(date),"1970-01-01"), y = total_views, color = as.factor(year(date)))) +
  geom_line(aes(y=rollmean(total_views, 7, fill= NA)), size = 1.5) +
  geom_vline(xintercept = as.Date(yday("2018-03-28")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date(yday('2018-03-28'), "1970-01-01"), y=1E6, label="switch to Singapore datacenter (page load time 3.2s --> 2.1s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("pageviews/day (excludes bots), 7-day average", labels = polloi::compress) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d", name = "Date") +
  labs(title = "Daily Wikimedia pageviews year-over-year comparison for Bangladesh (mobile + desktop combined), 2016-2018") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"),
        legend.title=element_blank())

ggsave("daily_pageviews_bangladesh_yoy_total.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

#YOY for Bangladesh - Mobile
pageviews_bangladesh_yoy_mobile<- pageviews_bycountry_2016_18 %>%
  dplyr::filter(country == 'Bangladesh',
                access_method == 'mobile (web + app)') %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(total_views = sum(pageviews))

#Plot pageviews (7-day moving average) with dates
p <- ggplot(pageviews_bangladesh_yoy_mobile, aes(x = as.Date(yday(date),"1970-01-01"), y = total_views, color = as.factor(year(date)))) +
  geom_line(aes(y=rollmean(total_views, 7, fill= NA)), size = 1.5) +
  geom_vline(xintercept = as.Date(yday('2018-03-28')),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date(yday('2018-03-28'), "1970-01-01"), y=550E3, label="switch to Singapore datacenter (page load time 3.2s --> 2.1s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("mobile (web+apps) pageviews/day (excludes bots), 7-day average", labels = polloi::compress) +
  scale_x_date(date_breaks = "months", date_labels = "%b %d", name = "Date") +
  labs(title = "Daily Wikimedia mobile pageviews year-over-year comparison for Bangladesh, 2016-2018 ") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"),
        legend.title=element_blank())

ggsave("daily_pageviews_bangladesh_yoy_mobile.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


#YOY for Bangladesh - Desktop
pageviews_bangladesh_yoy_desktop<- pageviews_bycountry_2016_18 %>%
  dplyr::filter(country == 'Bangladesh',
                access_method == 'desktop') %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(total_views = sum(pageviews))

#Plot pageviews (7-day moving average) with dates
p <- ggplot(pageviews_bangladesh_yoy_desktop, aes(x = as.Date(yday(date),"1970-01-01"), y = total_views, color = as.factor(year(date)))) +
  geom_line(aes(y=rollmean(total_views, 7, fill= NA)), size =1.5) +
  geom_vline(xintercept = as.Date(yday('2018-03-28')),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date(yday('2018-03-28'), "1970-01-01"), y=400E3, label="switch to Singapore datacenter (page load time 3.2s --> 2.1s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("desktop pageviews/day (excludes bots), 7-day average", labels = polloi::compress) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d", name = "Date") +
  labs(title = "Daily desktop Wikimedia pageviews year-over-year comparison for Bangladesh, 2016-2018") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"),
        legend.title=element_blank())

ggsave("daily_pageviews_bangladesh_yoy_desktop.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

##YOY for Indonesia
### Total/Combined mobile and desktop
pageviews_indonesia_yoy_total<- pageviews_bycountry_2016_18 %>%
  dplyr::filter(country == "Indonesia") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(total_views = sum(pageviews))

#Plot pageviews (7-day moving average) with dates
p <- ggplot(pageviews_indonesia_yoy_total, aes(x = as.Date(yday(date),"1970-01-01"), y = total_views, color = as.factor(year(date)))) +
  geom_line(aes(y=rollmean(total_views, 7, fill= NA)), size =1.5) +
  geom_vline(xintercept = as.Date(yday("2018-03-26")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date(yday('2018-03-26'), "1970-01-01"), y=5E6, label="switch to Singapore datacenter (page load time 3s --> 2s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("pageviews/day (excludes bots), 7-day average", labels = polloi::compress) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d", name = "Date") +
  labs(title = "Daily Wikimedia pageviews year-over-year comparison for Indonesia (mobile + desktop combined), 2016-2018") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"),
        legend.title=element_blank())

ggsave("daily_pageviews_indonesia_yoy_total.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

#YOY for Indonesia - Mobile
pageviews_indonesia_yoy_mobile<- pageviews_bycountry_2016_18 %>%
  dplyr::filter(country == 'Indonesia',
                access_method == 'mobile (web + app)') %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(total_views = sum(pageviews))

#Plot pageviews (7-day moving average) with dates
p <- ggplot(pageviews_indonesia_yoy_mobile, aes(x = as.Date(yday(date),"1970-01-01"), y = total_views, color = as.factor(year(date)))) +
  geom_line(aes(y=rollmean(total_views, 7, fill= NA)), size =1.5) +
  geom_vline(xintercept = as.Date(yday('2018-03-26')),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date(yday('2018-03-26'), "1970-01-01"), y=3.5E6, label="switch to Singapore datacenter (page load time 3s --> 2s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("mobile (web+apps) pageviews/day (excludes bots), 7-day average", labels = polloi::compress) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d", name = "Date") +
  labs(title = "Daily Wikimedia mobile pageviews year-over-year comparison for Indonesia, 2016-2018 ") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"),
        legend.title=element_blank())

ggsave("daily_pageviews_indonesia_yoy_mobile.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


#YOY for Indonesia - Desktop
pageviews_indonesia_yoy_desktop<- pageviews_bycountry_2016_18 %>%
  dplyr::filter(country == 'Indonesia',
                access_method == 'desktop') %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(total_views = sum(pageviews))

#Plot pageviews (7-day moving average) with dates
p <- ggplot(pageviews_indonesia_yoy_desktop, aes(x = as.Date(yday(date),"1970-01-01"), y = total_views, color = as.factor(year(date)))) +
  geom_line(aes(y=rollmean(total_views, 7, fill= NA)), size = 1.5) +
  geom_vline(xintercept = as.Date(yday('2018-03-26')),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date(yday('2018-03-26'), "1970-01-01"), y=1.7E6, label="switch to Singapore datacenter (page load time 3s --> 2s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("desktop pageviews/day (excludes bots), 7-day average", labels = polloi::compress) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d", name = "Date") +
  labs(title = "Daily desktop Wikimedia pageviews year-over-year comparison for Indonesia, 2016-2018") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"),
        legend.title=element_blank())

ggsave("daily_pageviews_indonesia_yoy_desktop.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

##YOY for India
### Total/Combined mobile and desktop
pageviews_india_yoy_total<- pageviews_bycountry_2016_18 %>%
  dplyr::filter(country == "India") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(total_views = sum(pageviews))

#Plot pageviews (7-day moving average) with dates
p <- ggplot(pageviews_india_yoy_total, aes(x = as.Date(yday(date),"1970-01-01"), y = total_views, color = as.factor(year(date)))) +
  geom_line(aes(y=rollmean(total_views, 7, fill= NA)), size =1.5) +
  geom_vline(xintercept = as.Date(yday("2018-03-29")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date(yday('2018-03-29'), "1970-01-01"), y=21E6, label="switch to Singapore datacenter"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("pageviews/day (excludes bots), 7-day average", labels = polloi::compress) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d", name = "Date") +
  labs(title = "Daily Wikimedia pageviews year-over-year comparison for India (mobile + desktop combined), 2016-2018") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"),
        legend.title=element_blank())

ggsave("daily_pageviews_india_yoy_total.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

#YOY for India - Mobile
pageviews_india_yoy_mobile<- pageviews_bycountry_2016_18 %>%
  dplyr::filter(country == 'India',
                access_method == 'mobile (web + app)') %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(total_views = sum(pageviews))

#Plot pageviews (7-day moving average) with dates
p <- ggplot(pageviews_india_yoy_mobile, aes(x = as.Date(yday(date),"1970-01-01"), y = total_views, color = as.factor(year(date)))) +
  geom_line(aes(y=rollmean(total_views, 7, fill= NA)), size =1.5) +
  geom_vline(xintercept = as.Date(yday('2018-03-29')),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date(yday('2018-03-29'), "1970-01-01"), y=14E6, label="switch to Singapore datacenter"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("mobile (web+apps) pageviews/day (excludes bots), 7-day average", labels = polloi::compress) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d", name = "Date") +
  labs(title = "Daily Wikimedia mobile pageviews year-over-year comparison for India, 2016-2018 ") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"),
        legend.title=element_blank())

ggsave("daily_pageviews_india_yoy_mobile.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


#YOY for India - Desktop
pageviews_india_yoy_desktop<- pageviews_bycountry_2016_18 %>%
  dplyr::filter(country == 'India',
                access_method == 'desktop') %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(total_views = sum(pageviews))

#Plot pageviews (7-day moving average) with dates
p <- ggplot(pageviews_india_yoy_desktop, aes(x = as.Date(yday(date),"1970-01-01"), y = total_views, color = as.factor(year(date)))) +
  geom_line(aes(y=rollmean(total_views, 7, fill= NA)), size =1.5) +
  geom_vline(xintercept = as.Date(yday('2018-03-29')),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date(yday('2018-03-29'), "1970-01-01"), y=6.5E6, label="switch to Singapore datacenter"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("desktop pageviews/day (excludes bots), 7-day average", labels = polloi::compress) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d", name = "Date") +
  labs(title = "Daily desktop Wikimedia pageviews year-over-year comparison for India, 2016-2018") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"),
        legend.title=element_blank())

ggsave("daily_pageviews_india_yoy_desktop.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)