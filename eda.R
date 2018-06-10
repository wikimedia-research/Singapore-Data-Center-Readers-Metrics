library(tidyverse)
library(ggplot2)
library(scales)
library(reshape2)

fig_path <- file.path("figures")
plot_resolution <- 192


##Pageviews by countries for all eqsin target countries identified as "Done" in https://phabricator.wikimedia.org/T189252

pageviews_bycountry <- read.delim("data/pageviews_bycountry.tsv", sep = "\t", stringsAsFactors =FALSE)
pageviews_bycountry$date <- as.Date(pageviews_bycountry$date, format = "%m/%d/%Y")
#Relabel all mobile web and mobile app as mobile. 
pageviews_bycountry$access_method[!pageviews_bycountry$access_method == "desktop"] <- "mobile (web + app)"
  
#Find total daily Wikimedia pageviews for all eqsin regional countries
pageviews_daily_allcountries <- pageviews_bycountry %>%
  group_by(date, access_method) %>%
    summarise(total_views = sum(pageviews)) %>%
  arrange(desc(date))

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
  filter(country == "Japan" ) %>%
  group_by(date, access_method) %>%
  summarise(total_views = sum(pageviews))

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
  filter(country == "Bangladesh") %>%
  group_by(date, access_method) %>%
  summarise(total_views = sum(pageviews))

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
  filter(country == "Indonesia") %>%
  group_by(date, access_method) %>%
  summarise(total_views = sum(pageviews))

p <- ggplot(pageviews_indonesia, aes(x = date, y = total_views, color = access_method)) +
  geom_line() +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-26")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-26'), y=3E6, label="switch to Singapore datacenter (page load time 3s --> 2s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("pageviews (excludes bots)", labels = polloi::compress) +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Daily Wikimedia pageviews from Indonesia") +  
  ggthemes::theme_tufte(base_size = 14, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        plot.title = element_text( face="bold", size=18),
        panel.grid = element_line("gray70"),
        legend.position="bottom")

ggsave("daily_pageviews_indonesia.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


#Daily Wikimedia pageviews for India

pageviews_india<- pageviews_bycountry %>%
  filter(country == "India")%>%
  group_by(date, access_method) %>%
  summarise(total_views = sum(pageviews))

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

## March 30 Release Date countries

pageviews_march30<- pageviews_bycountry %>%
  ## filter for all EQSIN Target Country List with March 30 deploy date, (- Christmas Island for plot clarity) 
  filter(country %in% c("Bangladesh","BT", "Cocos (Keeling) Islands",
                        "Cambodia", "Korea, Republic of", "Lao People's Democratic Republic", 
                        "Mongolia", "Macao", "Maldives", "Taiwan")) %>%
  group_by(date, country) %>%
  summarise(total_views = sum(pageviews))

p <- ggplot(pageviews_march30, aes(x = date, y = total_views, color = country)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-30")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-30'), y=8E6, label="Service turn-up"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_log10("pageviews", labels = polloi::compress) +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Daily Wikmiedia Pageviews for all March 30 Service Rollout Countries") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("daily_pageviews_march30.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


##Uniques estimate for eqsin target countries identified as "Done" in https://phabricator.wikimedia.org/T189252

unique_devices_allcountries_wiki <- read.delim("data/unique_devices_allcountries.tsv", sep = "\t", stringsAsFactors =FALSE)
unique_devices_allcountries_wiki$date <- as.Date(unique_devices_allcountries_wiki$date, format = "%m/%d/%Y")

#Find daily unique devices for all eqsin impacted countries for all Wikipedia projects
unique_devices_allcountries <- unique_devices_allcountries_wiki %>%
  group_by(date) %>%
  summarise(total_uniques = sum(unique_devices)) %>%
  arrange(desc(date))

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


#Review of daily unique devices in Indonesia Wikipedia (in.wikipedia)

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
  ggthemes::theme_tufte(base_size = 14, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        plot.title = element_text( face="bold", size = 18),
        panel.grid = element_line("gray70"),
        legend.position="bottom")

ggsave("daily_uniques_indonesia.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


#Review of daily unique devices in Bangladesh on in Bengal Wikipedia (bn.wikipedia)

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

#break down by browser family

pageviews_bybrowserfamily <- read.delim("data/pageviews_by_browserfamily.tsv", sep = "\t", stringsAsFactors =FALSE)
pageviews_bybrowserfamily$date <- as.Date(pageviews_bybrowserfamily$date, format = "%m/%d/%Y")

#Find top 10 browsers 
top_browsers <- pageviews_bybrowserfamily %>%
  group_by(browser_family) %>%
  summarise(total_views = sum(as.numeric(pageviews)))  %>%
  top_n(10, total_views)

pageviews_bybrowserfamily_all <- pageviews_bybrowserfamily %>%
  filter(browser_family %in% top_browsers$browser_family)%>%
  group_by(date, browser_family) %>%
  summarise(total_views = sum(pageviews))


p <- ggplot(pageviews_bybrowserfamily_all, aes(x = date, y = total_views, color = browser_family)) +
  geom_line(size = 1.5) +
  scale_y_continuous("pageviews (excludes bots)", labels = polloi::compress) +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Total daily pageviews by browser for all eqsin impacted countries") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("daily_pageviews_bybrowser.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

## Look just at March 28 release dates
pageviews_bybrowserfamily_March28 <- pageviews_bybrowserfamily %>%
  filter(browser_family %in% top_browsers$browser_family,
         country %in% c("Hong Kong", "Philippines", "Bangladesh", "Sri Lanka", "Nepal", "Pakistan", "Japan"))%>%
  group_by(date, browser_family) %>%
  summarise(total_views = sum(pageviews))

p <- ggplot(pageviews_bybrowserfamily_March28 , aes(x = date, y = total_views, color = browser_family)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-28")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-28'), y=8E6, label="Deploy Date"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_log10("pageviews (excludes bots)", labels = polloi::compress) +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Total Daily Pageviews for March 28 Deploy Date") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("daily_pageviews__bybrowser_march28.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)



