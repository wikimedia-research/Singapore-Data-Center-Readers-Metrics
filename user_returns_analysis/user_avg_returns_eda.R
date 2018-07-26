#User Avg Returns Analysis
##avg returns within 31 days broken down by countries with significant load time decreases.

library(tidyverse)
library(ggplot2)
library(scales)
library(plyr)

fig_path <- file.path("figures/avg_user_returns")
plot_resolution <- 192


user_returns_bycountry <- read.delim("data/user_retention_allwikis_bycountry_31days.tsv", sep = "\t", stringsAsFactors =FALSE)
user_returns_bycountry$last_seen_date <- as.Date(user_returns_bycountry$last_seen_date, format = "%d-%b-%Y")


user_returns_bycountry$country  <- plyr::mapvalues(
  user_returns_bycountry$country, from=c("JP", "ID", "BD", "IN", "MY", "VN", "NC", "LK", "NP", "PK"), 
  to=c("Japan", "Indonesia", "Bangladesh", "India", "Malaysia", "Vietnam", "New Caledonia", "Sri Lanka", "Nepal","Pakistan"))

##Japan avg returns within 31 days

user_returns_jawiki_31days <- user_returns_bycountry %>%
  filter(country == "Japan")

p <- ggplot(user_returns_jawiki_31days, aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-28")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-28'), y=5.5, label="switch to Singapore datacenter (page load time 1.4s --> 1.2s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Average user returns within 31 days on all Wikipedia projects from Japan",
       caption ="Note: This measures average return time for only the users who return within 31 days. Does not account for users who do not return") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_returns_japan_31days.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

##Indonesia avg returns within 31 days

user_returns_idwiki_31days <- user_returns_bycountry %>%
  dplyr::filter(country == "Indonesia")

p <- ggplot(user_returns_idwiki_31days, aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-26")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-26'), y=6.5, label="switch to Singapore datacenter (page load time 3s --> 2s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Average user return time within 31 days on all Wikipedia Projects from Indonesia",
       caption ="Note: This measures average return time for only the users who return within 31 days. Does not account for users who do not return") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_returns_indonesia_31days.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

## Bangladesh avg returns within 31 days

user_returns_bdwiki_31days <-  user_returns_bycountry %>%
  filter(country == "Bangladesh")

p <- ggplot(user_returns_bdwiki_31days, aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-28")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-28'), y=6.5, label="switch to Singapore datacenter (page load time 3.2s --> 2.1s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Average user return time within 31 days on all Wikipedia projects from Bangladesh",
       caption ="Note: This measures average return time for only the users who return within 31 days. Does not account for users who do not return") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_returns_bangladesh_31days.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

## India avg returns within 31 days

user_returns_inwiki_31days <- user_returns_bycountry %>%
  filter(country == "India")

p <- ggplot(user_returns_inwiki_31days, aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-29")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-29'), y=6, label="switch to Singapore datacenter"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Average user return time within 31 days on all Wikipedia Projects from India",
       caption ="Note: This measures average return time for only the users who return within 31 days. Does not account for users who do not return") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_returns_india_31days.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


#avg returns within 31 days for all March 26 countries.
user_returns_March26 <- user_returns_bycountry %>%
  dplyr::filter(country %in% c("Indonesia", "Malaysia", "Vietnam", "New Caledonia")) %>%
  dplyr::group_by(last_seen_date, country) %>%
  dplyr::summarise(avg_days_till_next_access = sum(avg_days_till_next_access))

p <- ggplot(user_returns_March26, aes(x = last_seen_date, y = avg_days_till_next_access, color = country)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-26")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-26'), y=14, label="switch to Singapore datacenter"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Average user return time within 31 days on all Wikipedia Projects from countries with March 26 rollout date",
       subtitle = "Desktop and Mobile Web Combined") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_returns_March26_31days.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


#avg returns within 31 days for all March 28 countries.
user_returns_March28 <- user_returns_bycountry %>%
  dplyr::filter(country %in% c("Bangladesh", "Sri Lanka", "Nepal", "Pakistan")) %>%
  dplyr::group_by(last_seen_date, country)  %>%
  dplyr::summarise(avg_days_till_next_access = sum(avg_days_till_next_access))

p <- ggplot(user_returns_March28, aes(x = last_seen_date, y = avg_days_till_next_access, color = country)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-28")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-28'), y=12.5, label="switch to Singapore datacenter"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Average user return time within 31 days on all Wikipedia Projects from countries with March 28 rollout date",
       subtitle = "Desktop and Mobile Web Combined") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_returns_March28_31days.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)



##avg returns within 7 days broken down by countries with significant load time decreases.


user_returns_bycountry_7days <- read.delim("data/user_retention_allwikis_bycountry_7days.tsv", sep = "\t", stringsAsFactors =FALSE)
user_returns_bycountry_7days$last_seen_date <- as.Date(user_returns_bycountry_7days$last_seen_date, format = "%d-%b-%Y")

user_returns_bycountry_7days$country  <- plyr::mapvalues(
  user_returns_bycountry_7days$country, from=c("JP", "ID", "BD", "IN", "MY", "VN", "NC", "LK", "NP", "PK"), 
  to=c("Japan", "Indonesia", "Bangladesh", "India", "Malaysia", "Vietnam", "New Caledonia", "Sri Lanka", "Nepal","Pakistan"))

##Japan - avg returns within 7 days 
user_returns_jawiki_7days <- user_returns_bycountry_7days %>%
  dplyr::filter(country == "Japan")

p <- ggplot(user_returns_jawiki_7days, aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-28")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-28'), y=2.5, label="switch to Singapore datacenter (page load time 1.4s --> 1.2s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Average user returns within 7 days on all Wikipedia projects from Japan") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_returns_japan_7days.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

##Indonesia - avg returns within 7 days


user_returns_idwiki_7days <- user_returns_bycountry_7days %>%
  dplyr::filter(country == "Indonesia")

p <- ggplot(user_returns_idwiki_7days, aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-26")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-26'), y=2.8, label="switch to Singapore datacenter (page load time 3s --> 2s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Average user return time within 7 days on all Wikipedia Projects from Indonesia") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_returns_indonesia_7days.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

## Bangladesh- avg returns within 7 days 


user_returns_bdwiki_7days <-  user_returns_bycountry_7days %>%
  dplyr::filter(country == "Bangladesh")

p <- ggplot(user_returns_bdwiki_7days, aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-28")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-28'), y=2.6, label="switch to Singapore datacenter (page load time 3.2s --> 2.1s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Average user return time within 7 days on all Wikipedia projects from Bangladesh") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_returns_bangladesh_7days.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

## India- avg returns within 7 days


user_returns_inwiki_7days <- user_returns_bycountry_7days %>%
  dplyr::filter(country == "India")

p <- ggplot(user_returns_inwiki_7days, aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-29")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-29'), y=2.6, label="switch to Singapore datacenter"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Average user return time within 7 days on all Wikipedia Projects from India") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_returns_india_7days.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

#avg returns within 7 days for all March 26 countries.
user_returns_March26_7days <- user_returns_bycountry_7days %>%
  dplyr::filter(country %in% c("Indonesia", "Malaysia", "Vietnam", "New Caledonia")) %>%
  dplyr::group_by(last_seen_date, country) %>%
  dplyr::summarise(avg_days_till_next_access = sum(avg_days_till_next_access))

p <- ggplot(user_returns_March26_7days, aes(x = last_seen_date, y = avg_days_till_next_access, color = country)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-26")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-26'), y=6, label="switch to Singapore datacenter"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Average user return time within 7 days on all Wikipedia Projects from countries with March 26 rollout date",
       subtitle = "Desktop and Mobile Web Combined") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_returns_March26_7days.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


#avg returns within 7 days for all March 28 countries.
user_returns_March28_7days <- user_returns_bycountry_7days %>%
  dplyr::filter(country %in% c("Bangladesh", "Sri Lanka", "Nepal", "Pakistan")) %>%
  dplyr::group_by(last_seen_date, country)  %>%
  dplyr::summarise(avg_days_till_next_access = sum(avg_days_till_next_access))

p <- ggplot(user_returns_March28_7days, aes(x = last_seen_date, y = avg_days_till_next_access, color = country)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-28")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-28'), y=5.5, label="switch to Singapore datacenter"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Average user return time within 7 days on all Wikipedia Projects from countries with March 28 rollout date",
       subtitle = "Desktop and Mobile Web Combined") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_returns_March28_7days.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


##Long-term avg returns within 31 days [December 16, 2016 until May 28, 2018]

user_returns_bycountry_from2016 <- read.delim("data/user_retention_allwikis_bycountry_31days_long_term.tsv", sep = "\t", stringsAsFactors =FALSE)
user_returns_bycountry_from2016$last_seen_date <- as.Date(user_returns_bycountry_from2016$last_seen_date, format = "%d-%b-%Y")


##Japan avg returns within 31 days from 2016

user_returns_jawiki_31days_from2016 <- user_returns_bycountry_from2016 %>%
  dplyr::filter(country == "JP")

p <- ggplot(user_returns_jawiki_31days_from2016, aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-28")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-28'), y=5.5, label="switch to Singapore datacenter (page load time 1.4s --> 1.2s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "1 month")  +
  labs(title = "Average user returns within 31 days on all Wikipedia projects from Japan",
       caption ="Note: This measures average return time for only the users who return within 31 days. Does not account for users who do not return") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_returns_japan_31days_from2016.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

## Indonesia avg returns within 31 days from 2016

user_returns_idwiki_31days_from2016 <- user_returns_bycountry_from2016 %>%
  dplyr::filter(country == "ID")

p <- ggplot(user_returns_idwiki_31days_from2016, aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-26")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-26'), y=7, label="switch to Singapore datacenter (page load time 3s --> 2s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "1 month")  +
  labs(title = "Average user return time within 31 days on all Wikipedia Projects from Indonesia",
       caption ="Note: This measures average return time for only the users who return within 31 days. Does not account for users who do not return") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_returns_indonesia_31days_from2016.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

## Bangladesh avg returns within 31 days from 2016

user_returns_bdwiki_31days_from2016 <-  user_returns_bycountry_from2016 %>%
  dplyr::filter(country == "BD")

p <- ggplot(user_returns_bdwiki_31days_from2016, aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-28")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-28'), y=9, label="switch to Singapore datacenter (page load time 3.2s --> 2.1s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "1 month")  +
  labs(title = "Average user return time within 31 days on all Wikipedia projects from Bangladesh",
       caption ="Note: This measures average return time for only the users who return within 31 days. Does not account for users who do not return") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_returns_bangladesh_31days_from2016.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

## India avg returns within 31 days from 2016

user_returns_inwiki_31days_from2016 <- user_returns_bycountry_from2016 %>%
  dplyr::filter(country == "IN")

p <- ggplot(user_returns_inwiki_31days_from2016, aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-29")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-29'), y=6, label="switch to Singapore datacenter"), size=3, vjust = -1.2, angle = 90, color = "black") +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "1 month")  +
  labs(title = "Average user return time within 31 days on all Wikipedia Projects from India",
       caption ="Note: This measures average return time for only the users who return within 31 days. Does not account for users who do not return") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_returns_india_31days_from2016.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

