#User Retention Analysis
##avg returns within 31 days for all regional eqsin countries with service rollout.

library(tidyverse)
library(ggplot2)
library(scales)

fig_path <- file.path("figures")
plot_resolution <- 192


#User Retention on all Wikipedia projects from all eqsin countries

user_retention_all <- read.delim("data/user_retention_allregcountries.tsv", sep = "\t", stringsAsFactors =FALSE)
user_retention_all$last_seen_date <- as.Date(user_retention_all$last_seen_date, format = "%d-%b-%Y")

  
p <- ggplot(user_retention_all, aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
  geom_line() +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Average user return time within 31 days on all Wikipedia Projects",
       subtitle = "For all eqsin countries with service rollout") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_retention_allcountries.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


#User Retention on En Wiki projects from all eqsin countries

user_retention_enwiki <- read.delim("data/user_retention_allcountries_enwiki.tsv", sep = "\t", stringsAsFactors =FALSE)
user_retention_enwiki$last_seen_date <- as.Date(user_retention_enwiki$last_seen_date, format = "%d-%b-%Y")

p <- ggplot(user_retention_enwiki, aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
  geom_line() +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Average user return time within 31 days on English Wikipedia",
       subtitle = "For all eqsin countries with service rollout") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_retention_allcountries_enwiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


##avg returns within 31 days broken down by countries with significant load time decreases.

##Japan

user_retention_jawiki <- read.delim("data/user_retention_japan.tsv", sep = "\t", stringsAsFactors =FALSE)
user_retention_jawiki$last_seen_date <- as.Date(user_retention_jawiki$last_seen_date, format = "%d-%b-%Y")

p <- ggplot(user_retention_jawiki, aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-28")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-28'), y=5.5, label="switch to Singapore datacenter (page load time 1.4s --> 1.2s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Average user returns within 31 days on ja.wikipedia from Japan") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_retention_japan.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

##Indonesia

user_retention_idwiki <- read.delim("data/user_retention_indonesia.tsv", sep = "\t", stringsAsFactors =FALSE)
user_retention_idwiki$last_seen_date <- as.Date(user_retention_idwiki$last_seen_date, format = "%d-%b-%Y")

p <- ggplot(user_retention_idwiki, aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-26")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-26'), y=8, label="switch to Singapore datacenter (page load time 3s --> 2s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Average user return time within 31 days on id.wikipedia from Indonesia") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_retention_indonesia.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

## Bangladesh

user_retention_bnwiki <- read.delim("data/user_retention_bangladesh.tsv", sep = "\t", stringsAsFactors =FALSE)
user_retention_bnwiki$last_seen_date <- as.Date(user_retention_bnwiki$last_seen_date, format = "%d-%b-%Y")

p <- ggplot(user_retention_bnwiki, aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-28")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-28'), y=7.5, label="switch to Singapore datacenter (page load time 3.2s --> 2.1s)"), size=3, vjust = -1.2, angle = 90, color = "black") +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Average user return time within 31 days on bn.wikipedia from Bangladesh") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_retention_bangladesh.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

## India

user_retention_hiwiki <- read.delim("data/user_retention_india.tsv", sep = "\t", stringsAsFactors =FALSE)
user_retention_hiwiki$last_seen_date <- as.Date(user_retention_hiwiki$last_seen_date, format = "%d-%b-%Y")

p <- ggplot(user_retention_hiwiki, aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-29")),
             linetype = "dashed", color = "blue") +
  geom_text(aes(x=as.Date('2018-03-29'), y=7, label="switch to Singapore datacenter"), size=3, vjust = -1.2, angle = 90, color = "black") +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "5 days")  +
  labs(title = "Average user return time within 31 days on hi.wikipedia from India") +  
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave("user_retention_india.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)
