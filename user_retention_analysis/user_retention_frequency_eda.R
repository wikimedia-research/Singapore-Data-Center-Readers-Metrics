##Frequency of unique device seen on WP on a selected date and returning within 31 days.

library(tidyverse)
library(ggplot2)
library(scales)

fig_path <- file.path("figures")
plot_resolution <- 192

#Indonesia increase around 2018-02-09 on desktop
return_frequency_id_peak <- rbind(readr::read_rds("data/return_frequency_Indonesia_peak.rds"))

##seen on WP on 2018-02-07 and returning within 31 days.
return_frequency_id_Feb7 <- return_frequency_id_peak %>%
  dplyr::filter(date == 1517961600)

p <- ggplot(return_frequency_id_Feb7) + 
  geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = "Days until next access for February 7, 2018 from Indonesia on desktop all Wikipedia Projects") +  
  wmf::theme_min()


ggsave("return_frequency_Indonesia_Feb7.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

##seen on WP on 2018-02-08 and returning within 31 days.
return_frequency_id_Feb8 <- return_frequency_id_peak %>%
  dplyr::filter(date == 1518048000)

p <- ggplot(return_frequency_id_Feb8) + 
  geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = "Days until next access for February 8, 2018 from Indonesia on desktop all Wikipedia Projects") +  
  wmf::theme_min()


ggsave("return_frequency_Indonesia_Feb8.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

##seen on WP on 2018-02-09 and returning within 31 days.
return_frequency_id_Feb9 <- return_frequency_id_peak %>%
  dplyr::filter(date == 1518134400)

p <- ggplot(return_frequency_id_Feb9) + 
  geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = "Days until next access for February 9, 2018 from Indonesia on desktop on all Wikipedia Projects") +  
  wmf::theme_min()


ggsave("return_frequency_Indonesia_Feb9.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

##seen on WP on 2018-02-10 and returning within 31 days.
return_frequency_id_Feb10 <- return_frequency_id_peak %>%
  dplyr::filter(date == 1518220800)

p <- ggplot(return_frequency_id_Feb10) + 
  geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = "Days until next access for February 10, 2018 from Indonesia on desktop all Wikipedia Projects") +  
  wmf::theme_min()


ggsave("return_frequency_Indonesia_Feb10.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

##seen on WP on 2018-02-11 and returning within 31 days.
return_frequency_id_Feb11 <- return_frequency_id_peak %>%
  dplyr::filter(date == 1518307200)

p <- ggplot(return_frequency_id_Feb11) + 
  geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = "Days until next access for February 11, 2018 from Indonesia on desktop on all Wikipedia Projects") +  
  wmf::theme_min()


ggsave("return_frequency_Indonesia_Feb11.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

#Indonesia Drop around 2018-04-21 on desktop
return_frequency_id_drop <- rbind(readr::read_rds("data/return_frequency_Indonesia_drop.rds"))

##seen on WP on 2018-04-19 and returning within 31 days.
return_frequency_id_Apr19 <- return_frequency_id_drop %>%
  dplyr::filter(date == 1524096000)

p <- ggplot(return_frequency_id_Apr19) + 
  geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = "Days until next access for April 19, 2018 from Indonesia on desktop all Wikipedia Projects") +  
  wmf::theme_min()


ggsave("return_frequency_Indonesia_Apr19.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

##seen on WP on 2018-04-20 and returning within 31 days.
return_frequency_id_Apr20 <- return_frequency_id_drop %>%
  dplyr::filter(date == 1524182400)

p <- ggplot(return_frequency_id_Apr20) + 
  geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = "Days until next access for April 20, 2018 from Indonesia on desktop all Wikipedia Projects") +  
  wmf::theme_min()


ggsave("return_frequency_Indonesia_Apr20.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

##seen on WP on 2018-04-21 and returning within 31 days.
return_frequency_id_Apr21 <- return_frequency_id_drop %>%
  dplyr::filter(date == 1524268800)

p <- ggplot(return_frequency_id_Apr21) + 
  geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = "Days until next access for April 21, 2018 from Indonesia on desktop on all Wikipedia Projects") +  
  wmf::theme_min()


ggsave("return_frequency_Indonesia_Apr21.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

##seen on WP on 2018-04-22 and returning within 31 days.
return_frequency_id_Apr22<- return_frequency_id_drop %>%
  dplyr::filter(date == 1524355200)

p <- ggplot(return_frequency_id_Apr22) + 
  geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = "Days until next access for April 22, 2018 from Indonesia on desktop all Wikipedia Projects") +  
  wmf::theme_min()


ggsave("return_frequency_Indonesia_Apr22.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

##seen on WP on 2018-04-23 and returning within 31 days.
return_frequency_id_Apr23 <- return_frequency_id_drop %>%
  dplyr::filter(date == 1524441600)

p <- ggplot(return_frequency_id_Apr23) + 
  geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = "Days until next access for April 23, 2018 from Indonesia on desktop on all Wikipedia Projects") +  
  wmf::theme_min()


ggsave("return_frequency_Indonesia_Apr23.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)



##Bangladesh Drop

#Bangladesh decrease on mobile web between 2018-01-28 and 2018-01-30
return_frequency_bd_drop <- rbind(readr::read_rds("data/return_frequency_Bangladesh_drop.rds"))

##seen on WP on 2018-01-28 and returning within 31 days.
return_frequency_bd_Jan28 <- return_frequency_bd_drop %>%
  dplyr::filter(date == 1517097600)

p <- ggplot(return_frequency_bd_Jan28) + 
  geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = "Days until next access for January 28, 2018 from Bangladesh on mobile web all Wikipedia Projects") +  
  wmf::theme_min()


ggsave("return_frequency_Bangladesh_Jan28.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

##seen on WP on 2018-01-29 and returning within 31 days.
return_frequency_bd_Jan29 <- return_frequency_bd_drop %>%
  dplyr::filter(date == 1517184000)

p <- ggplot(return_frequency_bd_Jan29) + 
  geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = "Days until next access for January 29, 2018 from Bangladesh on mobile web all Wikipedia Projects") +  
  wmf::theme_min()


ggsave("return_frequency_Bangladesh_Jan29.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

##seen on WP on 2018-01-30 and returning within 31 days.
return_frequency_bd_Jan30 <- return_frequency_bd_drop %>%
  dplyr::filter(date == 1517270400)

p <- ggplot(return_frequency_bd_Jan30) + 
  geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = "Days until next access for January 30, 2018 from Bangladesh on mobile web all Wikipedia Projects") +  
  wmf::theme_min()


ggsave("return_frequency_Bangladesh_Jan30.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


##Bangladesh Peak
#Bangladesh increase on mobile web between 2018-04-08 and 2018-04-10
return_frequency_bd_peak <- rbind(readr::read_rds("data/return_frequency_Bangladesh_peak.rds"))

##seen on WP on 2018-04-08 and returning within 31 days.
return_frequency_bd_Apr8 <- return_frequency_bd_peak %>%
  dplyr::filter(date == 1523145600)

p <- ggplot(return_frequency_bd_Apr8) + 
  geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = "Days until next access for April 8, 2018 from Bangladesh on mobile web on all Wikipedia Projects") +  
  wmf::theme_min()


ggsave("return_frequency_Bangladesh_Apr8.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

##seen on WP on 2018-04-09 and returning within 31 days.
return_frequency_bd_Apr9 <- return_frequency_bd_peak %>%
  dplyr::filter(date == 1523232000)

p <- ggplot(return_frequency_bd_Apr9) + 
  geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = "Days until next access for April 9, 2018 from Bangladesh on mobile web on all Wikipedia Projects") +  
  wmf::theme_min()


ggsave("return_frequency_Bangladesh_Apr9.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

##seen on WP on 2018-04-10 and returning within 31 days.
return_frequency_bd_Apr10 <- return_frequency_bd_peak %>%
  dplyr::filter(date == 1523318400)

p <- ggplot(return_frequency_bd_Apr10) + 
  geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = "Days until next access for April 10, 2018 from Bangladesh on mobile web on all Wikipedia Projects") +  
  wmf::theme_min()


ggsave("return_frequency_Bangladesh_Apr10.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)


# India drop around 2018-01-25 and 2018-01-28 on desktop
return_frequency_india_peak <- rbind(readr::read_rds("data/return_frequency_India_drop.rds"))

##seen on WP on 2018-01-25 and returning within 31 days.
return_frequency_india_Jan25 <- return_frequency_india_peak %>%
  dplyr::filter(date == 1516838400)

p <- ggplot(return_frequency_india_Jan25) + 
  geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = "Days until next access for January 25, 2018 from India on desktop on all Wikipedia Projects") +  
  wmf::theme_min()


ggsave("return_frequency_India_Jan25.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

##seen on WP on 2018-01-26 and returning within 31 days.
return_frequency_india_Jan26 <- return_frequency_india_peak %>%
  dplyr::filter(date == 1516924800)

p <- ggplot(return_frequency_india_Jan26) + 
  geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = "Days until next access for January 26, 2018 from India on desktop on all Wikipedia Projects") +  
  wmf::theme_min()


ggsave("return_frequency_India_Jan26.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

##seen on WP on 2018-01-27 and returning within 31 days.
return_frequency_india_Jan27 <- return_frequency_india_peak %>%
  dplyr::filter(date == 1517011200)

p <- ggplot(return_frequency_india_Jan27) + 
  geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = "Days until next access for January 27, 2018 from India on desktop on all Wikipedia Projects") +  
  wmf::theme_min()


ggsave("return_frequency_India_Jan27.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)

##seen on WP on 2018-01-28 and returning within 31 days.
return_frequency_india_Jan28 <- return_frequency_india_peak %>%
  dplyr::filter(date == 1517097600)

p <- ggplot(return_frequency_india_Jan28) + 
  geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = "Days until next access for January 28, 2018 from India on desktop on all Wikipedia Projects") +  
  wmf::theme_min()


ggsave("return_frequency_India_Jan28.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)
rm(p)




