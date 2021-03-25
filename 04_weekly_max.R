### Get Weekly Maximums  
library(tidyverse)
library(lubridate)
# library(ggplot2)
# library(plotly)
library(zoo)

sum_len = 7
min_number_max = 60
min_percent_per_year = 0.7
quality_controlled <- readRDS("data/quality_controlled.rds")
num_obs_per_year <- readRDS("data/num_obs_per_year.rds")
all_years_id <- readRDS("data/all_years_id.rds")

wider_format <- quality_controlled  %>%
  mutate(date = as.Date(date))  %>%
  mutate(year = year(date)) %>%
  select(id, date, year, prcp) %>%
  distinct() %>%
  pivot_wider(names_from = id, values_from = prcp) 

numeric_wider_format <- wider_format %>%
  select(-date, -year)

prcp_weekly_numeric <- apply(numeric_wider_format, 2, 
                             rollsum, 
                             k = sum_len,
                             na.pad = TRUE)

prcp_weekly <- data.frame(date = wider_format$date, 
                          year = wider_format$year,
                          prcp_weekly_numeric) %>%
  pivot_longer(c(-date,-year),
               names_to = "id", values_to = "weekly_prcp") 

prcp_weekly_max <- prcp_weekly %>%
  group_by(id, year) %>%
  summarise(max_weekly_prcp = max(weekly_prcp, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(max_weekly_prcp = 
           ifelse(max_weekly_prcp > 0, max_weekly_prcp, NA)) %>%
  left_join(num_obs_per_year) %>%
  mutate(max_weekly_prcp = ifelse(n < 365*min_percent_per_year, 
                           NA,  max_weekly_prcp)) %>%
  full_join(all_years_id) %>%
  select(-dummy) 

saveRDS(prcp_weekly_max, "data/weekly_max_cleaned.rds")
write_csv(prcp_weekly_max, "data/weekly_max_cleaned.csv")

warning("Would be better to add in aggregates as well")
# 
# max_weekly_plot <- ggplot() + 
#   geom_line(data = prcp_weekly_max,  
#             aes(x = year, 
#                 y = max_weekly_prcp/10, 
#                 group = id), col = "grey")  +
#   geom_line(data = prcp_weekly_max %>%
#               filter(id == id[1]),  
#             aes(x = year, 
#                 y = max_weekly_prcp/10, 
#                 group = id), col = "red") +
#   theme_bw() +
#   theme(legend.position = "none") +
#   xlab("Year") +
#   ylab("Rainfall  (mm)") +
#   ggtitle("Annual Maximum Daily Rainfall")
# 
# max_weekly_plot  
