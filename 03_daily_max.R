### Get Daily Maximums  
library(tidyverse)
library(lubridate)
# library(ggplot2)
# library(plotly)

output_file_annual_max_rds = "data/daily_max_cleaned.rds"
output_file_annual_max_csv = "data/daily_max_cleaned.csv"

min_number_max = 60
min_percent_per_year = 0.7
quality_controlled <- readRDS("data/quality_controlled.rds")
num_obs_per_year <- readRDS("data/num_obs_per_year.rds")
all_years_id <- readRDS("data/all_years_id.rds")

prcp_daily_max <- quality_controlled %>%
  group_by(id, year) %>%
  summarise(max_prcp = max(prcp, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(max_prcp = ifelse(max_prcp < 0, NA,  max_prcp)) %>%
  left_join(num_obs_per_year) %>%
  mutate(max_prcp = ifelse(n < 365*min_percent_per_year, 
                           NA,  max_prcp)) %>%
  full_join(all_years_id) %>%
  select(-dummy) 

valid_station_ids <- prcp_daily_max %>%
  filter(!is.na(max_prcp)) %>%
  count(id) %>%
  filter(n > min_number_max) %>%
  pull(id)

prcp_daily_max <- prcp_daily_max %>%
  filter(id %in% valid_station_ids)

saveRDS(prcp_daily_max, output_file_annual_max_rds)
write_csv(prcp_daily_max, output_file_annual_max_csv)

# max_daily_plot <- ggplot() + 
#   geom_line(data = prcp_daily_max,  
#             aes(x = year, 
#                 y = max_prcp/10, 
#                 group = id), col = "grey")  +
#   geom_line(data = prcp_daily_max %>%
#               filter(id == id[1]),  
#             aes(x = year, 
#                 y = max_prcp/10, 
#                 group = id), col = "red") +
#   theme_bw() +
#   theme(legend.position = "none") +
#   xlab("Year") +
#   ylab("Rainfall  (mm)") +
#   ggtitle("Annual Maximum Daily Rainfall")
# 
# max_daily_plot  

# ggplotly(max_daily_plot)

