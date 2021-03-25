### Some preprocessing --------------------------------------------------------
library(tidyverse)
library(lubridate)

station_data <- readRDS("data/reduced_station_data.rds")

all_years = station_data %>%
  mutate(date = as.Date(date))  %>%
  mutate(year = year(date)) %>%
  select(year) %>%
  distinct() %>%
  mutate(dummy = 1)

all_years_id = station_data %>%
  select(id) %>%
  distinct() %>%
  mutate(dummy  = 1) %>%
  full_join(all_years)
saveRDS(all_years_id, "data/all_years_id.rds")

quality_controlled <- station_data  %>%
  mutate(date = as.Date(date))  %>%
  mutate(year = year(date)) %>%
  mutate(prcp = ifelse(!(qflag_prcp %in% c(" ", "O")),
                       NA, prcp)) 
saveRDS(quality_controlled, "data/quality_controlled.rds")

num_obs_per_year <- quality_controlled %>%
  filter(!is.na(prcp)) %>%
  group_by(id, year) %>%
  count(prcp >= 0) %>%
  ungroup()
saveRDS(num_obs_per_year, "data/num_obs_per_year.rds")
