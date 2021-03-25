library(rnoaa)
library(tidyverse)
library(oz)

aus_code  = "ASN"
min_years = 50
var = c("PRCP", "DAPR", "DWPR", "MDPR")
delta = 50
east_coast_longitude = 150

all_stations <- ghcnd_stations()

aus_stations <- all_stations %>%
  filter(element == "PRCP") %>%
  filter(str_detect(id, aus_code)) %>%
  filter(longitude > 0) # removes ocean

long_stations <- aus_stations %>%
  filter(longitude > east_coast_longitude) %>% #east coast only
  filter(last_year - first_year + 1 > min_years) %>% #long stations
  mutate(num_years = last_year - first_year + 1) %>%
  arrange(desc(num_years)) 

# stations per each cell
reduced_stations <- long_stations %>%
  mutate(long_round = round(longitude/0.5)*0.5, 
         lat_round = round(latitude/0.5)*0.5) %>%
  group_by(long_round,lat_round) %>%
  slice_head(n = 3) %>%
  ungroup()

ggplot()  +  
  geom_point(data  = aus_stations, 
             aes(x= longitude,  y = latitude),
             shape = 21, size = 0.2) + 
  geom_point(data  = long_stations, 
             aes(x= longitude,  y = latitude),
             shape = 21, size = 0.2) + 
  geom_point(data =  reduced_stations, 
             aes(x= longitude,  y = latitude),
             shape = 21, fill = "red") + 
  coord_fixed()

# ~12 minutes for 435 stations
monitors <- reduced_stations$id
time1 <- Sys.time()
reduced_station_data <- meteo_pull_monitors(monitors = monitors,
                                    keep_flags =  TRUE,
                                    var = "all")
time2 <- Sys.time()

saveRDS(reduced_stations, "data/reduced_station_meta.rds")
saveRDS(reduced_station_data, "data/reduced_station_data.rds")


