### Fit models
library(extRemes)

max_file = "data/weekly_max_cleaned.rds" #"data/daily_max.rds"
data_type =  "weekly" # "daily" # 
station_meta <- readRDS("data/reduced_station_meta.rds")
output_filename_rl_rds = paste("data/", data_type, "_rl_cleaned.rds", sep = "")
output_filename_rl_csv = paste("data/", data_type, "_rl_cleaned.csv", sep = "")

max_df <- readRDS(max_file)
if(data_type == "weekly"){
  max_df <- max_df %>%
    rename(max_prcp = max_weekly_prcp)
}

ids <- max_df %>% pull(id) %>% unique()

# need quality filter 
# need min observations filter

all_return_df = NULL
par_df = NULL
for(id_num in 1:length(ids)){

print(id_num)
  
x  = max_df %>%
  filter(id == ids[id_num] & 
           max_prcp > 0) %>%
  mutate(max_prcp = max_prcp / 10)  %>%
  pull(max_prcp)

# stationary_model <- fevd(x,  type = "GEV")
stationary_model <- fevd(x,  type = "GEV", method = "Lmoments")

summary_stationary_model <- summary(stationary_model)

# par_values = summary_stationary_model$par
par_values = summary_stationary_model

rl_1in100 <- qevd(0.99, par_values[1],
                     par_values[2],
                     par_values[3])

# emp_return_levels <- ecdf(x)(x)
# 
# emp_return_df <-  data.frame(rl = x,
#                              rp = emp_return_levels) %>%
#   filter(rp  != 1) %>%
#   mutate(`Return Period` = 1/(1-rp),
#          `Return Level` = round(rl,2))

rp <- c(2,5,10,20,50,100,250,500,1000,2500)
rl <- return.level(stationary_model, rp, do.ci = TRUE)
return_df  <- data.frame(rp  = rp, 
                               rl_low =  rl[,1],
                               rl = rl[,2],
                               rl_upp = rl[,3]) %>% 
  mutate(id = ids[id_num])

all_return_df = rbind(all_return_df, return_df)

}

all_return_df <- all_return_df %>%
  left_join(station_meta)

saveRDS(all_return_df, output_filename_rl_rds)
write_csv(all_return_df %>% filter(rp < 500), output_filename_rl_csv)

## some basic plots

ggplot(data = all_return_df %>% 
         filter(rp < 100)) +
  geom_jitter(aes(x= long_round, y = lat_round, 
                 col =  rl), shape = 15) +
  facet_grid(~rp) +
  scale_color_distiller(name = "Return level (mm)", 
                        # palette = "Spectral", #) +
                        palette = "YlGnBu",
                        limits = c(0,500)) +
                        # limits = c(0,2000)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("Longitude")  +
  ylab("Latitude") #+ 
  # ggtitle("1 in 100 year Rainfall")

ggplot(data = all_return_df %>% 
         filter(rp < 100)) +
  geom_histogram(aes(x = rl)) +
  facet_grid(~rp)

# old code

# # all_daily_return_wide <- all_return_df %>%
# #   pivot_wider(names_from = rp, values_from = rl)
# 
# todays_value <- 100
# last_time_greater <- max_df %>%
#   filter(id == ids[id_num] & 
#            max_prcp > 0) %>%
#   mutate(max_prcp = max_prcp / 10) %>%
#   filter(max_prcp > todays_value) %>%
#   filter(year == max(year)) %>%
#   pull(year)
# 
# # %% percent of annual rainfall
# 
# 
# # distbn_df <-  data.frame(x=x, 
# #                          pdf=density_y,
# #                          cdf = cdf_y)
# # ggplot(distbn_df)  +
# #   geom_line(aes(x = x,  y = pdf))
# # 
# # ggplot(distbn_df)  +
# #   geom_line(aes(x = x,  y = cdf))
# 
# return_level_plot <- ggplot()  +
#   geom_ribbon(data=  return_df, aes(x = rp, ymin = rl_low, ymax= rl_upp),  alpha = 0.2)  +
#   geom_line(data = return_df, aes(x = rp, y = rl)) +
#   # geom_line(data = distbn_df, aes(x = 1/(1-cdf),  y = x), col = "red") +
#   geom_point(data = emp_return_df, aes(x = `Return Period`, 
#                                        y = `Return Level`)) +
#   scale_x_log10() +
#   theme_bw() + 
#   ylab("Return Rainfall Level (mm)") + 
#   xlab("Return Period") +
#   ggtitle("Return Period of Extreme Daily Rainfall Events")
# return_level_plot
# # saveRDS(return_level_plot, "return_plot.rds")
