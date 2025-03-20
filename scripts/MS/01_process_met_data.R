

library(tidyverse)

met <- read.csv('./data/raw_data/rotoehu_era5_1980_2022.csv')

# process met data
met <- met %>% 
  mutate(windspeed = sqrt(MET_wnduvu^2 + MET_wnduvv^2)) %>% 
  select(Date, everything(), -MET_tmpdew, -MET_wnduvu, -MET_wnduvv, -MET_ppsnow) %>% 
  rename(date = Date)
met$date <- as.Date(met$date)

colnames(met) <- c('date', 'air_temp', 'rain', 'air_pressure', 'shortwave', 'longwave', 'windspeed')
met$lake <- 'Rotoehu'

# calculate max, min, mean metrics
met_summ <- met %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(month, year) %>% 
  summarise_at(vars(c('air_temp', 'air_pressure', 'shortwave', 'windspeed')), 
               list(min = min, mean = mean,  max = max))  

rain_summ <- met %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(month, year) %>% 
  summarise(rain_sum = sum(rain))  

all_sum <- left_join(met_summ, rain_summ)

write.csv(met, paste0('./data/processed_data/rotoehu_met_', min(year(met$date)), "_", 
                      max(year(met$date)), '.csv'), row.names = FALSE)
write.csv(all_sum, paste0('./data/processed_data/rotoehu_met_summaries_',
                          min(year(met$date)), "_", 
                          max(year(met$date)), '.csv'), row.names = FALSE)
#

