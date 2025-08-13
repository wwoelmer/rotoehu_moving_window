# compare ERA5 data with buoy data

era5 <- read.csv('./data/processed_data/rotoehu_met_1980_2022.csv')
era5_long <- era5 %>% 
  pivot_longer(air_temp:windspeed, names_to = 'variable', values_to = 'value') %>% 
  select(-lake) %>% 
  mutate(method = 'ERA5')
era5_long$date <- as.Date(era5_long$date)

buoy <- read.csv('./data/raw_data/Rotoehu_202101-202508_meteorology.csv')

# take daily average of buoy data
buoy_d <- buoy %>% 
  mutate(date = as.Date(DateTime)) %>%
  group_by(date) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# rename the columns to match era5
buoy_names <- buoy_d %>% 
  rename(air_temp = TmpAir,
         rain = PpRain,
         shortwave = RadSWD,
         windspeed = WndSpd,
         longwave = RadClr)

buoy_long <- buoy_names %>% 
  select(date, air_temp, rain, longwave, shortwave, windspeed) %>% 
  mutate(method = 'buoy') %>% 
  pivot_longer(air_temp:windspeed, names_to = 'variable', values_to = 'value')


both <- full_join(buoy_long, era5_long)
both <- both %>% 
  filter(date > min(buoy_long$date) & date < max(buoy_long$date))

ggplot(both, aes(x = as.Date(date), y = value, color = method)) +
  geom_point() +
  facet_wrap(~variable, scales = 'free')

both_wide <- both %>% 
  filter(!variable %in% c('air_pressure', 'longwave')) %>% 
  pivot_wider(names_from = method,
              values_from = value)

ggplot(both_wide, aes(x = buoy, y = ERA5, color = variable)) +
  geom_point() +
  facet_wrap(~variable, scales = 'free') +
  theme_bw() 
