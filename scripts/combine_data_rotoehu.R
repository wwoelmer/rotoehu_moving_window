# combine multiple data sources together
library(tidyverse)
library(plotly)
library(zoo)
library(readxl)
source('./scripts/functions/yr_to_hydro_yr.R')

##########################
# read in water quality data from Deniz
wq <- read.csv("./data/raw_data/rotoehu_waterquality_2000_2021.csv")
wq$date <- as.Date(wq$date)
wq$site <- as.character(wq$site)
wq <- wq %>% 
  mutate(year = year(date),
         month = month(date)) 
wq$category <- NA

##################################################################
# temp and thermal stratification metrics
temp <- read.csv('./data/processed_data/rotoehu_thermal_metrics_1990_2024.csv')
temp <- temp %>% 
  select(-X) %>% 
  mutate(month = month(date)) 
temp$date <- as.Date(temp$date)

df <- left_join(wq, temp)
###########################
# ctd data
ctd <- read.csv('./data/raw_data/BoP_ctd_2003_2022.csv')
ctd$date <- as.Date(ctd$date)
ctd <- ctd %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(lake=='Rotoehu',
         depth_m %in% c(1, 8)) %>% # keep surface and bottom data
  distinct(lake, year, month, depth_m, site, .keep_all = TRUE) 

ggplot(ctd, aes(x = date, y = DO_gm3, color = as.factor(depth_m))) +
  geom_line()

ggplot(ctd, aes(x = date, y = chla_ugL, color = as.factor(depth_m))) +
  geom_line()

ctd_wide <- ctd %>% 
  pivot_wider(names_from = depth_m, values_from = chla_ugL:temp_C)

ggplot(ctd_wide, aes(x = chla_ugL_1, y = chla_ugL_8)) +
  geom_point()


df <- left_join(df, ctd_wide, by = c('lake', 'year', 'month', 'site'))
df <- df %>% 
  rename(date = date.x) %>% 
  select(-c(category, date.y, time, DO_gm3_1, DO_gm3_8, chla_ugL_1, 
            PAR_umolm2s_8, turbidity_ntu_8)) %>% 
  select(lake, site, date, year, month, chla_ugL_INT, top_TN_ugL, top_TP_ugL,
         secchi_m, everything())

###############################################################################################
# interpolate missing data
df <- df[order(df$date),]

df <- df %>% 
  group_by(lake, site) %>% 
  mutate(chla_ugL_8 = na.approx(chla_ugL_8, na.rm = FALSE, rule = 2, maxgap = 15),
         DO_sat_1 = na.approx(DO_sat_1, na.rm = FALSE, rule = 2, maxgap = 15),
         DO_sat_8 = na.approx(DO_sat_8, na.rm = FALSE, rule = 2, maxgap = 15),
         PAR_umolm2s_1 = na.approx(PAR_umolm2s_1, na.rm = FALSE, rule = 2, maxgap = 15),
         turbidity_ntu_1 = na.approx(turbidity_ntu_1, na.rm = FALSE, rule = 2, maxgap = 15),
         temp_C_1 = na.approx(temp_C_1, na.rm = FALSE, rule = 2, maxgap = 15),
         temp_C_8 = na.approx(temp_C_8, na.rm = FALSE, rule = 2, maxgap = 15))

###########################################################################################
#####################################
### met data
met <- read.csv('./data/processed_data/Rotoehu_met_summaries_1999_2023.csv')
df <- left_join(df, met, by = c('month', 'year'))

##################################################################################
## lake level
lvl <- read_excel('./data/raw_data/EDS-686238-HL143688-Entire Record.xlsx', skip = 5)
lvl <- na.omit(lvl)
colnames(lvl) <- c('date', 'end', 'avg_level_m')
lvl$avg_level_m <- as.numeric(lvl$avg_level_m)
lvl <- lvl %>% select(-end)
lvl$date <- as.POSIXct(lvl$date)
lvl <- yr_to_hydro_yr(lvl)
lvl$month <- month(lvl$date)
#lvl <- lvl %>% select(-date)

lvl <- lvl %>% 
  group_by(hydroyear, month) %>% 
  mutate(monthly_avg_level_m = mean(avg_level_m, na.rm = TRUE))
df$date <- as.Date(df$date)
lvl$date <- as.Date(lvl$date)
lvl <- lvl %>% 
  ungroup() %>% 
  select(-month)

df <- left_join(df, lvl, by = 'date')

###################################################################################
## alum loading
al <- read.csv('./data/raw_data/alum_dosing_rotoehu_2011_2022.csv')
al$date <- as.Date(al$date)
al <- al %>% 
  select(date, L_alum_day) 

ggplot(al, aes(x = date, y  = L_alum_day)) +
  geom_point(size = 2)

# sum the amount of alum dosed since last sampling dates
dates <- unique(df$date)
al_sum <- data.frame()

for(i in 134:length(dates)){ # start at 134 before no alum was dosed before dates[134]
  sub <- al %>% 
    filter(date <= dates[i] & date > dates[i-1]) %>% 
    mutate(sum_alum = sum(L_alum_day), 
           sample_date = dates[i]) %>% 
    select(-L_alum_day)
  al_sum <- rbind(al_sum, sub)
  
}

al_sum <- al_sum %>% 
  distinct(sample_date, .keep_all = TRUE) %>% 
  select(-date) %>% 
  rename(date = sample_date)

ggplot(al_sum, aes(x = date, y  = sum_alum)) +
  geom_point(size = 2)


df <- left_join(df, al_sum, by = 'date')
# set NA's to zero as there was no dosing done on these dates
df <- df %>% 
  mutate(sum_alum = ifelse(date < min(al$date), 0, sum_alum))

ggplot(df, aes(x = date, y  = sum_alum)) +
  geom_point(size = 2)

###################################################################################
## land cover data

lc <- read.csv('./data/processed_data/landcover_1996_2018.csv')
lc$site <- as.character(lc$site)

lc_wide <- lc %>% 
  pivot_wider(names_from = lc_category, values_from = c(area_pct, area_ha)) %>% 
  select(-catchment)

df <- left_join(df, lc_wide, by = c('lake', 'site', 'year'))

####################################################################################
## write the dataset as master file
write.csv(df, './data/master_rotoehu.csv', row.names = FALSE)

