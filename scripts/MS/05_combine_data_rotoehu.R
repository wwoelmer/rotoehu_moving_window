library(tidyverse)
library(plotly)
library(zoo)
library(readxl)
source('./scripts/functions/yr_to_hydro_yr.R')

##################################################################################
# read in water quality data from 1990 to 2021
wq <- read.csv("./data/processed_data/rotoehu_wqdata_1990_2024.csv")
wq$date <- as.POSIXct(wq$date)
wq <- yr_to_hydro_yr(wq)
#wq$date <- as.Date(wq$date)
wq$site <- as.character(wq$site)
wq <- wq %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  select(-X, -top_turbidity_NTU, -bottom_turbidity_NTU,
         -(TN_TP:DP_TP)) ## use turbidity data from the CTD instead


##################################################################################
# temp and thermal stratification metrics
temp <- read.csv('./data/processed_data/rotoehu_thermal_metrics_1990_2024.csv') 
temp$date <- as.Date(temp$date)
temp <- temp %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  select(-date) %>% 
  rename(temp_C_1 = temp_0,
         temp_C_8 = temp_8) %>% 
  distinct(year, month, .keep_all = TRUE)

df <- left_join(wq, temp)

##################################################################################
# ctd data
ctd <- read.csv('./data/processed_data/rotoehu_ctd_1990_2024.csv')
ctd$date <- as.Date(ctd$date)
ctd <- ctd %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(depth_m %in% c(1, 8)) %>% 
  distinct(year, month, depth_m, .keep_all = TRUE) 

ctd_wide <- ctd %>% 
  rename(chla_ugL = chl_ugL,
         DO_sat = DO_psat,
         PAR_umolm2s = PAR,
         turbidity_ntu = turb_NTU) %>% 
  pivot_wider(names_from = depth_m, values_from = chla_ugL:temp_C) %>% 
  select(-temp_C_8, - temp_C_1, -date)

df <- left_join(df, ctd_wide)

###############################################################################################
# interpolate missing data
df <- df[order(df$date),]

df <- df %>% 
  group_by(lake, site) %>% 
  mutate(chla_ugL_8 = na.approx(chla_ugL_8, na.rm = FALSE, rule = 2, maxgap = 15),
         chla_ugL_1 = na.approx(chla_ugL_1, na.rm = FALSE, rule = 2, maxgap = 15),
         DO_sat_1 = na.approx(DO_sat_1, na.rm = FALSE, rule = 2, maxgap = 15),
         DO_sat_8 = na.approx(DO_sat_8, na.rm = FALSE, rule = 2, maxgap = 15),
         DO_mgL_1 = na.approx(DO_mgL_1, na.rm = FALSE, rule = 2, maxgap = 15),
         DO_mgL_8 = na.approx(DO_mgL_8, na.rm = FALSE, rule = 2, maxgap = 15),
         PAR_umolm2s_1 = na.approx(PAR_umolm2s_1, na.rm = FALSE, rule = 2, maxgap = 15),
         PAR_umolm2s_8 = na.approx(PAR_umolm2s_8, na.rm = FALSE, rule = 2, maxgap = 15),
         turbidity_ntu_1 = na.approx(turbidity_ntu_1, na.rm = FALSE, rule = 2, maxgap = 15),
         turbidity_ntu_8 = na.approx(turbidity_ntu_8, na.rm = FALSE, rule = 2, maxgap = 15),
         Conductivity_1 = na.approx(Conductivity_1, na.rm = FALSE, rule = 2, maxgap = 15),
         Conductivity_8 = na.approx(Conductivity_8, na.rm = FALSE, rule = 2, maxgap = 15),
         temp_C_1 = na.approx(temp_C_1, na.rm = FALSE, rule = 2, maxgap = 15),
         temp_C_8 = na.approx(temp_C_8, na.rm = FALSE, rule = 2, maxgap = 15),
         thermo_depth = na.approx(thermo_depth, na.rm = FALSE, rule = 2, maxgap = 15),
         schmidt_stability = na.approx(schmidt_stability, na.rm = FALSE, rule = 2, maxgap = 15))

##################################################################################
### met data
met <- read.csv('./data/processed_data/rotoehu_met_summaries_1980_2022.csv')
df <- left_join(df, met)

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

lvl <- lvl %>% 
  group_by(hydroyear, month) %>% 
  mutate(monthly_avg_level_m = mean(avg_level_m, na.rm = TRUE))

df$date <- as.Date(df$date) + 1
lvl$date <- as.Date(lvl$date)

df <- left_join(df, lvl)

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

df <- left_join(df, al_sum)

# set NA's to zero as there was no dosing done on these dates
df <- df %>% 
  mutate(sum_alum = ifelse(date < min(al$date), 0, sum_alum))

ggplot(df, aes(x = date, y  = sum_alum)) +
  geom_point(size = 2)

###################################################################################
## land cover data

lc <- read.csv('./data/processed_data/rotoehu_landcover_1996_2018.csv')
lc$site <- as.character(lc$site)

lc_wide <- lc %>% 
  pivot_wider(names_from = lc_category, values_from = c(area_pct, area_ha)) %>% 
  select(-catchment)

df <- left_join(df, lc_wide)

################################################################################
# climate indices
soi <- read.csv('./data/raw_data/soi-rolling-average-1990-2022-and-temperature-anomaly-1990-2020.csv')
soi <- soi %>% 
  mutate(month = ifelse(month < 10, paste0("0", month), month))
soi$date <- paste0(soi$year, "-", soi$month, "-01")
soi$date <- ymd(soi$date)

soi$date <- as.POSIXct(soi$date)
soi <- yr_to_hydro_yr(soi)
soi$month <- as.integer(soi$month)

ggplot(soi, aes(x = as.Date(date), y = soi_3mth_mean, color = as.factor(year))) +
  geom_point() +
  geom_line()

ggplot(soi, aes(x = as.Date(date), y = de_trended_temp_anomaly)) +
  geom_point() +
  geom_line()

soi <- soi %>% select(-date)

df <- left_join(df, soi)

####################################################################################
## write the dataset as master file

# do some organizing of columns
df <- df %>% 
  select(lake, site, date, year, month, hydroyear,  
         chla_ugL_INT, top_TN_ugL, top_TP_ugL, secchi_m, everything(),
         -hydroyear_label)

write.csv(df, './data/master_rotoehu.csv', row.names = FALSE)

