library(tidyverse)
library(readxl)
library(rLakeAnalyzer)

ysi <- read_excel('./data/raw_data/Lake_Rotoehu_temp_DO_1990.xlsx', 
                  sheet = 'Profile')
ysi <- ysi %>% 
  select(-StationName, -LakeName) %>% 
  rename(date = Date,
         depth_m = 'Depth (m)',
         temp_C = 'Temperature (degC)',
         DO_psat = 'DO%',
         DO_mgL = 'DO (mg/l)')
ysi$date <- as.Date(ysi$date)

ctd <- read.csv('./data/raw_data/ctd_2021_2024_rotoehu.csv') %>% 
  mutate(date = as.Date(date)) %>% 
  select(-LocationName) %>% 
  rename(DO_mgL = DO_mgl)

ctd2 <- read.csv('./data/raw_data/BoP_ctd_2003_2022.csv') %>% 
  filter(lake=='Rotoehu') %>% 
  rename(chl_ugL = chla_ugL,
         DO_mgL = DO_gm3,
         DO_psat = DO_sat,
         PAR = PAR_umolm2s,
         Conductivity = spcond_uScm,
         turb_NTU = turbidity_ntu) %>% 
  select(-lake, -site, -method, -time) %>% 
  mutate(date = as.Date(date),
         depth_m = as.integer(depth_m))

ctd3 <- full_join(ctd, ctd2)
ctd4 <- ctd3 %>% 
  distinct(date, depth_m, .keep_all = TRUE)

ysi2 <- full_join(ctd4, ysi)
ysi2 <- ysi2 %>% 
  arrange(date)

ggplot(ysi2, aes(x = as.Date(date), y = temp_C, color = as.factor(depth_m))) +
  geom_line() +
  geom_point()

ggplot(ysi2, aes(x = as.Date(date), y = DO_mgL, color = as.factor(depth_m))) +
  geom_line() +
  geom_point()

# get rid of a couple weird dates
ysi2 <- ysi2 %>% 
  filter(!date %in% c(as.Date('2020-06-17'), as.Date('2021-04-21')))


write.csv(ysi2, './data/processed_data/rotoehu_ctd_1990_2024.csv', row.names = FALSE)

# extract surface and bottom temps for analysis
temps <- ysi2 %>% 
  filter(depth_m==0|depth_m==8) %>% 
  select(date, depth_m, temp_C) %>% 
  distinct(date, depth_m, .keep_all = TRUE) %>% 
  pivot_wider(names_from = depth_m, values_from = temp_C, 
              names_prefix = 'temp_')

#################################################################
# calculate thermocline depth and schmidt stability

# read in bathy estimates
bty <- read_excel('./data/raw_data/Rotlakes_bathymetry.xls', skip = 1) 
colnames(bty) <- c('lake', 'depth_m', 'vol_to_bottom_m3', 'vol_at_countour_m3', 
                   'planar_sa_m2', 'model_sd_m2')

bty$depth_m <- abs(bty$depth_m)
bty <- bty %>% 
  filter(lake=='Rotoehu')


# area and depth of bathymetry
bthA <- bty$model_sd_m2
bthD <- bty$depth_m

bty_interp <- data.frame(lake = bty$lake[1],
                         depth_m = seq(1, max(bty$depth_m), 1),
                         vol_at_contour_m3  = approx(bty$depth_m, bty$vol_at_countour_m3 , seq(0, max(bty$depth_m) - 1, 1))$y)
######################################################################################################
# calculate volumetrically weighted DO


hypo_o2 <- ysi2 %>% 
  select(date, depth_m, temp_C, DO_mgL) %>% 
  distinct(date, depth_m, .keep_all = TRUE) %>% 
  group_by(date) %>% 
  mutate(hypo_depth = meta.depths(temp_C, depth_m, seasonal = FALSE, mixed.cutoff = 0)[2]) %>% 
  group_by(date) %>% 
  filter(depth_m > hypo_depth) %>% 
  left_join(bty_interp, by = 'depth_m') %>% 
  mutate(DO_vol = DO_mgL*vol_at_contour_m3)

hypo_VW <- hypo_o2 %>% 
  group_by(date) %>% 
  summarise(DO_sum = sum(DO_vol))


ggplot(hypo_VW, aes(x = as.Date(date), y = DO_sum)) +
  geom_point() +
  geom_line() +
  geom_smooth()

##################################################################################

t_metrics <- ysi2 %>% 
  select(date, depth_m, temp_C, DO_mgL) %>% 
  distinct(date, depth_m, .keep_all = TRUE) %>% 
  group_by(date) %>% 
  mutate(thermo_depth = thermo.depth(temp_C, depth_m, seasonal = FALSE),
         thermo_depth = ifelse(is.na(thermo_depth), 0, thermo_depth),
         schmidt_stability = schmidt.stability(temp_C, 
                                               depth_m, 
                                               bthA = bty$model_sd_m2, 
                                               bthD = bty$depth_m),
         strat = ifelse(thermo_depth > 0, 1, 0)) %>% 
  select(date, everything()) %>% 
  distinct(date, .keep_all = TRUE)

ggplot(t_metrics, aes(x = as.Date(date), y = thermo_depth, 
                      color = as.factor(year(date)))) +
  geom_point() +
  geom_line()

ggplot(t_metrics, aes(x = as.Date(date), y = schmidt_stability, 
                      color = as.factor(year(date)))) +
  geom_point() +
  geom_line()

#######################################################################
# combine and write file
t_dat <- full_join(t_metrics, temps) 
t_dat <- t_dat %>% 
  select(date, thermo_depth, schmidt_stability, temp_0, temp_8)
write.csv(t_dat, './data/processed_data/rotoehu_thermal_metrics_1990_2024.csv')
