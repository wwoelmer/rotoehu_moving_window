# use RLakeAnalyzer to calculate mixing metrics for each lake
# look at monthly long-term dataset and then compare to time periods were buoy data is available
# mxing metrics can also be used in the driver analysis

#install.packages('rLakeAnalyzer')
#install.packages('patchwork')
library(rLakeAnalyzer)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(plotly)
library(readxl)
library(patchwork)

ctd <- read.csv('./data/processed_data/rotoehu_ctd_1990_2024.csv')
ctd$date <- as.Date(ctd$date)

# remove any dups
ctd <- ctd %>% 
  distinct(date, depth_m, .keep_all = TRUE) %>% 
  filter(!is.na(temp_C))


# read in bathy estimates
bty <- read_excel('./data/raw_data/Rotlakes_bathymetry.xls', skip = 1)
colnames(bty) <- c('lake', 'depth_m', 'vol_to_bottom_m3', 'vol_at_countour_m3', 'planar_sa_m2', 
                   'model_sd_m2')

bty$depth_m <- abs(bty$depth_m)

# area and depth of bathymetry
bthA <- bty$model_sd_m2
bthD <- bty$depth_m

# and wind
met <- read.csv('./data/processed_data/Rotoehu_met_1999_2023.csv')
met$date <- as.Date(met$date)

# merge wind into ctd dataframe
ctd <- left_join(ctd, met, by = 'date')

# calculate a bunch of mixing metrics
t_metrics <- ctd %>% 
  select(lake, date, depth_m, temp_C, windspeed) %>% 
  group_by(lake, date) %>% 
  mutate(thermo_depth = thermo.depth(temp_C, depth_m, seasonal = TRUE),
         thermo_depth = ifelse(is.na(thermo_depth), 0, thermo_depth),
         schmidt_stability = schmidt.stability(temp_C, depth_m, bthA = bty$model_sd_m2, bthD = bty$depth_m),
         epi_temp = epi.temperature(wtr = temp_C, depths = depth_m, bthA = bthA, bthD = bthD),
         epi_dens = water.density(epi_temp),
         hypo_temp = hypo.temperature(wtr = temp_C, depths = depth_m, bthA = bthA, bthD = bthD),
         hypo_dens = water.density(hypo_temp),
         meta_top = meta.depths(temp_C, depth_m)[1],
         meta_bot = meta.depths(temp_C, depth_m)[2],
         uStar = uStar(wndSpeed = windspeed, wndHeight = 10, averageEpiDense = epi_dens),
         lake_num = lake.number(bthA = bthA, bthD = bthD, uStar = uStar, St = schmidt_stability,
                                metaT = meta_top, metaB = meta_bot, averageHypoDense = hypo_dens),
         strat = ifelse(thermo_depth > 0, 1, 0)) %>% 
  select(lake, date, everything()) %>% 
  distinct(lake, date, .keep_all = TRUE)


colnames(t_metrics)

write.csv(t_metrics, './data/processed_data/BoP_mix_index_1990_2024.csv', row.names = FALSE)

# only one day of data for Rotoma? remove
t_metrics <- t_metrics %>% 
  filter(lake!='Rotoma')

# add in mixing status
mix <-  read.csv('./data/processed_data/BoP_wq_2007_2021.csv')
mix <- mix %>% 
  select(lake, laketype)

t_metrics <- left_join(t_metrics, mix, by = 'lake')



ggplot(t_metrics, aes(x = log(schmidt_stability), y = log(lake_num), color = lake)) +
  geom_point() +
  geom_smooth(aes(group = laketype))

ggplot(t_metrics, aes(x = schmidt_stability, y = thermo_depth, color = lake)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~laketype, scales = 'free')

ggplot(t_metrics, aes(x = date, y = lake_num, color = lake)) +
  geom_point() +
  facet_wrap(~lake, scales = 'free')

ggplot(t_metrics, aes(x = lake_num, color = lake, fill = lake)) +
  geom_histogram(position = 'identity') +
  facet_wrap(~laketype, scales = 'free')

ggplot(t_metrics, aes(x = date, y = schmidt_stability, color = as.factor(year(date)))) +
  facet_wrap(~lake, scales = 'free') +
  geom_line()

ggplot(t_metrics, aes(x = date, y = meta_top, color = as.factor(year(date)))) +
  facet_wrap(~lake, scales = 'free') +
  geom_line() +
  geom_line(aes(y = meta_bot))

ggplotly(t_metrics %>% 
           filter(lake=='Rotoehu') %>% 
           ggplot(aes(x = date, y = thermo_depth, color = as.factor(year(date)))) +
           geom_line()
)

ggplotly(t_metrics %>% 
           #filter(lake %in% c('Rotoehu', 'Okaro')) %>% 
           ggplot(aes(x = date, y = thermo_depth, color = paste0(lake, site))) +
           geom_line() +
           geom_point()
)

ggplotly(t_metrics %>% 
           filter(lake %in% c('Rotoehu')) %>% 
           ggplot(aes(x = date, y = thermo_depth)) +
           geom_point(aes(color = mixed < 1)) +
           geom_line(aes(color = as.factor(year(date))))
)

ggplotly(t_metrics %>% 
           filter(lake=='Rotoehu') %>% 
           ggplot(aes(x = date, y = as.factor(mixed), group = lake)) +
           geom_line()
)

######################################################################################
# calculate number of times mixing each year



ggplot(t_metrics, aes(x = as.Date(date), y = strat)) +
  geom_line() +
  facet_wrap(~paste0(lake, site))

mix <- t_metrics %>%
  group_by(lake, year(date), site) %>%
  mutate(num_mix = sum(head(strat, -1)==1 & tail(strat, -1) == 0)) %>% 
  distinct(lake, year(date), .keep_all = TRUE)

### this ^^ is just calculating when it goes from stratified to fully mixed, not capturing
## when the thermocline shallows

ggplotly(
  ggplot(mix,
    aes(x = date, y = test_num_mix, color = paste0(lake, site))) +
    geom_line() +
    geom_point())

ggplot(mix, aes(x = as.factor(test_num_mix), fill = paste0(lake, site))) +
  geom_histogram(stat = 'count', position = 'dodge') +
  facet_wrap(~lake, scales = 'free_x')

mix %>% 
  filter(lake=='Rotoehu') %>% 
  ggplot(aes(x = as.Date(date), y = num_mix)) +
  geom_point() +
  geom_line() +
  geom_line(data = dat, aes(as.Date(date), y = tli_annual, color = 'tli'))



ggplot(t_metrics, aes(x = thermo_depth,  fill = paste0(lake, site))) +
  geom_histogram( position = 'dodge') +
  facet_wrap(~lake, scales = 'free_x')

