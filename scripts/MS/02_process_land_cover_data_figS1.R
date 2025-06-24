# process lawa land cover data

library(readxl)
library(tidyverse)
library(plotly)

multiplesheets <- function(fname) {
  
  # getting info about all excel sheets
  sheets <- readxl::excel_sheets(fname)
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x))
  data_frame <- lapply(tibble, as.data.frame)
  
  # assigning names to data frames
  names(data_frame) <- sheets
  
  # print data frame
  print(data_frame)
}

# specifying the path name
path <- './data/raw_data/lawa-land-cover-data_june-2021.xlsx'
dat <- multiplesheets(path)

# create dataframes from list
list2env(dat, envir = .GlobalEnv)

## work with the catchment detailed data
lc <- `Catchment Detailed`
colnames(lc) <- c('region', 'catchment', 'catchment_id', 'lc_category', 
                  'area_1996_ha','area_2001_ha', 'area_2008_ha', 'area_2012_ha', 'area_2018_ha',
                  'area_1996_pct','area_2001_pct', 'area_2008_pct', 'area_2012_pct', 'area_2018_pct',
                  'area_change_1996_2018_ha', 'area_change_1996_2018_pct')


# rename ugly catchment cover categories
lc$lc_category <- recode(lc$lc_category,
                         "Broadleaved Indigenous Hardwoods" = 'native_hardwood',
                         "Built-up Area (settlement)" = 'settlement',
                         "Deciduous Hardwoods" = 'decid_hardwood',
                         "Exotic Forest" = 'exotic_forest',
                         "Forest - Harvested" = 'forest_harvested',
                         "Gorse and/or Broom" = 'gorse',
                         "Herbaceous Freshwater Vegetation" = 'aq_vegetation',
                         "High Producing Exotic Grassland" = 'hp_exotic_grassland',
                         "Indigenous Forest" = 'native_forest',
                         "Lake or Pond" = 'water', 
                         "Manuka and/or Kanuka" =  'manuka')

# filter down to rotoehu
lc <- lc %>% 
  filter(region=='Bay of Plenty Region',
         catchment=='Lake Rotoehu')

lc_long_ha <- lc %>% 
  select(region:area_2018_ha) %>% 
  pivot_longer(cols = area_1996_ha:area_2018_ha, names_to = 'year', values_to = 'area_ha') %>% 
  mutate(year = gsub('[area_]', '', year)) %>% 
  mutate(year = gsub('[h]', '', year))

# complete time series
lc_fill <- lc_long_ha %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(lc_category, region, catchment, catchment_id) %>% 
  complete(year = 1996:2018) %>% 
  fill(area_ha)

lc_fill %>% 
  filter(lc_category!='Catchment Area') %>% 
ggplot(aes(x = year, y = area_ha, color = lc_category)) +
  geom_line()

catchment_area <- unique(lc_fill$area_ha[lc_fill$lc_category=="Catchment Area"])

lc_fill <- lc_fill %>% 
  filter(lc_category!="Catchment Area") %>% 
  group_by(year, lc_category) %>% 
  mutate(area_pct = area_ha/catchment_area*100)

ggplotly(ggplot(lc_fill, aes(x = year, y = area_pct, fill = lc_category)) +
  geom_area())

p1 <- ggplot(lc_fill, aes(x = year, y = area_pct, fill = lc_category)) +
  geom_area() +
  labs(fill = 'Land Use Category') +
  xlab('Date') +
  ylab('Percent of Catchment') +
  theme_bw()

ggsave('./figures/MS/si_figs/landcover_pct.png', p1,
       dpi = 300, units = 'mm', height = 300, width = 450, scale = 0.5)

lc_fill$lake <- 'Rotoehu'
lc_fill$site <- '3'
lc_fill <- lc_fill %>% 
  select(lake, site, year, lc_category, catchment, area_ha, area_pct)

min(lc_fill$year)
max(lc_fill$year)

write.csv(lc_fill, './data/processed_data/rotoehu_landcover_1996_2018.csv', row.names = FALSE)
