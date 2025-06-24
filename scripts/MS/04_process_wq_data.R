# combine all water quality data from 1990 to 2024
library(tidyverse)
library(plotly)
library(zoo)
library(readxl)
source('./scripts/functions/yr_to_hydro_yr.R')

##########################
# read in water quality data from 1990 to 2021
wq1 <- read.csv("./data/raw_data/rotoehu_waterquality_2000_2021.csv")
wq1$date <- as.Date(wq1$date)
wq1$site <- as.character(wq1$site)

################################
# add in newest wq data
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
path <- './data/raw_data/Lakes Rotoehu WQ sample and CTD profile data from 2021-01-01 to 2024-08-01.xlsx'
dat <- multiplesheets(path)

# create dataframes from list
list2env(dat, envir = .GlobalEnv)
wq2 <- `WQ Sample Data`

wq2 <- wq2 %>% 
  select(LocationName, Time, `CHLA (mg/m^3)`, `DRP (g/m^3)`, 
         `NH4-N (g/m^3)`:`NO3-N (g/m^3)`, `TN (g/m^3)`, `TP (g/m^3)`, 
         "VC - SD (m)", "Turbidity NTU (_NTU)") %>% 
  rename(chl_mgm3 = `CHLA (mg/m^3)`,
         DRP_gm3 = "DRP (g/m^3)",
         NH4_gm3 = "NH4-N (g/m^3)",
         NNN_gm3 = "NNN (g/m^3)",
         NO2_gm3 = "NO2-N (g/m^3)",
         NO3_gm3 = "NO3-N (g/m^3)",
         TN_gm3 = "TN (g/m^3)",
         TP_gm3 = "TP (g/m^3)",
         secchi_m = "VC - SD (m)",
         turbidity_NTU = "Turbidity NTU (_NTU)") 

# add depth column
wq2 <- wq2 %>% 
  mutate(depth = ifelse(LocationName=="Lake Rotoehu at Site 3 (Integrated)", 'integrated', 'bottom')) %>% 
  select(-LocationName)

# convert units
wq2 <- wq2 %>% 
  mutate(TN_ugL = TN_gm3*1000,
         TP_ugL = TP_gm3*1000,
         DRP_ugL = DRP_gm3*1000,
         NH4_ugL = NH4_gm3*1000,
         NO3_ugL = NNN_gm3*1000) %>% 
  select(Time, chl_mgm3, secchi_m, depth, TN_ugL:NO3_ugL, turbidity_NTU) %>% 
  rename(chla_ugL = chl_mgm3) %>% 
  mutate(date = as.Date(Time)) %>% 
  select(-Time)

# paste depth into the column names
wq2_wide <- wq2 %>% 
  mutate(depth_cat = ifelse(depth=='integrated', 'top', 'bottom')) %>% 
  pivot_longer(c(chla_ugL, secchi_m, TN_ugL:turbidity_NTU), names_to = 'variable', values_to = 'value') %>% 
  mutate(variable = paste0(depth_cat, "_", variable)) %>% 
  select(date, variable, value) %>% 
  filter(!is.na(value))

wq2 <- wq2_wide %>% 
  group_by(variable, date) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  pivot_wider(names_from = 'variable', values_from = 'value') %>% 
  rename(chla_ugL_INT = top_chla_ugL,
         secchi_m = top_secchi_m) %>% 
    mutate(lake = 'Rotoehu',
           site = '3')


wq <- full_join(wq1, wq2)
wq <- wq %>% 
  mutate(year = year(date),
         month = month(date)) 


# add in data from 1990's
wq90 <- read_excel('./data/raw_data/Rotoehu_1990_1999_PaulScholes.xlsx')

# categorize depths
wq90 <- wq90 %>% 
  mutate(depthcat = ifelse(DepthFrom > 3, 'bottom', 'top'),
         depthcat = ifelse(Unit=='SecchiDepth (m)', '', depthcat),
         depthcat = ifelse(Unit=='Chla (mg/m3)', 'INT', depthcat))

# for days when more than one TN or TP sample was taken above 3 m, take the average
wq90 <- wq90 %>% 
  select(-DepthFrom) %>% 
  group_by(Date, Unit, depthcat) %>% 
  summarise(Results = mean(Results), n = n()) %>% 
  mutate(Results = round(Results, digits = 2))

# make wide and rename variables
wq90 <- wq90 %>% 
  ungroup() %>% 
  select(-n) %>% 
  mutate(variable_depth = paste0(Unit, "_", depthcat)) %>% 
  select(-Unit, -depthcat) %>% 
  pivot_wider(names_from = variable_depth, values_from = Results) %>% 
  rename(chla_ugL_INT = `Chla (mg/m3)_INT`,
         bottom_DRP_ugL = "DRP (mg/m3)_bottom",
         top_DRP_ugL = "DRP (mg/m3)_top",
         bottom_NH4_ugL = "NH4-N (mg/m3)_bottom",
         top_NH4_ugL = "NH4-N (mg/m3)_top",
         bottom_NO3_ugL = "NNN (mg/m3)_bottom",
         top_NO3_ugL = "NNN (mg/m3)_top",
         secchi_m = `SecchiDepth (m)_`,
         bottom_TN_ugL = "TN (mg/m3)_bottom",
         top_TN_ugL = "TN (mg/m3)_top",
         bottom_TP_ugL = "TP (mg/m3)_bottom",
         top_TP_ugL = "TP (mg/m3)_top",
         bottom_pH = pH_bottom,
         top_pH = pH_top,
         date = Date)# edit to fit same format as other wq data


wq90 <- wq90 %>% 
  mutate(lake = 'Rotoehu',
         site = '3') 

## and some data from 1999 and 2000 to connect it all
wq00 <- read_excel('./data/raw_data/Rotoehu for Whitney.xlsx')
wq00 <- wq00 %>% 
  filter(Lake=='Rotoehu',
         `H year end` > 1999 & `H year end` < 2001) %>% 
  select(Date, `Chl-a (mg/m3)`:`TP (mg/m3)`) %>% 
  rename(date = Date,
         chla_ugL_INT = `Chl-a (mg/m3)`,
         secchi_m = `Clarity (m)`,
         top_TN_ugL = `TN (mg/m3)`,
         top_TP_ugL = `TP (mg/m3)`)

wq <- full_join(wq, wq90) 
wq <- full_join(wq, wq00)
wq <- wq %>% 
  arrange(date)

min(wq$date)
max(wq$date)

write.csv(wq, './data/processed_data/rotoehu_wqdata_1990_2024.csv', row.names = FALSE)
