library(ggplot2)
library(readxl)
library(tidyverse)
library(ggpubr)


##########################################################################
# data from hydro year 1990 to 1998 from Paul Scholes
dat <- read_excel('./data/raw_data/Rotoehu_1990_1999_PaulScholes.xlsx')

dat %>% 
  filter(DepthFrom > 6.75) %>% 
ggplot(aes(x = as.Date(Date), y = Results, color = as.factor(DepthFrom))) +
  geom_point() +
  facet_wrap(~Unit, scales = 'free')

# select surface depth for TN, TP
dat <- dat %>% 
  mutate(depth_adj = ifelse(Unit=='SecchiDepth (m)', 1, DepthFrom)) %>% 
  filter(depth_adj <=3)

ggplot(dat, aes(x = as.Date(Date), y = Results, color = as.factor(depth_adj))) +
  geom_point() +
  facet_wrap(~Unit, scales = 'free')

# for days when more than one TN or TP sample was taken above 3 m, take the average
dat_clean <- dat %>% 
  group_by(Date, Unit) %>% 
  summarise(Results = mean(Results), n = n()) %>% 
  mutate(Results = round(Results, digits = 2))

ggplot(dat_clean, aes(x = as.Date(Date), y = Results, color = as.factor(n))) +
  geom_point() +
  facet_wrap(~Unit, scales = 'free')


# make wide and rename variables
dat_wide <- dat_clean %>% 
  select(-n) %>% 
  pivot_wider(names_from = Unit, values_from = Results) %>% 
  rename(chl_mgm3 = `Chla (mg/m3)`,
         DRP_mgm3 = `DRP (mg/m3)`,
         NH4_mgm3 = `NH4-N (mg/m3)`,
         NNN_mgm3 = `NNN (mg/m3)`,
         secchi_m = `SecchiDepth (m)`,
         TN_mgm3 = `TN (mg/m3)`,
         TP_mgm3 = `TP (mg/m3)`,
         date = Date)

# get rid of non-TLI variables
dat_wide <- dat_wide %>% 
  select(-pH, DRP_mgm3, NH4_mgm3, NNN_mgm3)

dat_wide <- na.omit(dat_wide)

### convert to hydroyear
yr_to_hydro_yr <- function(data){ # data = dataframe with 'date' column in as.POSIXct() format
  data$hydroyear <- data$date+(184*60*60*24)
  data$hydroyear <- format(data$hydroyear,"%Y")
  data$hydroyear <- as.numeric(data$hydroyear)
  data$hydroyear_label <- paste(data$hydroyear-1, data$hydroyear, sep = "-")
  return(data)
}

dat_wide <- yr_to_hydro_yr(data = dat_wide)

# calculate number of sampling dates per year
dat_wide <- dat_wide %>% 
  ungroup() %>% 
  mutate(hydroyear,
         month = month(date)) %>% 
  distinct(hydroyear, month, .keep_all = TRUE) %>% 
  group_by(hydroyear) %>% 
  mutate(n = length(unique(paste0(hydroyear, month))))

ggplot(dat_wide, aes(x = as.Date(date), y = chl_mgm3, color = as.factor(hydroyear))) +
  geom_line()

# select just TLI Variables
dat_wide <- dat_wide %>% 
  select(date, hydroyear_label, hydroyear, chl_mgm3, secchi_m, TN_mgm3, TP_mgm3)

#################################################################################################
# data from hydroyear 1999 and 2000 from Keith Hamill
dat2 <- read_excel('./data/raw_data/Rotoehu for Whitney.xlsx')
dat2 <- dat2 %>% 
  filter(Lake=='Rotoehu',
         `H year end` > 1998 & `H year end` < 2001) %>% 
  select(Date, `H year`, `H year end`, `Chl-a (mg/m3)`:`TP (mg/m3)`) %>% 
  rename(date = Date,
         hydroyear_label = `H year`,
         hydroyear = `H year end`,
         chl_mgm3 = `Chl-a (mg/m3)`,
         secchi_m = `Clarity (m)`,
         TN_mgm3 = `TN (mg/m3)`,
         TP_mgm3 = `TP (mg/m3)`,)

###################################################################################################
# combine data sources
dat_90s <- full_join(dat_wide, dat2)

tp <- ggplot(dat_90s, aes(x = date, y = TP_mgm3, color = as.factor(hydroyear))) +
  geom_point() +
  geom_line()+
  labs(color = 'Year') +
  theme_bw()+
  theme(text = element_text(size = 14))

tn <- ggplot(dat_90s, aes(x = date, y = TN_mgm3, color = as.factor(hydroyear))) +
  geom_point() +
  geom_line()+
  labs(color = 'Year') +
  theme_bw()+
  theme(text = element_text(size = 14))

chl <- ggplot(dat_90s, aes(x = date, y = chl_mgm3, color = as.factor(hydroyear))) +
  geom_point() +
  geom_line()+
  labs(color = 'Year') +
  theme_bw()+
  theme(text = element_text(size = 14))

secchi <- ggplot(dat_90s, aes(x = date, y = secchi_m, color = as.factor(hydroyear))) +
  geom_point() +
  geom_line() +
  labs(color = 'Year') +
  theme_bw()+
  theme(text = element_text(size = 14))

p_vars <- ggarrange(tn, tp, chl, secchi, common.legend = TRUE)
p_vars
ggsave('./figures/tli_vars_90s.png', p_vars, dpi = 300, units = 'mm', height = 350, width = 500, scale = 0.5)

###################################################################################################
# combine with data from 2001 onward
dat_00s <- read.csv('./data/master_rotoehu.csv')
dat_00s$date <- as.POSIXct(dat_00s$date)
dat_00s <- dat_00s %>% 
  rename(chl_mgm3 = chla_ugL_INT,
         TN_mgm3 = top_TN_ugL,
         TP_mgm3 = top_TP_ugL) %>% 
  select(date, chl_mgm3, secchi_m, TN_mgm3, TP_mgm3) 
dat_00s <- yr_to_hydro_yr(data = dat_00s)


dat_all <- full_join(dat_90s, dat_00s)

################################################################################################
# calculate TLI
source('./scripts/R/tli_fx.R')

# calculate each month's tli and each year's TLI (annual TLI is the mean of variable then TLI calculation, done within function)
dat_all <- dat_all %>% 
  group_by(hydroyear) %>% 
  mutate(tli_annual = tli_fx(chl = chl_mgm3, TN = TN_mgm3, TP = TP_mgm3, secchi = secchi_m, timescale = 'annual')) %>% 
  mutate(month = month(date)) %>% 
  group_by(hydroyear, month) %>% 
  mutate(tli_monthly = tli_fx(chl = chl_mgm3, TN = TN_mgm3, TP = TP_mgm3, secchi = secchi_m, timescale = 'monthly'))

# subset to pre-July 2020 for manuscript
dat_all <- dat_all %>% 
  filter(date < as.Date('2021-07-10'))

p1 <- ggplot(dat_all, aes(x = as.Date(date), y = tli_annual)) +
  geom_point(aes(x = as.Date(date), y = tli_monthly, color = as.factor(hydroyear)), size = 2) +
  geom_line(aes(x = as.Date(date), y = tli_monthly, color = as.factor(hydroyear))) +
  geom_line(size = 1.5) +
  theme_bw() +
  xlab('Date') +
  ylab('Trophic Level Index') +
  labs(color = 'Year',
       linetype = '') +
  theme(text = element_text(size = 14)) #+
  #geom_hline(aes(yintercept = mean(tli_annual), linetype = 'Mean'))
p1

ggplotly(p1)
mean(dat_all$tli_annual)

ggsave('./figures/tli_timeseries.png', p1, dpi = 300, units = 'mm', height = 300, width = 500, scale = 0.5)

#############################################################################################
write.csv(dat_all, './data/processed_data/90s_data/tli_rotoehu.csv', row.names = FALSE)
