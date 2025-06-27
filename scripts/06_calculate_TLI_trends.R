library(ggplot2)
library(readxl)
library(tidyverse)
library(ggpubr)
library(Kendall)
library(plotly)
library(patchwork)


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


###################################################################################################
# combine with data from 2001 onward
dat_00s <- read.csv('./data/master_rotoehu.csv')
#dat_00s <- read.csv('./data/raw_data/rotoehu_waterquality_2000_2021.csv')
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
source('./scripts/functions//tli_fx.R')

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

# calculate decade
getDecade <- function(year) {
  year <- ifelse(year<2000, year-1900, year)
  decade <- floor(year/10) * 10
  return (decade)
}

dat_all$decade <- getDecade(year(dat_all$date))

# make it 1990 instead of 90
dat_all <- dat_all %>% 
  mutate(decade = ifelse(decade==90, 1990, decade))


dat_all <- read.csv('./data/processed_data/rotoehu_tli_1990_2021.csv')




tli <- ggplot(dat_all, aes(x = as.Date(date), y = tli_annual)) +
  geom_point(aes(x = as.Date(date), y = tli_monthly, color = as.factor(decade)), size = 2) +
  geom_line(aes(x = as.Date(date), y = tli_monthly, color = as.factor(decade))) +
  geom_line(size = 1) +
  theme_bw() +
  scale_color_manual(values = c('#D55E00', '#009E73',  '#E69F00', '#0072B2')) +
  xlab('Date') +
  ylab('Trophic Level Index') +
  labs(color = 'Decade',
       linetype = '') +
  theme(text = element_text(size = 12)) #+
  #geom_hline(aes(yintercept = mean(tli_annual), linetype = 'Mean'))
tli

#################
# make plots of each of the TLI components
chl <- ggplot(dat_all, aes(x = as.Date(date), y = chl_mgm3, color = as.factor(decade))) +
  geom_point(size = 2) +
  geom_line() +
  theme_bw() +
  #scale_x_date(expand = c(0,0))+
  xlab('Date') +
  ylab('Chlorophyll-a (ug/L)') +
  scale_color_manual(values = c('#D55E00', '#009E73',  '#E69F00', '#0072B2')) +
  theme(text = element_text(size = 12),
        legend.position = 'none', 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank())

tn <- ggplot(dat_all, aes(x = as.Date(date), y = TN_mgm3, color = as.factor(decade))) +
  geom_point(size = 2) +
  geom_line() +
  theme_bw() +
  scale_color_manual(values = c('#D55E00', '#009E73',  '#E69F00', '#0072B2')) +
  #scale_x_date(expand = c(0,0))+
  xlab('Date') +
  ylab('Total Nitrogen (ug/L)') +
  labs(color = 'Year',
       linetype = '') +
  theme(text = element_text(size = 12),
        legend.position = 'none', 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank())
tn

tp <- ggplot(dat_all, aes(x = as.Date(date), y = TP_mgm3, color = as.factor(decade))) +
  geom_point(size = 2) +
  geom_line() +
  theme_bw() +
  scale_color_manual(values = c('#D55E00', '#009E73',  '#E69F00', '#0072B2')) +
  #scale_x_date(expand = c(0,0))+
  xlab('Date') +
  ylab('Total Phosphorus (ug/L)') +
  labs(color = 'Year',
       linetype = '') +
  theme(text = element_text(size = 12),
        legend.position = 'none', 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank())
tp

secchi <- ggplot(dat_all, aes(x = as.Date(date), y = secchi_m, color = as.factor(decade))) +
  geom_point(size = 2) +
  geom_line() +
  theme_bw() +
  xlab('Date') +
  ylab('Secchi Depth (m)') +
  scale_color_manual(values = c('#D55E00', '#009E73',  '#E69F00', '#0072B2')) +
  #scale_x_date(expand = c(0,0))+
  labs(color = 'Year',
       linetype = '') +
  theme(text = element_text(size = 12),
        legend.position = 'none', 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank())

secchi

#components <- ggarrange(chl, secchi, tn, tp, labels = 'auto')
#p1 <- ggarrange(components, tli, ncol = 1, labels = 'auto')
#p1

library(patchwork)
(components <- chl +secchi+ tn+tp+tli +plot_layout(ncol = 1)+plot_annotation(tag_levels = 'A',tag_suffix = '.') & 
    theme(plot.tag = element_text(face = "bold")))


ggsave('./figures/MS/si_figs/tli_components_1990_2021.png', components, dpi = 300, units = 'mm', 
       height = 500, width = 350, scale = 0.6)


#############################################################################################
write.csv(dat_all, './data/processed_data/rotoehu_tli_1990_2021.csv', row.names = FALSE)


# calculate individual TLi components
dat_all <- dat_all %>% 
  group_by(date) %>% 
  mutate(TLI_tp = 0.218+2.92*log10(TN_mgm3 ),
         TLI_tn = -3.61+3.01*log10(TP_mgm3 ),
         TLI_chl = 2.22+2.54*log10(chl_mgm3 ),
         TLI_secchi = 5.56+2.6*log10(1/secchi_m - 1/40) ) %>% 
  ungroup()

dat_long <- dat_all %>% 
  pivot_longer(TLI_tp:TLI_secchi, names_to = 'TLI_var', values_to = 'value')

tli_parts <- ggplot(dat_long, aes(x = as.Date(date), y = value, color = as.factor(hydroyear))) +
  geom_point() +
  geom_point(size = 2) +
  facet_wrap(~TLI_var) +
  geom_line() +
  theme_bw() +
  xlab('Date') +
  labs(color = 'Year',
       linetype = '') +
  theme(text = element_text(size = 12),
        legend.position = 'none')

tli_parts_1facet <- ggplot(dat_long, aes(x = as.Date(date), y = value, color = TLI_var)) +
  geom_point() +
  geom_point(size = 2) +
  geom_line() +
  theme_bw() +
  xlab('Date') +
  labs(color = 'Year',
       linetype = '') +
  theme(text = element_text(size = 12))

tli_parts

#ggsave('./figures/resubmission/si_figs/tli_components_1990_2022.png', tli_parts_1facet, 
      # dpi = 300, units = 'mm', 
      # height = 200, width = 300, scale = 0.8)

##### calculate trends
data_split <- split(dat_long, dat_long$TLI_var)

# Function for Mann-Kendall trend test
test_trend <- function(df) {
  MannKendall(df$value)
}

trends <- lapply(data_split, test_trend)
trends

# format the df
trends_df <- lapply(names(trends), function(var){
  out <- trends[[var]]
  data.frame(variable = var,
             tau = round(out$tau, 3),
             p_value = round(out$sl, 3))
}) %>% 
  bind_rows()

trends_df

write.csv(trends_df, './figures/si_table_trend_output_tli_components.csv', row.names = FALSE)
