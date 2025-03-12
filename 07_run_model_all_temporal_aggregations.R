# run simple AR model for TLI + one covariate from set of potential driver variables

library(tidyverse)
library(MuMIn)
library(tidymodels)
library(plotly)
library(RColorBrewer)
library(patchwork)
library(ggpubr)
library(statcomp)


# read in data
dat <- read.csv('./data/master_rotoehu.csv')
# set zeroes to NA for alum
dat <- dat %>% 
  mutate(sum_alum = ifelse(is.na(sum_alum), 0, sum_alum))

# subset to time period of this study
dat <- dat %>% 
  filter(date > as.Date('2000-07-01')&
           date < as.Date ('2021-07-01')) 

mean_secchi <- mean(dat$secchi_m, na.rm = TRUE)
mean_secchi

#calculate hydro year
dat$hydroyear <- as.POSIXct(dat$date) + (184*60*60*24)
dat$hydroyear <- format(dat$hydroyear,"%Y")
dat$hydroyear <- as.numeric(dat$hydroyear)
dat$hydroyear_label <- paste(dat$hydroyear-1, dat$hydroyear, sep = "-")


# remove daily water level as it is similar ot monthly (see daily_vs_monthly_water_level_comparison.R)
dat <- dat %>% 
  select(-avg_level_m)

# get rid of dates that do not have all four TLi variables
dat <- dat %>% 
  filter(across(c(secchi_m, top_TN_ugL, top_TP_ugL, chla_ugL_INT), ~ !is.na(.)))


# calculate monthly TLI
source('./scripts/functions/tli_fx.R')

dat <- dat %>% 
  group_by(month, year, lake, site) %>%
  mutate(tli_monthly = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m)) %>% 
  group_by(year, lake, site) %>%
  mutate(tli_annual = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m)) %>% 
  select(lake, site, date, tli_annual, tli_monthly, 
         chla_ugL_INT, top_TN_ugL, top_TP_ugL, secchi_m, 
         everything())


#######################################################
# run the ar model simulation
source('./scripts/functions/run_ar.R')

# this set of variables comes from the decadal analysis (90s, 2000s, 2010s) plus alum, and 'none'
# see output of script 06_decadal_correlation_coefficient.R for list of variables
test_vars <- c("bottom_DRP_ugL", "bottom_NH4_ugL",
               "temp_C_8", "air_temp_mean", "windspeed_min", 
               "monthly_avg_level_m", 
               #"schmidt_stability", 
               "sum_alum",
               "none")

###############################################################################################
# run the simulation on the entire dataset, without subsetting to time periods

out_all_ts <- data.frame()

for(i in 1:length(test_vars)){
  if(test_vars[i]=='none'){
    dat_ar <- dat %>% 
      ungroup() %>% 
      select(date, id_var)  
  }else{
    dat_ar <- dat %>% 
      ungroup() %>% 
      select(date, id_var, test_vars[i])  
  }
  # run the model
  d <- run_ar(data = dat_ar, id_var = id_var, id_covar = test_vars[i])
  d$start_date <- min(dat_ar$date)
  d$end_date <- max(dat_ar$date)
  d$n <- nrow(dat_ar)
  out_all_ts <- rbind(out_all_ts, d)
  
  
}


col_no_all <- length(unique(out_all_ts$id_covar))
col_pal_all <- colorRampPalette(brewer.pal(9, "Set1"))(col_no_all)

full <- out_all_ts %>% 
  distinct(id_covar, .keep_all = TRUE) %>% 
  ggplot(aes(x = id_covar, y = r2, color = as.factor(id_covar))) +
  geom_point(size = 4) +
  scale_color_manual(values = col_pal_all) +
  theme_bw() +
  ylab('R2') +
  labs(color = 'Covariate') +
  ylim(0.2, 0.7) +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  xlab('Covariate')
full

write.csv(out_all_ts, './data/model_output_full.csv')

