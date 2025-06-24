# run moving window analysis including thermocline depth

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

dat %>% 
  distinct(year, tli_annual, .keep_all = TRUE) %>% 
  ggplot(aes(x = as.Date(date), y = tli_annual)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 3.9) +
  ylim(0, 6) +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('Date') +
  ylab('Annual TLI')

#######################################################
# run the ar model simulation
source('./scripts/functions/run_ar.R')

# this set of variables comes from the decadal analysis (90s, 2000s, 2010s) plus alum, and 'none'
# see output of script 06_decadal_correlation_coefficient.R for list of variables
test_vars <- c("bottom_DRP_ugL", "bottom_NH4_ugL",
               "temp_C_8", "air_temp_mean", "windspeed_min", 
               "monthly_avg_level_m", 
               "thermo_depth",
               "sum_alum",
               "none")

id_var <- "tli_monthly"
window_length <- 100
n_iter <- seq(1, nrow(dat) - window_length)

out <- data.frame()

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
  
  print(test_vars[i])
  
  for(j in 1:length(n_iter)){
    print(n_iter[j])
    # subset to the 100 observations in the iteration
    start <- j
    end <- j + window_length
    dat_sub <- dat_ar[start:end,]
    opd <-  weighted_ordinal_pattern_distribution(x = dat_sub$tli_monthly, ndemb = 4)
    pe <- permutation_entropy(opd) 
    
    # run the model
    d <- run_ar(data = dat_sub, 
                id_var = id_var, 
                id_covar = test_vars[i], 
                window_length = window_length)
    d$iter_start <- start
    d$iter_end <- end
    d$start_date <- min(dat_sub$date)
    d$end_date <- max(dat_sub$date)
    d$n <- nrow(dat_sub)
    d$pe <- pe
    out <- rbind(out, d)
    
  }
}

####################################################################################
## analyze output to compare thermocline depth results

ggplot(out, aes(x = as.Date(start_date), y = r2, color = id_covar)) +
  geom_line() +
  geom_point(size = 2) +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('Start date of iteration (+100 obs)') +
  facet_wrap(~id_covar, scales = 'free') +
  ylab(bquote(~R^2)) +
  #scale_color_manual(values = col_pal) +
  labs(color = 'Driver')

out_prop <- out %>% 
  distinct(id_covar, iter_start, .keep_all = TRUE) %>% 
  select(id_covar:iter_end, start_date, end_date, r2, p_value) %>% 
  group_by(iter_start) %>% 
  mutate(diff_from_best = max(r2) - r2,
         rank = dense_rank(desc(r2)),
         r2_none = r2[id_covar=='none'],
         diff_from_none = r2 - r2_none,
         rank_AR = dense_rank(desc(diff_from_none)),
         aic_none = aic[id_covar=='none'],
         diff_from_none_aic = aic - aic_none,
         rank_aic = dense_rank(desc(diff_from_none_aic)),
         significant = p_value < 0.05)

thermo <- ggplot(out_prop, aes(x = as.Date(start_date), y = diff_from_none_aic)) +
  geom_rect(aes(xmin = min(as.Date(start_date)), xmax = max(as.Date(start_date)), 
                ymin = -2, ymax = 2), alpha = 0.8, fill = "grey") +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(color = as.factor(id_covar), shape = significant)) +
  facet_wrap(~id_covar) +
  theme_bw() +
  ylab(expression(Delta~AIC[c])) +
  xlab('Start of Iteration') +
  theme(text=element_text(size=18)) +
  labs(color = 'Driver')

thermo

ggsave('./figures/resubmission/si_figs/drivers_plus_thermocline_deltaAIC.png', thermo,
       dpi = 300, units = 'mm', height = 300, width = 400, scale = 0.6)
