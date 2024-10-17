# test the effect of including only lag 1
# run simple AR model for TLI + one covariate from each group

library(tidyverse)
library(MuMIn)
library(tidymodels)
library(plotly)
library(RColorBrewer)
library(patchwork)
library(ggpubr)
library(statcomp)


# read in data
#dat <- read.csv('./data/processed_data/BoP_wq_2007_2021.csv')
dat <- read.csv('./data/master_rotoehu.csv')

#calculate hydro year
dat$hydroyear <- as.POSIXct(dat$date) + (184*60*60*24)
dat$hydroyear <- format(dat$hydroyear,"%Y")
dat$hydroyear <- as.numeric(dat$hydroyear)
dat$hydroyear_label <- paste(dat$hydroyear-1, dat$hydroyear, sep = "-")


# remove daily water level as it is similar ot monthly (see daily_vs_monthly_water_level_comparison.R)
dat <- dat %>% 
  select(-avg_level_m)


# calculate monthly TLI
source('./scripts/R/tli_fx.R')

dat <- dat %>% 
  group_by(month, year, lake, site) %>%
  mutate(tli_monthly = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m)) %>% 
  group_by(year, lake, site) %>%
  mutate(tli_annual = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m))

#######################################################
# run the ar model simulation
source('./scripts/R/run_ar.R')

# this set of variables comes from the decadal analysis (90s, 2000s, 2010s) plus land cover, alum, and 'none'
test_vars <- c("bottom_DRP_ugL", "bottom_NH4_ugL",
               "temp_C_8", "air_temp_mean", "windspeed_min", 
               "monthly_avg_level_m", 
               "schmidt_stability", 
               "sum_alum")

id_var <- "tli_monthly"


window_length <- 100
n_iter <- seq(1, nrow(dat) - window_length)

out <- data.frame()

for(i in 1:length(test_vars)){
  dat_ar <- dat %>% 
    ungroup() %>% 
    select(date, id_var, test_vars[i])
  
  for(j in 1:length(n_iter)){
    
    # subset to the 100 observations in the iteration
    start <- j
    end <- j + window_length
    dat_sub <- dat_ar[start:end,]
    
    # run the model
    d <- run_ar(data = dat_sub, id_var = id_var, id_covar = test_vars[i], window_length = window_length, lag_id = FALSE)
    d$iter_start <- start
    d$iter_end <- end
    d$start_date <- min(dat_sub$date)
    d$end_date <- max(dat_sub$date)
    d$n <- nrow(dat_sub)
    out <- rbind(out, d)
    
  }
}

################################################################################
# set up labels and levels of factor

out$id_covar <- factor(out$id_covar, 
                       levels = c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                                  "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                  "schmidt_stability", "sum_alum"),
                       labels = c("bottom DRP", "bottom NH4", "bottom water temp",
                                  "mean air temp", "min windspeed", "monthly water level", 
                                  "schmidt stability", "alum dosed"))
 col_no <- length(unique(out$id_covar)) + 1
col_pal <- colorRampPalette(brewer.pal(9, "Set1"))(col_no)

p <- ggplot(out, aes(x = iter_start, y = r2, color = id_covar)) +
           geom_point() +
           scale_color_manual(values = col_pal) +
           geom_line() +
           theme_bw() +
  xlab('Start of Iteration') +
  labs(color = 'Driver') +
  ylab('R2')
p

ggsave('./figures/moving_window/r2_timeseries_1lag.png', p,
       dpi = 300, units = 'mm', height = 300, width = 600, scale = 0.4)
