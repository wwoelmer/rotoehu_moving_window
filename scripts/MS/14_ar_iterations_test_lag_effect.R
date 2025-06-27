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
  mutate(tli_annual = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m))

ggplot(dat, aes(x = as.Date(date), y = tli_monthly, color = as.factor(year))) +
  geom_point()

summ_tli <- dat %>% 
  group_by(year) %>% 
  summarise(min = min(tli_monthly),
            max = max(tli_monthly)) %>% 
  mutate(diff = round(max - min, 1))
summ_tli

#######################################################
# run the ar model simulation
source('./scripts/functions/run_ar.R')

# this set of variables comes from the decadal analysis (90s, 2000s, 2010s) plus land cover, alum, and 'none'
test_vars <- c("bottom_DRP_ugL", "bottom_NH4_ugL",
               "temp_C_8", "air_temp_mean", "windspeed_min", 
               "monthly_avg_level_m", 
               "sum_alum", "none")

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
                                  "sum_alum", "none"),
                       labels = c("bottom DRP", "bottom NH4", "bottom water temp",
                                  "mean air temp", "min windspeed", "monthly water level", 
                                  "alum dosed", "None"))
col_no <- length(unique(out$id_covar)) + 1
col_pal <- colorRampPalette(brewer.pal(9, "Set1"))(col_no)

out_prop <- out %>% 
  distinct(id_covar, start_date, .keep_all = TRUE) %>% 
  group_by(start_date) %>% 
  mutate(diff_from_best = max(r2) - r2,
         rank = dense_rank(desc(r2)),
         r2_none = r2[id_covar=='None'],
         diff_from_none = r2 - r2_none,
         rank_AR = dense_rank(desc(diff_from_none)),
         aic_none = aic[id_covar=='None'],
         diff_from_none_aic = aic - aic_none,
         rank_aic = dense_rank(desc(diff_from_none_aic)),
         significant = p_value < 0.05)
out_prop$start_date <- as.Date(out_prop$start_date)

p <- ggplot(out_prop, aes(x = (start_date), y = diff_from_none_aic, color = id_covar)) +
  geom_rect(aes(xmin = min(start_date), xmax = max(start_date), 
                ymin = -2, ymax = 2), alpha = 0.8, fill = "grey") +
  geom_hline(aes(yintercept = 0)) +
  geom_point(size = 2) +
  facet_wrap(~id_covar, scales = 'free_y', ncol = 1) +
  theme_bw() +
  ylab(expression(Delta~AIC[c])) +
  xlab('Window start date') +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = col_pal) 
p

ggsave('./figures/MS/si_figs/deltaaic_1_lag.png', p,
       dpi = 300, units = 'mm',  height = 500, width = 350, scale = 0.6)
