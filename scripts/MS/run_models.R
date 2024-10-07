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


# calculate monthly TLI
source('./scripts/functions/tli_fx.R')

dat <- dat %>% 
  group_by(month, year, lake, site) %>%
  mutate(tli_monthly = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m)) %>% 
  group_by(year, lake, site) %>%
  mutate(tli_annual = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m))

hist(dat$tli_monthly)
tli <- ggplot(dat, aes(x = as.Date(date), y = tli_monthly)) +
  geom_point(size = 1.2) +
  geom_line(aes(x = as.Date(date), y = tli_annual), linewidth = 2) +
  theme_bw()
ggplotly(tli)

mean(dat$tli_annual)

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

# this set of variables comes from the decadal analysis (90s, 2000s, 2010s) plus land cover, alum, and 'none'
test_vars <- c("bottom_DRP_ugL", "bottom_NH4_ugL",
               "temp_C_8", "air_temp_mean", "windspeed_min", 
               "monthly_avg_level_m", 
               "schmidt_stability", 
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

write.csv(out, './data/model_output.csv', row.names = FALSE)




#######################################################################################
### PROBABLY CUT BELOW ################################################################
## select a single driving covariate and compare across model parameters
out %>% 
  filter(id_covar=='air_temp_mean') %>% 
  ggplot(aes(x = as.Date(start_date), y = value, color = covar)) +
  geom_point() +
  facet_wrap(~covar, scales = 'free_y') +
  theme_bw()

out %>% 
  filter(id_covar=='avg_level_m') %>% 
  ggplot(aes(x = as.Date(start_date), y = value, color = covar)) +
  geom_point() +
  facet_wrap(~covar, scales = 'free_y') +
  theme_bw()

out %>% 
  filter(id_covar=='none') %>% 
  ggplot(aes(x = as.Date(start_date), y = value, color = covar)) +
  geom_point() +
  facet_wrap(~covar, scales = 'free_y') +
  theme_bw()

## look at p-values
out %>% 
  filter(id_covar=='none',
         p_value < 0.05) %>% 
  ggplot(aes(x = as.Date(start_date), y = p_value, color = covar)) +
  geom_point() +
  facet_wrap(~covar, scales = 'free_y') +
  theme_bw()

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
    d$iter_start <- start
    d$iter_end <- end
    d$start_date <- min(dat_sub$date)
    d$end_date <- max(dat_sub$date)
    d$n <- nrow(dat_sub)
    out_all_ts <- rbind(out_all_ts, d)
    
  
}


col_no_all <- length(unique(out_all_ts$id_covar))
col_pal_all <- colorRampPalette(brewer.pal(9, "Set1"))(col_no_all)

out_all_ts %>% 
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


length(unique(dat$year))

############################################
# plot time series of driver variables with highlighted areas where that driver was of a high rank
source('./scripts/R/plot_date_range_rank.R')

# check a few out
plot_date_range_rank(variable = 'rain_mean', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'Rain')
plot_date_range_rank(variable = 'rain_mean', rank_plot = 2, df_rank = out_prop, df_driver = dat, ylab = 'Rain')
plot_date_range_rank(variable = 'temp_C_8', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'Bottom Water Temperature (C)')
plot_date_range_rank(variable = 'air_temp_mean', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'Mean Air Temperature (C)')
plot_date_range_rank(variable = 'bottom_DRP_ugL', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'Bottom Water DRP (ug/L)')
plot_date_range_rank(variable = 'TN_TP', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'TN:TP')
plot_date_range_rank(variable = 'thermo_depth', rank_plot = 4, df_rank = out_prop, df_driver = dat, ylab = 'thermocline depth')

dat <- dat %>% 
  mutate(diff_P = top_DRP_ugL - bottom_DRP_ugL,
         diff_NH4 = top_NH4_ugL - bottom_NH4_ugL,
         diff_NO3 = top_NO3_ugL - bottom_NO3_ugL) 
  
plot_date_range_rank(variable = 'bottom_DRP_ugL', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'diff btw surface and bottom DRP')

# some examples to save
exforest <- plot_date_range_rank(variable = 'area_pct_exotic_forest', rank_plot = 2, df_rank = out_prop, df_driver = dat, ylab = '% Exotic Forest in Watershed', title = FALSE, shading = FALSE)
lvl <- plot_date_range_rank(variable = 'avg_level_m', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'Average Water Level (m)', title = FALSE, shading = FALSE)
drp <- plot_date_range_rank(variable = 'bottom_DRP_ugL', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'Bottom Water DRP (ug/L)', title = FALSE, shading = FALSE)
btemp <- plot_date_range_rank(variable = 'temp_C_8', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'Bottom Water Temperature (C)', title = FALSE, shading = FALSE)

(exforest + lvl)/ (drp + btemp)
drp
btemp

sel_vars <- c('area_pct_exotic_forest', 'avg_level_m')
ggplot(out_prop[out_prop$id_covar %in% sel_vars,], aes(x = as.Date(start_date), y = as.factor(rank), color = as.factor(id_covar))) +
  geom_point() +
  facet_wrap(~id_covar) +
  theme_bw() +
  ylab('Rank') +
  xlab('Start of Iteration') +
  theme(text=element_text(size=12)) +
  scale_color_manual(values = col_pal) +
  labs(color = 'Covariate')

