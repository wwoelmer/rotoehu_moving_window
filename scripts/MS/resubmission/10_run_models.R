# run simple AR model for TLI + one covariate from set of potential driver variables
# across three different temporal aggregations (full window, 3 discrete windows, 8-year moving window)

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

hist(dat$tli_monthly)
tli <- ggplot(dat, aes(x = as.Date(date), y = tli_monthly)) +
  geom_point(size = 1.2) +
  geom_line(aes(x = as.Date(date), y = tli_annual), linewidth = 2) +
  theme_bw()
ggplotly(tli)

mean(dat$tli_annual, na.rm = TRUE)

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
               #"schmidt_stability", 
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

col_no_all <- length(unique(out$id_covar))
col_pal_all <- colorRampPalette(brewer.pal(9, "Set1"))(col_no_all)


p1 <- out %>% 
  distinct(id_covar, start_date, .keep_all = TRUE) %>% 
  ggplot(aes(x = as.Date(start_date), y = r2, color = as.factor(id_covar))) +
  geom_point(size = 2) +
  scale_color_manual(values = col_pal_all) +
  #facet_wrap(~id_covar, scales = 'free') +
  theme_bw() +
  ylab('R2') +
  labs(color = 'Covariate') +
  ylim(0.2, 0.7) +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  xlab('Covariate')
p1
write.csv(out, './data/model_output_moving_window.csv', row.names = FALSE)

###############################################################################################
# run the simulation on three 7-year windows

out_three <- data.frame()

# identify the dates at which to split the time series
dates <- unique(dat$date)
split_indices <- round(seq(1, length(dates), length.out = 4)[-c(4)])

# Get the three split dates
split_dates <- dates[split_indices]

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
  
   for(j in 1:length(split_dates)){
    print(split_dates[j])
     
    id_split <- which(dat_ar$date == split_dates[j])
    dat_window <- dat_ar[id_split:(id_split + window_length - 1),]
    dat_window <- na.omit(dat_window)
    
    opd <-  weighted_ordinal_pattern_distribution(x = dat_window$tli_monthly, ndemb = 4)
    pe <- permutation_entropy(opd) 
    
    # run the model
    d <- run_ar(data = dat_window, 
                id_var = id_var, 
                id_covar = test_vars[i])
    d$iter_start <- NA
    d$iter_end <- NA
    d$start_date <- min(dat_window$date)
    d$end_date <- max(dat_window$date)
    d$n <- nrow(dat_window)
    d$pe <- pe
    d$time_frame <- 'discrete_window'
    out_three <- rbind(out_three, d)
    
  }
}


col_no_all <- length(unique(out_three$id_covar))
col_pal_all <- colorRampPalette(brewer.pal(9, "Set1"))(col_no_all)


p2 <- out_three %>% 
  distinct(id_covar, start_date, .keep_all = TRUE) %>% 
  ggplot(aes(x = id_covar, y = r2, color = as.factor(id_covar))) +
  geom_point(size = 4) +
  scale_color_manual(values = col_pal_all) +
  facet_wrap(~start_date, scales = 'free') +
  theme_bw() +
  ylab('R2') +
  labs(color = 'Covariate') +
  ylim(0.2, 0.7) +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  xlab('Covariate')
p2
write.csv(out_three, './data/model_output_three_windows.csv', row.names = FALSE)


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
  
  opd <-  weighted_ordinal_pattern_distribution(x = dat_ar$tli_monthly, ndemb = 4)
  pe <- permutation_entropy(opd) 
  
  # run the model
  d <- run_ar(data = dat_ar, id_var = id_var, id_covar = test_vars[i])
  d$start_date <- min(dat_ar$date)
  d$end_date <- max(dat_ar$date)
  d$n <- nrow(dat_ar)
  d$pe <- pe
  d$time_frame <- 'full_ts'
  out_all_ts <- rbind(out_all_ts, d)
  
  
}


col_no_all <- length(unique(out_all_ts$id_covar))
col_pal_all <- colorRampPalette(brewer.pal(9, "Set1"))(col_no_all)

p3 <- out_all_ts %>% 
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
p3
ggarrange(p3, p2, p1, common.legend = TRUE, nrow = 1)

write.csv(out_all_ts, './data/model_output_full.csv', row.names = FALSE)