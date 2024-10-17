# do some stuff with thresholds between TLI and driver variables

library(tidyverse)
#install.packages('moments')
library(moments)

#### model output
out <- read.csv('./data/processed_data/moving_window/model_output.csv')
vars <- unique(out$id_covar)

coef <- out %>% 
  filter(covar %in% vars) %>% 
  select(covar:p_value, r2, pe, start_date, end_date)

### observational data
data <- read.csv('./data/master_rotoehu.csv')

#calculate hydro year
data$hydroyear <- as.POSIXct(data$date) + (184*60*60*24)
data$hydroyear <- format(data$hydroyear,"%Y")
data$hydroyear <- as.numeric(data$hydroyear)
data$hydroyear_label <- paste(data$hydroyear-1, data$hydroyear, sep = "-")

source('./scripts/R/tli_fx.R')

# calculate monthly TLI and log transform non-normal data
data_tf <- data %>% 
  group_by(month, year) %>%
  mutate(tli_monthly = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m)) %>% 
  select(date, tli_monthly, year, month, vars[vars!='none']) %>% 
  mutate(log_DRP = log10(bottom_DRP_ugL),
         log_NH4 = log10(bottom_NH4_ugL),
         log_schmidt_stability = log10(schmidt_stability),
         log_alum = log10(sum_alum))

colnames(data_tf)

data_long <- data_tf %>% 
  pivot_longer(c(temp_C_8:monthly_avg_level_m, log_DRP:log_alum), names_to = 'variable', values_to = 'value')

ggplot(data_long, aes(x = value, fill = as.factor(variable))) +
  geom_histogram() +
  facet_wrap(~as.factor(variable), scales = 'free') 
  
ggplot(data_long, aes(x = value, y = tli_monthly, color = as.factor(year(date)))) +
  geom_point() +
  facet_wrap(~as.factor(variable), scales = 'free') #+
  geom_smooth() 

ggplot(data_long, aes(x = as.Date(date), y = value, color = as.factor(variable))) +
  geom_point() +
  geom_line() +
  facet_wrap(~as.factor(variable), scales = 'free') 

# some missing data for temp and schmidt stability in early 2000's?

## check correlations among variables
cor_df <- data_tf %>% 
  ungroup() %>% 
  select(tli_monthly, bottom_DRP_ugL:sum_alum) 
cor_df <- na.omit(cor_df)

cor_out <- round(cor(cor_df), 2)
cor_out
write.csv(cor_out, './data/processed_data/moving_window/correlation_across_drivers.csv', row.names = FALSE)

#####################################################################################
# can we connect shifts in parameter values to changes in drivers?

# for each simulation time period, calculate the mean, min, max values of each driver and see if that relates to changes in parameter values or TLI
summ <- NULL
start_dates <- unique(coef$start_date)
end_dates <- unique(coef$end_date)

for (i in 1:length(start_dates)){
  sub <- data_tf %>% 
    ungroup() %>% 
    filter(date >= start_dates[i] & date <= end_dates[i]) %>%
    select(date, vars[vars!='none']) %>% 
    pivot_longer(bottom_DRP_ugL:sum_alum, names_to = 'variable', values_to = 'obs_value') #%>%
  
  ggplot(sub, aes(x = as.Date(date), y = obs_value)) +
    geom_point() +
    geom_line() +
    geom_smooth(method = 'lm') +
    facet_wrap(~variable, scales = 'free_y')

  # calculate statistics summarizing the time series
  sub <- sub %>% 
    group_by(variable) %>% 
    mutate(mean = mean(obs_value, na.rm = TRUE),
           min = min(obs_value, na.rm = TRUE),
           max = max(obs_value, na.rm = TRUE),
           sd = sd(obs_value, na.rm = TRUE),
           cv = sd/mean,
           kurtosis = kurtosis(obs_value, na.rm = TRUE),
           skew = skewness(obs_value, na.rm = TRUE)) %>% 
    distinct(variable, .keep_all = TRUE) %>% 
   # select(-obs_value) %>% 
    mutate(start_date = coef$start_date[i],
           sim_no = i)
  
  sub_long <- sub %>% 
    pivot_longer(obs_value:skew, names_to = 'summary', values_to = 'value')
  
  summ <- rbind(summ, sub_long)
  
}

summ <- summ %>% 
  rename(covar = variable)

ggplot(summ, aes(x = as.Date(start_date), y = value, color = summary)) +
  geom_point() +
  geom_line() +
  facet_wrap(~covar, scales = 'free')

coef <- coef %>% 
  rename(param_value = value)
x <- left_join(coef, summ, by = c('start_date', 'covar'))

ggplot(x[x$summary=='mean',], aes(x = value, y = param_value, color = as.factor(year(start_date)))) + 
  geom_point() +
  facet_wrap(~covar, scales = 'free') +
#  geom_smooth() +
  ylab('Parameter Value') +
  xlab('Mean driver value') +
  ggtitle('Mean value of driver time series')

ggplot(x[x$summary=='min',], aes(x = value, y = param_value, color = as.factor(year(start_date)))) + 
  geom_point() +
  facet_wrap(~covar, scales = 'free') +
#  geom_smooth() +
  ylab('Parameter Value') +
  xlab('Minimum driver value') +
  ggtitle('Min value of driver time series')

ggplot(x[x$summary=='max',], aes(x = value, y = param_value, color = (covar))) + 
  geom_point() +
  facet_wrap(~covar, scales = 'free') +
#  geom_smooth() +
  ylab('Parameter Value') +
  xlab('Maximum driver value') +
  ggtitle('Max value of driver time series')

ggplot(x[x$summary=='cv',], aes(x = value, y = param_value, color = as.factor(year(start_date)))) + 
  geom_point() +
  facet_wrap(~covar, scales = 'free') +
  geom_smooth() +
  ylab('Parameter Value') +
  xlab('CV of driver value') +
  ggtitle('CV of driver time series')

ggplot(x, aes(x = pe, y = param_value, color = (covar))) + 
  geom_point() +
  facet_wrap(~covar, scales = 'free') +
  geom_smooth() +
  ylab('Parameter Value') +
  xlab('Permutation Entropy') +
  ggtitle('Permutation Entropy of driver time series')

ggplot(x[x$summary=='sd',], aes(x = value, y = param_value, color = (covar))) + 
  geom_point() +
  facet_wrap(~covar, scales = 'free') +
  geom_smooth() +
  ylab('Parameter Value') +
  xlab('Standard Deviation of driver value') +
  ggtitle('Standard Deviation of driver time series')

ggplot(x[x$summary=='kurtosis',], aes(x = value, y = param_value, color = (covar))) + 
  geom_point() +
  facet_wrap(~covar, scales = 'free') +
  geom_smooth() +
  ylab('Parameter Value') +
  xlab('Kurtosis of driver value') +
  ggtitle('Kurtosis of driver time series')

ggplot(x[x$summary=='skew',], aes(x = value, y = param_value, color = (covar))) + 
  geom_point() +
  facet_wrap(~covar, scales = 'free') +
  geom_smooth() +
  ylab('Parameter Value') +
  xlab('Skew of driver value') +
  ggtitle('Skew of driver value') 

