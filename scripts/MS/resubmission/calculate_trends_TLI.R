# conduct a trend analysis on the TLI components
library(tidyverse)
library(plotly)
library(Kendall)
library(zoo)
source('./scripts/functions/tli_fx.R')

data <- read.csv('./data/master_rotoehu.csv')
data <- data %>% 
  select(date:secchi_m)

# do linear interpolation on the few months without obs
data <- data %>% 
  mutate(chla_ugL_INT = na.approx(chla_ugL_INT, na.rm = FALSE, rule = 2, maxgap = 4),
         top_TN_ugL = na.approx(top_TN_ugL, na.rm = FALSE, rule = 2, maxgap = 4),
         top_TP_ugL = na.approx(top_TP_ugL, na.rm = FALSE, rule = 2, maxgap = 4),
         secchi_m = na.approx(secchi_m, na.rm = FALSE, rule = 2, maxgap = 4))

# calculate TLI
tli_obs <- data %>% 
  group_by(date) %>% 
  mutate(tli_obs = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m)) %>% 
  ungroup() %>% 
  group_by(hydroyear) %>% 
  mutate(tli_annual = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m)) 


ts_data <- ts(tli_obs$tli_obs, start = c(year(min(tli_obs$date)), month(min(tli_obs$date))), frequency = 12)
decomp <- decompose(ts_data)
plot(decomp)

tli_trend <- MannKendall(tli_obs$tli_obs)
tli_trend_df <- data.frame(variable = 'TLI',
                        tau = round(tli_trend$tau, 3),
                        p_value = round(tli_trend$sl, 3))  
tli_trend_df  
################################################################################
# trend analysis and time series decomposition for TLI components
data_long <- data %>% 
  pivot_longer(chla_ugL_INT:secchi_m, names_to = 'tli_var', values_to = 'value')

ggplot(data_long, aes(x = as.Date(date), y = value, color = tli_var)) +
  geom_point() +
  facet_wrap(~tli_var, scales = 'free') +
  theme_bw()

head(data_long)

# functions for ts decomposition and trend test
# Function for time series decomposition
decompose_ts <- function(df) {
  ts_data <- ts(df$value, start = c(year(min(df$date)), month(min(df$date))), frequency = 12)
  decompose(ts_data)
}

# Function for Mann-Kendall trend test
test_trend <- function(df) {
  MannKendall(df$value)
}

data_split <- split(data_long, data_long$tli_var)

# run the time series decomposition function
decomp <- lapply(data_split, decompose_ts)
plot(decomp$chla_ugL_INT)
plot(decomp$top_TN_ugL)
plot(decomp$top_TP_ugL)
plot(decomp$secchi_m)
title(main = 'Secchi')

trends <- lapply(data_split, test_trend)
trends$chla_ugL_INT$tau[1]

trends_df <- lapply(names(trends), function(var){
  out <- trends[[var]]
  data.frame(variable = var,
             tau = round(out$tau, 3),
             p_value = round(out$sl, 3))
}) %>% 
  bind_rows()

trends_df
trends_df <- rbind(tli_trend_df, trends_df)

write.csv(trends_df, './figures/trend_output.csv', row.names = FALSE)
