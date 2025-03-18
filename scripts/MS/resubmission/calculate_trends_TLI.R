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
data <- data %>% 
  group_by(date) %>% 
  mutate(tli_obs = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m)) %>% 
  ungroup() %>% 
  group_by(hydroyear) %>% 
  mutate(tli_annual = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m)) 

# select relevant vars
select_vars <-  c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                  "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                  "sum_alum", "chla_ugL_INT", "top_TN_ugL", "top_TP_ugL",
                  "secchi_m", "tli_obs")

data_long <- data %>% 
  filter(date < '2021-06-30') %>% 
  select(date, hydroyear, select_vars) %>% 
  pivot_longer(bottom_DRP_ugL:tli_obs, names_to = 'variable', values_to = 'value')

################################################################################
# trend analysis and summary stats for variables


ggplot(data_long, aes(x = as.Date(date), y = value, color = variable)) +
  geom_point() +
  facet_wrap(~variable, scales = 'free') +
  theme_bw()

head(data_long)


summ_stats <- data_long %>% 
  group_by(variable) %>% 
  summarise(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            n = n(),
            trend = MannKendall(value)$tau,
            p_value = MannKendall(value)$sl)

summ_stats


write.csv(summ_stats, './figures/trend_output.csv', row.names = FALSE)
