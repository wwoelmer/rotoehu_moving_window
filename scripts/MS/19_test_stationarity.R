library(tidyverse)
#install.packages('tseries')
library(tseries)

################################################################################
# test whether TLI and selected drivers are stationary using Augmented Dickey-Fuller 
# and Kwiatkowski-Phillips-Schmidt Shin test

# read in data
dat <- read.csv('./data/master_rotoehu.csv')

# set zeroes to NA for alum
dat <- dat %>% 
  mutate(sum_alum = ifelse(is.na(sum_alum), 0, sum_alum)) %>% 
  filter(date > as.Date('2000-07-01')&
           date < as.Date ('2021-07-01')) %>% 
  select(-avg_level_m) %>% 
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

test_vars <- c("bottom_DRP_ugL", "bottom_NH4_ugL",
               "temp_C_8", "air_temp_mean", "windspeed_min", 
               "monthly_avg_level_m", 
               "sum_alum")

dat <- dat %>% 
  ungroup() %>% 
  select(tli_monthly, test_vars)

adf_results <- lapply(dat, adf.test)
adf_pvalue <- sapply(adf_results, function(x) x$p.value)
adf_pvalue <- pivot_longer()

kpss_results <- lapply(dat, kpss.test)
kpss_pvalue <- sapply(kpss_results, function(x) x$p.value)

pvals <- data.frame(variable = names(adf_pvalue),
                    adf_pvalue = adf_pvalue,
                    kpss_pvalue = kpss_pvalue[names(adf_pvalue)])
pvals

pvals$stationarity <-  with(pvals, ifelse(
  adf_pvalue < 0.05 & kpss_pvalue > 0.05, "stationary",
  ifelse(adf_pvalue > 0.05 & kpss_pvalue < 0.05, "non-stationary", "inconclusive")
))
pvals

write.csv(pvals,
          './data/statistical_tests/driver_tli_observations_stationarity.csv', 
          row.names = FALSE)

################################################################################
# test whether the AIC between TLI and drivers passes these tests

out <- read.csv('./data/model_output_moving_window.csv') %>% 
  select(id_covar, start_date, aic, r2)

out <- out %>% 
  distinct(id_covar, start_date, .keep_all = TRUE)

# Define a function that runs ADF and KPSS on a vector and returns p-values
get_stationarity <- function(x) {
  adf_p <- tryCatch(adf.test(x)$p.value, error = function(e) NA)
  kpss_p <- tryCatch(kpss.test(x)$p.value, error = function(e) NA)
  data.frame(adf_p = adf_p, kpss_p = kpss_p)
}

# Apply across groups
stationarity_df <- out %>%
  group_by(id_covar) %>%
  summarise(get_stationarity(aic), .groups = "drop") %>%
  mutate(
    stationarity = case_when(
      adf_p < 0.05 & kpss_p > 0.05 ~ "stationary",
      adf_p > 0.05 & kpss_p < 0.05 ~ "non-stationary",
      TRUE ~ "inconclusive"
    )
  )
stationarity_df

write.csv(stationarity_df,
          './data/statistical_tests/driver_tli_relationship_stationarity.csv', 
          row.names = FALSE)
