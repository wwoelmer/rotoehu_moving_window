# examine residuals on the full time series, and pick a few windows (to see if the length of time series
# affects model fit)
library(tidyverse)
library(lmtest)
source('./scripts/functions/tli_fx.R')

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
dat <- dat %>% 
  group_by(month, year, lake, site) %>%
  mutate(tli_monthly = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m)) %>% 
  group_by(year, lake, site) %>%
  mutate(tli_annual = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m)) %>% 
  select(lake, site, date, tli_annual, tli_monthly, 
         chla_ugL_INT, top_TN_ugL, top_TP_ugL, secchi_m, 
         everything())

# define the variables and windows for loop
test_vars <- c("bottom_DRP_ugL", "bottom_NH4_ugL",
               "temp_C_8", "air_temp_mean", "windspeed_min", 
               "monthly_avg_level_m", 
               "sum_alum")

id_var <- "tli_monthly"

windows <- c('full', 'first_iter', 'last_iter')

# create output dataframe
out <- data.frame(driver = character(),
                  window = numeric(),
                  normality_shapirowilks = numeric(),
                  suitability_reset = numeric(),
                  stationarity_ADF = numeric(),
                  stringsAsFactors = FALSE)



# loop through drivers to check for regression statistics
for(i in 1:length(test_vars)){
  for(j in 1:length(windows)){
    print(test_vars[i])
    print(windows[j])
    
    # subset the data
    dat_ar <- dat %>% 
      ungroup() %>% 
      select(date, id_var, test_vars[i]) 
    
    if(windows[j]=='first_iter'){
      dat_ar <- dat_ar[1:100,]
        
    }else if(windows[j]=='last_iter'){
      dat_ar <- tail(dat_ar, 100)
    }
    
    # extract significant lags
    x <- as.data.frame(dat_ar[,id_var])
    par(mfrow = c(1, 1))
    pacf(x, plot = TRUE)
    acf(x, plot = TRUE)
    r <- pacf(x, plot = FALSE)$acf
    lag_save <- which(abs(r) >= qnorm(1 - 0.05 / 2) / sqrt(nrow(x))) 
    
    #### create df with the significant lags
    newcols <- paste0("tli_lag_", lag_save)
    sampdf <- data.frame(matrix(ncol = length(newcols), nrow = nrow(dat_ar)))
    colnames(sampdf) <- newcols
    newdf <- cbind(dat_ar, sampdf)
    
    for(k in 1:length(lag_save)){
      newdf[, ncol(dat_ar) + k] <- lag(dat_ar[,id_var], n = lag_save[k])
    }
    
    # remove NA's for AR modeling
    newdf <- na.omit(newdf)
    
    ##### create model with all lags and id_covar
    # setup some names
    `%notin%` <- Negate(`%in%`)
    extras <- c('date', id_var, 'lake')
    drivers_selected <- names(newdf)[names(newdf)%notin%extras]
    
    fml_selected <- as.formula(paste0(id_var, " ~ ", paste(drivers_selected, collapse= "+")))
    fml_selected
    
    # run the model and store outputs
    m1 <- lm(fml_selected, data = newdf)
    summary(m1)
    
    # conduct summary statistics on residuals
    pacf(resid(m1), main = "PACF of Residuals")
    hist(resid(m1))
    
    adf <- adf.test(resid(m1))
    adf_pvalue <- adf$p.value
    
    sw_normal <- shapiro.test(resid(m1))
    sw_normal_pvalue <- sw_normal$p.value
    
    reset <- resettest(m1, power = 2:3, type = 'fitted')
    reset_pvalue <- reset$p.value
    plot(resid(m1))
    
    out <- rbind(out,
                 data.frame(driver = test_vars[i],
                            window = windows[j],
                            normality_shapirowilks = sw_normal_pvalue,
                            suitability_reset = reset_pvalue,
                            stationarity_ADF = adf_pvalue))
    
  }
}

out

write.csv(out, './data/statistical_tests/ar_model_residual_tests.csv', row.names = FALSE)

  

  
