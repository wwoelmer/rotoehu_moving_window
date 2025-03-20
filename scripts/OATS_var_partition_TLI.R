# partition the contribution of each TLI component to the change in the TLI
library(tidyverse)
library(plotly)
source('./scripts/functions/tli_fx.R')

data <- read.csv('./data/master_rotoehu.csv')
data <- data %>% 
  select(date:secchi_m)

# calculate TLI
tli_obs <- data %>% 
  group_by(date) %>% 
  mutate(tli_obs = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m)) %>% 
  ungroup() %>% 
  group_by(hydroyear) %>% 
  mutate(tli_annual = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m)) 
  

data_long <- data %>% 
  pivot_longer(chla_ugL_INT:secchi_m, names_to = 'tli_var', values_to = 'value')

ggplot(data_long, aes(x = as.Date(date), y = value, color = tli_var)) +
  geom_point() +
  facet_wrap(~tli_var, scales = 'free') +
  theme_bw()

# subset to 2021 when the study ends


#window_length <- 100
#n_iter <- seq(1, nrow(data) - window_length)
vars <- c('chla_ugL_INT', 'top_TN_ugL', 'top_TP_ugL', 'secchi_m')
n_years <- unique(data$hydroyear)

out <- data.frame()

for(i in 1:length(n_years)){ #n_iter
  #start <- i
  #end <- i + window_length
  #dat_sub <- data[start:end,]
  dat_sub <- data %>% 
    filter(hydroyear==n_years[i])

  
  for(j in 1:length(vars)){
    
    col_name <- vars[j]  # Get the column name
    tli <- dat_sub %>%
      #mutate(!!col_name := mean(.data[[col_name]], na.rm = TRUE)) %>% 
      mutate(across(.cols = all_of(vars[vars != col_name]), # Select only columns in vars excluding current_col
        ~ mean(.x, na.rm = TRUE))) %>% 
      mutate(mean_variable = vars[j])
    
    tli <- tli %>% 
      group_by(date) %>% 
      mutate(tli_mean = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m)) 
    print(head(tli))
    
    # calculate the variance due to taking the mean, equivalent to the 'sensitivity' of the TLI to this variable?
    tli <- tli %>% 
      mutate(tli_mean = ifelse(is.nan(tli_mean), NA, tli_mean)) 
    tli$variance_mean <- var(tli$tli_mean, na.rm = TRUE)
    
    # some columns to identify the windows
   # tli$window_num <- start
  #  tli$start_date <- min(dat_sub$date)

    
    out <- rbind(out, tli)
      
  }
}


ggplot(out, aes(x = as.Date(date), y = variance_mean, color = mean_variable))+
  geom_point()

# calculate the proportion of variance
out_prop <- out %>% 
  ungroup() %>% 
  distinct(hydroyear, mean_variable, .keep_all = TRUE) %>% 
  group_by(hydroyear) %>% 
  mutate(total_var = sum(variance_mean, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(hydroyear, mean_variable) %>% 
  mutate(prop_var = variance_mean/total_var)

ggplot(out_prop, aes(x = as.Date(date), y = prop_var, color = mean_variable))+
  geom_line(size = 2)


ggplotly(ggplot(out_prop, aes(x = as.Date(date), y = prop_var, fill = mean_variable))+
  geom_area())

out_prop <- out_prop %>% 
  group_by(hydroyear) %>% 
  mutate(sum_prop = sum(prop_var))

ggplotly(ggplot(out_prop, aes(x = year, y = prop_var, fill = mean_variable)) +
  geom_bar(stat = 'identity', position = 'dodge'))

## combine with annual TLI estimates
tli_annual <- tli_obs %>% 
  mutate(tli_obs= ifelse(is.nan(tli_obs), NA, tli_obs)) %>% 
  group_by(hydroyear) %>% 
  mutate(variance_obs = var(tli_obs, na.rm = TRUE)) %>% 
  distinct(hydroyear, .keep_all = TRUE) %>% 
  select(hydroyear, tli_annual, variance_obs)

out_prop <- left_join(out_prop, tli_annual)

# fix the year where secchi is NA, set to 0
out_prop <- out_prop %>% 
  mutate(prop_var = ifelse(is.na(prop_var), 0, prop_var))

ggplot(out_prop, aes(x = as.Date(date), y = prop_var, fill = mean_variable))+
  geom_area() +
  geom_line(aes(x = as.Date(date), y = tli_annual/6)) +
  scale_y_continuous(name = "Proportion of Variance",
                     sec.axis = sec_axis(~ .*6, name = "Annual TLI"))

ggplot(out_prop, aes(x = prop_var, y = tli_annual, color = mean_variable)) +
  geom_point() +
  facet_wrap(~mean_variable)

ggplot(out_prop, aes(x = variance_mean, y = variance_obs, color = mean_variable)) +
  geom_point() +
  facet_wrap(~mean_variable)

ggplot(out_prop, aes(x = tli_mean, y = tli_annual, color = mean_variable)) +
  geom_point() +
  facet_wrap(~mean_variable) +
  geom_smooth(method = 'lm') +
  geom_abline(slope = 1, intercept = 0)

ggplot(out, aes(x = as.Date(date), y = tli_obs - tli_mean, color = mean_variable)) +
  geom_point() +
  facet_wrap(~mean_variable)

ggplot(out, aes(x = as.Date(date), y = tli_mean, color = mean_variable)) +
  geom_point() +
  #geom_point(aes(x = as.Date(date), y = tli_obs, color = 'black')) +
  facet_wrap(~mean_variable)

ggplot(out, aes(x = as.Date(date), y = tli_obs)) +
  geom_point() +
  geom_line()
