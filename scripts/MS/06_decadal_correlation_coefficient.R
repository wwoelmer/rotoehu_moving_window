# calculate correlation coefficients by decade
#install.packages('Hmisc')
#install.packages('ggthemes')
#install.packages('ggpomological')

library(tidyverse)
library(Hmisc)
library(ggthemes)
library(RColorBrewer)
library(plotly)


getDecade <- function(year) {
  year <- ifelse(year<2000, year-1900, year)
  decade <- floor(year/10) * 10
  return (decade)
}


df <- read.csv('./data/master_rotoehu.csv')
df$decade <- getDecade(df$year)

# get rid of double observations in a given month
df <- df %>% 
  distinct(year, month, .keep_all = TRUE)

################################################################################
# calculate TLI

source('./scripts/functions/tli_fx.R')

# calculate each month's tli and each year's TLI (annual TLI is the mean of variable then TLI calculation, done within function)
df <- df %>% 
  group_by(hydroyear) %>% 
  mutate(tli_annual = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m, timescale = 'annual')) %>% 
  mutate(month = month(date)) %>% 
  group_by(hydroyear, month) %>% 
  mutate(tli_monthly = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m, timescale = 'monthly')) %>% 
  filter(!is.na(tli_monthly))


p1 <- ggplot(df, aes(x = as.Date(date), y = tli_annual)) +
  geom_point(aes(x = as.Date(date), y = tli_monthly, color = as.factor(hydroyear)), size = 2) +
  geom_line(aes(x = as.Date(date), y = tli_monthly, color = as.factor(hydroyear))) +
  geom_line(size = 1.5) +
  theme_bw() +
  xlab('Date') +
  ylab('Trophic Level Index') +
  labs(color = 'Year',
       linetype = '') +
  theme(text = element_text(size = 14)) 
p1

##################################################################################
# some data cleanup

# remove some variables that have lots of NA's or no clear mechanism
df <- df %>% 
  select(-c(avg_level_m, top_pH, bottom_pH, 
            air_pressure_min, air_pressure_mean, air_pressure_max,
            shortwave_min, shortwave_mean, shortwave_max))
df <- df %>% 
  select(-(area_pct_native_forest:area_ha_water),
         -(area_pct_aq_vegetation:area_pct_manuka))

# CTD data for chl, PAR, turbidity, etc. don't start until 2003 so need to remove them for the decadal analysis
df <- df %>% 
  select(-(DO_mgL_1:turbidity_ntu_1)) %>% 
  select(-chla_ugL_8, -chla_ugL_1)

# remove surface soluble nutrients because they are too strongly correlated with the TLI variables
df <- df %>% 
  select(-c(top_DRP_ugL, top_NH4_ugL, top_NO3_ugL, bottom_TN_ugL, bottom_TP_ugL)) %>% 
  filter(hydroyear <= 2021)

df_long <- df %>% 
  ungroup() %>% 
  select(lake, site, date, decade, tli_annual, tli_monthly, everything(), 
         -year, -month, - hydroyear, -soi_phase) %>%
  filter(decade <2020) %>% 
  pivot_longer(chla_ugL_INT:de_trended_temp_anomaly, names_to = 'variable', values_to = 'value')

ggplot(df_long, aes(x = as.factor(decade), y = value, fill = as.factor(decade))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = 'free') +
  theme_bw() +
  xlab('Decade') +
  labs(fill = 'Decade')


#################################################################################
# plot correlations

decades <- c('90', '2000', '2010', 'all')
vars_out <- data.frame(variable = NA,
                       value = NA,
                       decade = NA)

df <- df %>% 
  select(tli_monthly, everything())

for(i in 1:length(decades)){
  if(decades[i]!='all'){
    cor_df <- df %>% 
      ungroup() %>% 
      filter(decade==decades[i]) %>% 
      dplyr::select(-c(date, hydroyear, month, tli_annual, decade, 
                year, soi_phase, top_TP_ugL, top_TN_ugL, chla_ugL_INT, secchi_m,
                de_trended_temp_anomaly, lake, site))
  }else{
    cor_df <- df %>% 
      ungroup() %>% 
      select(-c(date, hydroyear, month, tli_annual, decade, 
                year, soi_phase, top_TP_ugL, top_TN_ugL, chla_ugL_INT, secchi_m,
                de_trended_temp_anomaly, lake, site))
  }
  print(decades[i])
  print(nrow(cor_df))
  
  cor_df <- na.omit(cor_df)
  cor_out <- rcorr(as.matrix(cor_df)) # default is pearson
  try(corrplot::corrplot(cor_out$r, type = 'upper',
                         sig.level = 0.01, insig = 'blank', p.mat = cor_out$P,  
                         main = paste0('Decade: ', decades[i], "'s")), silent = TRUE)
  
  vars <- cor_out$r[cor_out$P > 0.05]
  
  
  r <- as.data.frame(cor_out$r)[1,]
  r <- r %>% 
    mutate_all(~ ifelse(is.nan(.), 0, .))
  
  p <- as.data.frame(cor_out$P)[1,]
  p <- p %>% 
    mutate_all(~ ifelse(is.nan(.), 0, .))
  
  r_sub <- r
  for(j in 2:ncol(r_sub)){
    if(p[,j]>=0.05){
      r_sub[,j] <- NA
    }
  }
  
  r_long <- r_sub %>% 
    pivot_longer(tli_monthly:soi_3mth_mean, names_to = 'variable', values_to = 'value')
  r_long$decade <- decades[i]
  vars_out <- rbind(vars_out, r_long)
  
  write.csv(cor_out$r, paste0('./data/processed_data/correlation_matrices/correlation_matrix_Rotoehu_', decades[i], 's.csv'), row.names = FALSE)
  
}

vars_out <- na.omit(vars_out)
vars_out <- vars_out %>% 
  filter(variable!='tli_monthly')
vars_out$decade <- factor(vars_out$decade, levels = c('90', '2000', '2010', 'all'))

# remove min and max variables since they are similar to mean
`%notin%` <- Negate(`%in%`)
vars_remove <- c('air_temp_max', 'air_temp_min', 'longwave_max', 'longwave_min', 
                 'windspeed_mean', 'windspeed_max', 'DO_mgL_8', 'temp_C_1')
vars_out <- vars_out %>% 
  filter(variable %notin% vars_remove)

col_pal <- colorRampPalette(brewer.pal(9, "Paired"))(13)

vars_select <- vars_out %>% 
  filter(value > 0.3 | value < -0.3)  
vars_select$variable <- factor(vars_select$variable, 
                               levels = c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                                          "air_temp_mean", "windspeed_min", "monthly_avg_level_m"),
                               labels = c("bottom DRP", "bottom NH4", "bottom water temp",
                                          "mean air temp", "min windspeed", "monthly water level"))

p1 <- ggplot(vars_select, aes(x = decade, y = value, fill = variable)) +
  geom_col(position = 'dodge') +
  scale_fill_calc() +
  #scale_fill_brewer(palette = 'Spectral') +
 # scale_fill_manual(values = col_pal) +
  geom_vline(xintercept = 1.5) +
  geom_vline(xintercept = 2.5) +
  geom_vline(xintercept = 3.5) +
  theme_bw() +
  ylab('Correlation Coefficient') 
p1
ggsave('./figures/figS3_corr_by_decade.png', p1, dpi = 300, units = 'mm', height = 200, width = 500, scale = 0.4)

################################################################################
vars_sig <- vars_out %>% 
  filter(value > 0.3 | value < -0.3) 
vars_plot <- unique(vars_sig$variable)

df_vars_sig <- df_long %>% 
  filter(variable %in% vars_plot) 
  
df_vars_sig$variable <- factor(df_vars_sig$variable, 
                               levels = c("DRP_mgm3", "NH4_mgm3", "temp_8",
                                          "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                          "schmidt_stability"),
                               labels = c("bottom DRP", "bottom NH4", "bottom water temp",
                                          "mean air temp", "min windspeed", "monthly water level", 
                                          "schmidt stability"))

p2 <- ggplot(df_vars_sig, aes(x = as.factor(decade), y = value, fill = as.factor(decade))) +
  geom_boxplot() +
  scale_fill_pomological() +
  facet_wrap(~variable, scales = 'free') +
  theme_bw() +
  xlab('Decade') +
  labs(fill = 'Decade')
p2
ggsave('./figures/1991_2021_analysis/selected_vars_decade_boxplots.png', p2, dpi = 300, units = 'mm', 
       height = 300, width = 500, scale = 0.4)

df_summary <- df_long %>% 
  select(variable, value, decade) %>% 
  group_by(variable, decade) %>% 
  reframe(mean = round(mean(value, na.rm = TRUE), 2), min = min(value, na.rm = TRUE), 
          max= max(value, na.rm = TRUE), range = min - max, n = n())

# what months were sampled in 90's
ggplot(df_long, aes(y = month)) +
  geom_histogram() +
  facet_wrap(~decade) +
  theme_bw() 

df_long %>% 
  filter(variable=='windspeed_min') %>% 
ggplot(aes(x = month, y = value)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~decade) +
  theme_bw() 
