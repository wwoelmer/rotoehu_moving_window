# plot parameters over simulation window
library(tidyverse)
library(plotly)
library(RColorBrewer)

out <- read.csv('./data/model_output_moving_window.csv')

test_vars <- c("bottom_DRP_ugL", "bottom_NH4_ugL",
               "temp_C_8", "air_temp_mean", "windspeed_min", 
               "monthly_avg_level_m", 
               "sum_alum",
               "none")

# set to NA the value in the first window with alum dosing as the parameter value is affected by the switch from 0 alum
out <- out %>% 
  mutate(value = ifelse(id_covar=='sum_alum' & start_date=='2003-03-21', NA, value))

col_pal <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#F781BF", "#999999")


out$id_covar <- factor(out$id_covar, 
                             levels = c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                                        "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                        "sum_alum"),
                             labels = c("Bottom DRP", "Bottom NH4", "Bottom Water Temp",
                                        "Mean Air Temp", "Min Windspeed", "Water Level", 
                                         "Alum Dosed"))

################################################################################
# look at parameter values
params <- out %>% 
  filter(covar %in% test_vars) %>% 
  ggplot(aes(x = as.Date(start_date), y = value, color = id_covar)) +
  geom_point() +
  scale_color_manual(values = col_pal) +
  facet_wrap(~id_covar, scales = 'free_y') +
  theme_bw() +
  xlab('Start of Iteration') +
  ylab('Parameter Value') +
  labs(color = 'Covariate') +
  theme(text=element_text(size=15),
        legend.position = 'none')

params

ggsave('./figures/figure7_parameter_time_series.png', params, dpi = 300, units = 'mm', 
       height = 400, width = 550, scale = 0.4)



out %>% 
  filter(covar %in% test_vars) %>% 
  ggplot(aes(y = value, fill = id_covar)) +
  geom_histogram() +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = col_pal) +
  facet_wrap(~id_covar, scales = 'free_y') +
  theme_bw() +
  xlab('Start of Iteration') +
  ylab('Parameter Value') +
  labs(color = 'Covariate') +
  theme(text=element_text(size=15),
        legend.position = 'none')

out_mw <- out %>% 
  filter(covar %in% test_vars) %>% 
  mutate(timeperiod = 'Moving window') %>% 
  select(id_covar, value, timeperiod, start_date)

outfull <- read.csv('./data/model_output_full.csv') 
outfull <- outfull %>% 
  filter(covar %in% test_vars) %>% 
  mutate(timeperiod = 'Full period')

outfull$id_covar <- factor(outfull$id_covar, 
                       levels = c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                                  "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                  "sum_alum"),
                       labels = c("Bottom DRP", "Bottom NH4", "Bottom Water Temp",
                                  "Mean Air Temp", "Min Windspeed", "Water Level", 
                                  "Alum Dosed"))

out_dw <- read.csv('./data/model_output_three_windows.csv')  
out_dw <- out_dw %>% 
  filter(covar %in% test_vars) %>% 
  mutate(timeperiod = 'Discrete window')
out_dw$id_covar <- factor(out_dw$id_covar, 
                           levels = c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                                      "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                      "sum_alum"),
                           labels = c("Bottom DRP", "Bottom NH4", "Bottom Water Temp",
                                      "Mean Air Temp", "Min Windspeed", "Water Level", 
                                      "Alum Dosed"))

out_all <- full_join(out_mw, outfull)
out_all <- full_join(out_all, out_dw)

out_all$timeperiod <- factor(out_all$timeperiod, levels = c('Full period',
                                                            'Discrete window',
                                                            'Moving window'))

ggplot() +
  geom_histogram(data = out_all[out_all$timeperiod=='Moving window',],
                 aes(x = value, fill = timeperiod)) +
  facet_wrap(~id_covar, scales = 'free_x') +
  geom_point(data = out_all[out_all$timeperiod!='Moving window',],
             aes(y = 0, x = value, shape = timeperiod, color = timeperiod),
             size = 3) +
  geom_vline(xintercept = 0) +
  scale_fill_manual(values = '#4575b4') +
  scale_color_manual(values = c('#d73027', '#fdae61')) +
  theme_bw() +
  ylab('Parameter Value') +
  xlab('Frequency') +
  guides(color = guide_legend(title = "Time Period", order = 1),  # Combine legends
         shape = guide_legend(title = "Time Period", order = 1),
         fill = guide_legend(title = NULL, order = 2)) +
  theme(text=element_text(size=14),
        legend.spacing = unit(0, "cm"))
