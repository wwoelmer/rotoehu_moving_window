# plot parameters over simulation window
library(tidyverse)
library(plotly)
library(RColorBrewer)

out <- read.csv('./data/processed_data/moving_window/model_output.csv')

test_vars <- c("bottom_DRP_ugL", "bottom_NH4_ugL",
               "temp_C_8", "air_temp_mean", "windspeed_min", 
               "monthly_avg_level_m", 
               "schmidt_stability", 
               "sum_alum",
               "none")

col_no <- length(unique(out$id_covar))
col_pal <- colorRampPalette(brewer.pal(9, "Set1"))(col_no)

out$id_covar <- factor(out$id_covar, 
                             levels = c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                                        "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                        "schmidt_stability", "sum_alum"),
                             labels = c("Bottom DRP (µg/L)", "Bottom NH4 (µg/L)", "Bottom Water Temp (°C)",
                                        "Mean Air Temp (°C)", "Min Windspeed (m2/s)", "Water Level (m)", 
                                        "Schmidt Stability (J/m2)", "Alum Dosed (L/day)"))
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

ggsave('./figures/moving_window/MS/fig7_parameter_time_series.png', params, dpi = 300, units = 'mm', 
       height = 400, width = 550, scale = 0.4)
