# plot parameters over simulation window
library(tidyverse)
library(plotly)
library(RColorBrewer)

out <- read.csv('./data/model_output.csv')

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
ggplotly(params)

ggsave('./figures/figure7_parameter_time_series.png', params, dpi = 300, units = 'mm', 
       height = 400, width = 550, scale = 0.4)
