# calculate difference between surface and bottom nutrients back to 1991
library(tidyverse)
library(readxl)
library(lubridate)

data <- read.csv('./data/master_rotoehu.csv')

data <- data %>% 
  mutate(diff_DRP = top_DRP_ugL - bottom_DRP_ugL,
         diff_NH4 = top_NH4_ugL - bottom_NH4_ugL,
         diff_temp = temp_C_1 - temp_C_8)
######
diff_nutrients <- data %>% 
  pivot_longer(diff_DRP:diff_NH4, names_to = 'variable', values_to = 'value') %>% 
  ggplot(aes(x = as.Date(date), y = value, color = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = 'free_y') +
  theme_bw() +
  xlab('Date') +
  ylab('Top - Bottom Concentration (ug/L)') +
  geom_vline(aes(xintercept = as.Date('2000-01-01')))
diff_nutrients

mean_diff <- data %>% 
  pivot_longer(diff_DRP:diff_NH4, names_to = 'variable', values_to = 'value') %>% 
  select(date, variable, value) %>% 
  group_by(variable) %>% 
  summarise(mean = mean(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE))
mean_diff  


ggsave('./figures/figureS6_diff_surface_bottom_nutrients.png', diff_nutrients,
       dpi = 300, units = 'mm', height = 300, width = 600, scale = 0.3)

diff_temp <- data %>% 
  ggplot(aes(x = as.Date(date), y = diff_temp)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  xlab('Date') +
  ylab('Surface - Bottom Water Temperature (C)') +
  geom_vline(aes(xintercept = as.Date('2000-01-01')))

diff_temp
ggplotly(diff_temp)
ggsave('./figures/figureS8_diff_surface_bottom_temperature.png', diff_temp,
       dpi = 300, units = 'mm', height = 300, width = 600, scale = 0.3)
