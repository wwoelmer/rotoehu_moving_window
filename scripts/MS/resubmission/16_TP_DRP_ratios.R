# plot TP:DRP over time
library(tidyverse)

data <- read.csv('./data/master_rotoehu.csv')

data <- data %>% 
  select(date, year, month, hydroyear, top_TP_ugL, top_DRP_ugL, bottom_DRP_ugL, bottom_TP_ugL)

## add a minimum value for days when DRP is 0 to avoid INF
min_DRP <- data %>% 
  filter(top_DRP_ugL > 0) %>%
  summarise(min_value = min(top_DRP_ugL)) %>%
  pull(min_value)
min_DRP

data <- data %>% 
  mutate(top_DRP_ugL = top_DRP_ugL + min_DRP)

data <- data %>% 
  mutate(top_TP_DRP = top_TP_ugL/top_DRP_ugL)

ggplot(data, aes(x = as.Date(date), y = top_TP_DRP)) +
  geom_point(size = 2) +
  theme_bw() +
  ylab('TP:DRP') +
  xlab('Date')
