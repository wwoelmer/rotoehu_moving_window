# time series of driver data
#install.packages('ggpomological')
library(tidyverse)
library(RColorBrewer)

getDecade <- function(year) {
  year <- ifelse(year<2000, year-1900, year)
  decade <- floor(year/10) * 10
  return (decade)
}

# through 1990's 
data <- read.csv('./data/master_rotoehu.csv')
data$decade <- getDecade(data$year)


select_vars <-  c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                  "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                  "sum_alum")

data_long <- data %>% 
  filter(decade!=2020) %>% 
  select(date, decade, hydroyear, select_vars) %>% 
  pivot_longer(bottom_DRP_ugL:sum_alum, names_to = 'variable', values_to = 'value')

data_long$variable <- factor(data_long$variable, 
                             levels = c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                                        "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                        "sum_alum"),
                             labels = c("bottom DRP", "bottom NH4", "bottom water temp",
                                        "mean air temp", "min windspeed", "monthly water level", 
                                         "alum dosed"))

# calculate trends in variables
# Function for Mann-Kendall trend test
test_trend <- function(df) {
  library(Kendall)
  MannKendall(df$value)
}

data_split <- split(data_long, data_long$variable)
trends <- lapply(data_split, test_trend)
trends

trends_df <- lapply(names(trends), function(var){
  out <- trends[[var]]
  data.frame(variable = var,
             tau = round(out$tau, 3),
             p_value = round(out$sl, 3))
}) %>% 
  bind_rows()

trends_df
write.csv(trends_df, './figures/driver_trends.csv', row.names = FALSE)

p1 <- ggplot(data_long, aes(x = as.Date(date), y = value, color = as.factor(hydroyear))) +
  geom_point() +
  geom_line() +
  #scale_color_manual(values = c('#B40F20', '#90A959',  '#E49436')) +
  facet_wrap(~variable, ncol = 1, scales = 'free_y') +
  theme_bw() +
  labs(color = 'Decade') +
  xlab('Date') +
  ylab('value')
p1
ggplotly(p1)
ggsave('./figures/figureS7_selected_vars_decade_timeseries.png', p1, dpi = 300, 
       units = 'mm', height = 800, width = 600, scale = 0.4)
