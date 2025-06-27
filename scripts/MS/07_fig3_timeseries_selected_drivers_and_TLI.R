# time series of driver data
library(tidyverse)
library(RColorBrewer)

getDecade <- function(year) {
  year <- ifelse(year<2000, year-1900, year)
  decade <- floor(year/10) * 10
  return (decade)
}


tli_dat <- read.csv('./data/processed_data/rotoehu_tli_1990_2021.csv')
tli_dat <- tli_dat %>% 
  mutate(decade = ifelse(decade==90, 1990, decade))


driver_dat <- read.csv('./data/master_rotoehu.csv')
driver_dat$decade <- getDecade(driver_dat$year)


select_vars <-  c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                  "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                  "sum_alum")

driver_long <- driver_dat %>% 
  select(c(date, decade, hydroyear, select_vars)) %>% 
  pivot_longer(bottom_DRP_ugL:sum_alum, names_to = 'variable', values_to = 'value')%>%
  mutate(decade = if_else(decade == 90, 1990, decade))%>%
  filter(!(variable == "sum_alum" & value== 0))%>%
  mutate(date = as.Date(date))%>%
  filter(date < '2021-06-14')

driver_long$variable <- factor(driver_long$variable, 
                               levels = c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                                          "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                          "sum_alum"),
                               labels = c("Bottom DRP \n(µg/L)", 
                                          "Bottom NH4 \n(µg/L)",
                                          "Bottom Water \nTemp (°C)",
                                          "Mean Air \nTemp (°C)", "Min Windspeed \n(m/s)", "Water Level (m)", 
                                          "Alum Dosed \n(L/day)"))

driver_long$decade <- factor(driver_long$decade, levels = c("1990", "2000", "2010", "2020"))



## Plots

# set themes:

driver_theme <- theme(axis.title = element_text(size = 12, colour = "black"), 
                           axis.text = element_text(size = 9), 
                           panel.grid = element_blank(), 
                           legend.position = "none",
                           axis.text.x = element_blank(), 
                           axis.ticks.x = element_blank(), 
                           plot.margin = margin(0, 0, 1, 0, "points"))

alum_theme <- theme(axis.title = element_text(size = 12, colour = "black"), 
                      axis.text = element_text(size = 9), 
                      panel.grid = element_blank(), 
                      legend.position = "none",
                      plot.margin = margin(0, 0, 1, 0, "points"))

# Drivers: 

(drp <- ggplot(driver_long%>%filter(variable == "Bottom DRP \n(µg/L)"), aes(x = date, y = value, color = as.factor(decade))) +
    geom_point(size = 1.5) +
    geom_line(size = 0.4, alpha = 0.4) +
    scale_color_manual(values = c('#D55E00', '#009E73',  '#E69F00', '#0072B2')) +
    scale_x_date(limits = c(as.Date("1990-07-25"),as.Date("2021-06-14")))+
    theme_bw() +
    labs(color = 'Decade', y = "Bottom DRP \n(µg/L)", x = NULL, tag = "B.    ") +
    driver_theme)

(nh4 <- ggplot(driver_long%>%filter(variable == "Bottom NH4 \n(µg/L)"), aes(x = date, y = value, color = as.factor(decade))) +
    geom_point(size = 1.5) +
    geom_line(size = 0.4, alpha = 0.4) +
    scale_color_manual(values = c('#D55E00', '#009E73',  '#E69F00', '#0072B2')) +
    scale_x_date(limits = c(as.Date("1990-07-25"),as.Date("2021-06-14")))+
    theme_bw() +
    labs(color = 'Decade', y = "Bottom NH4 \n(µg/L)", x = NULL) +
    driver_theme)

(water_temp <- ggplot(driver_long%>%filter(variable ==  "Bottom Water \nTemp (°C)",,), aes(x = date, y = value, color = as.factor(decade))) +
    geom_point(size = 1.5) +
    geom_line(size = 0.4, alpha = 0.4) +
    scale_color_manual(values = c('#D55E00', '#009E73',  '#E69F00', '#0072B2')) +
    scale_x_date(limits = c(as.Date("1990-07-25"),as.Date("2021-06-14")))+
    theme_bw() +
    labs(color = 'Decade', y =  "Bottom Water \nTemp (°C)", x = NULL) +
    driver_theme)

(air_temp <- ggplot(driver_long%>%filter(variable ==  "Mean Air \nTemp (°C)",), aes(x = date, y = value, color = as.factor(decade))) +
    geom_point(size = 1.5) +
    geom_line(size = 0.4, alpha = 0.4) +
    scale_color_manual(values = c('#D55E00', '#009E73',  '#E69F00', '#0072B2')) +
    scale_x_date(limits = c(as.Date("1990-07-25"),as.Date("2021-06-14")))+
    theme_bw() +
    labs(color = 'Decade', y =  "Mean Air \nTemp (°C)", x = NULL) +
    driver_theme)

(wind <- ggplot(driver_long%>%filter(variable ==  "Min Windspeed \n(m/s)"), aes(x = date, y = value, color = as.factor(decade))) +
    geom_point(size = 1.5) +
    geom_line(size = 0.4, alpha = 0.4) +
    scale_color_manual(values = c('#D55E00', '#009E73',  '#E69F00', '#0072B2')) +
    scale_x_date(limits = c(as.Date("1990-07-25"),as.Date("2021-06-14")))+
    theme_bw() +
    labs(color = 'Decade', y =  "Min Windspeed \n(m/s)", x = NULL) +
    driver_theme)

(water_level <- ggplot(driver_long%>%filter(variable ==  "Water Level (m)"), aes(x = date, y = value, color = as.factor(decade))) +
    geom_point(size = 1.5) +
    geom_line(size = 0.4, alpha = 0.4) +
    scale_color_manual(values = c('#D55E00', '#009E73',  '#E69F00', '#0072B2')) +
    scale_x_date(limits = c(as.Date("1990-07-25"),as.Date("2021-06-14")))+
    theme_bw() +
    labs(color = 'Decade', y =  "Water \nLevel (m)", x = NULL) +
    driver_theme)


(alum <- ggplot(driver_long%>%filter(variable ==  "Alum Dosed \n(L/day)"), aes(x = date, y = value, color = as.factor(decade))) +
    geom_point(size = 1.5) +
    geom_line(size = 0.4, alpha = 0.4) +
    scale_color_manual(values = c( '#E69F00', '#0072B2')) +
    scale_x_date(limits = c(as.Date("1990-07-25"),as.Date("2021-06-14")))+
    theme_bw() +
    labs(color = 'Decade', y =  "Alum Dosed \n(L/day)", x = "\nDate") +
    theme(legend.position = "none", 
          plot.margin = margin(0, 0, 1, 0, "points"))+
    alum_theme)

# Trophic level index: 

(tli <- ggplot(tli_dat, aes(x = as.Date(date), y = tli_annual)) +
    geom_point(aes(x = as.Date(date), y = tli_monthly, color = as.factor(decade)), size = 1.5 ) +
    geom_line(aes(x = as.Date(date), y = tli_monthly, color = as.factor(decade)), size = 0.4, alpha = 0.4) +
    geom_line(size = 0.5) +
    geom_hline(yintercept = c(5, 4), linetype = "dotted")+
    annotate("text", 
             x = c(as.Date("1990-12-01"), as.Date("1990-07-01"), as.Date("1990-09-01")), 
             y = c(5.8, 4.4, 3), 
             label = c("Supertrophic", "Eutrophic", "Mestrophic"), 
             size = 2.5, colour = "grey30")+
   
    theme_bw() +
    scale_color_manual(values = c('#D55E00', '#009E73',  '#E69F00', '#0072B2')) +
    labs(y = 'Trophic Level \nIndex', x = NULL, colour = "Decade", tag = "A.    ") +
    theme(legend.justification = "top" , axis.text.x = element_blank(), 
          plot.margin = margin(0, 0, 0, 0, "points"),  axis.ticks.x = element_blank(),
          axis.title = element_text(size = 12, colour = "black"), 
          axis.text = element_text(size = 9), 
          panel.grid = element_blank())+
    coord_cartesian(xlim = c(as.Date("1990-07-25"),as.Date("2021-06-14"))
                    ))


# Combine plots: 

(tli_and_drivers <- 
    tli /
    drp /
    nh4 / 
    water_temp /
    air_temp /
    wind /
    water_level /
    alum +
    plot_layout (heights = c(1.2,1,1,1,1,1,1,1)))


ggsave('./figures/MS/tli_and_drivers.png', tli_and_drivers, dpi = 300, 
       units = 'mm', height = 500, width = 350, scale = 0.6)


