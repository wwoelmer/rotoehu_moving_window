# plot parameters over simulation window
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(ggpubr)

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
                       labels = c("Bottom DRP", "Bottom NH4", "Bottom water temp",
                                  "Mean air temp", "Min windspeed", "Water level", 
                                  "Alum dosed"))

################################################################################
# look at parameter values

out_mw <- out %>% 
  filter(covar %in% test_vars) %>% 
  mutate(timeperiod = 'Moving window') %>% 
  select(id_covar, value, timeperiod, start_date, std_error)

outfull <- read.csv('./data/model_output_full.csv') 
outfull <- outfull %>% 
  filter(covar %in% test_vars) %>% 
  mutate(timeperiod = 'Full period')

outfull$id_covar <- factor(outfull$id_covar, 
                       levels = c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                                  "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                  "sum_alum"),
                       labels = c("Bottom DRP", "Bottom NH4", "Bottom water temp",
                                  "Mean air temp", "Min windspeed", "Water level", 
                                  "Alum dosed"))

out_dw <- read.csv('./data/model_output_three_windows.csv')  
out_dw <- out_dw %>% 
  filter(covar %in% test_vars) %>% 
  mutate(timeperiod = 'Discrete window')

out_dw$id_covar <- factor(out_dw$id_covar, 
                           levels = c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                                      "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                      "sum_alum"),
                          labels = c("Bottom DRP", "Bottom NH4", "Bottom water temp",
                                     "Mean air temp", "Min windspeed", "Water level", 
                                     "Alum dosed"))

out_all <- full_join(out_mw, outfull)
out_all <- full_join(out_all, out_dw)

out_all$timeperiod <- factor(out_all$timeperiod, 
                             levels = c("Full period", "Discrete window", "Moving window"), 
                             labels = c("Full", "Discrete", "Moving"))

# make dataframe of direction labels:
hyp_dir_labels <- data.frame(id_covar = factor(c("Bottom DRP", "Bottom NH4", "Bottom water temp",
                                                 "Mean air temp", "Min windspeed", "Water level", 
                                                 "Alum dosed")) , 
                             direction = c(" +", " +", "+/-", " +", "+/-", " -", " -"))


(p1 <-  ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, colour = "grey20")+
    geom_point(data = out_all,
               aes(y = value, x = as.Date(start_date), fill = timeperiod, colour = timeperiod, size = timeperiod, alpha = timeperiod)) +
    facet_wrap(~id_covar, scales = 'free_y', ncol = 1) +
    scale_fill_manual(values = c('#d73027', '#fdae61', '#4575b4'), guide = 'none')+
    scale_colour_manual(values = c('#d73027', '#fdae61', '#4575b4'))+
    scale_size_manual (values = c(3,3,2),  guide = 'none')+
    scale_alpha_manual(values = c(1, 1, 0.7), guide = 'none')+
    labs(x = "\nWindow start date", y = "Parameter value") +
     geom_text(
      data    = hyp_dir_labels,
      mapping = aes(x = as.Date("2015-04-01"), y = Inf, label = direction), 
      hjust = 0.1, vjust = 1.5)+
    coord_cartesian(xlim = c(as.Date("2000-07-11"),as.Date("2014-06-01")), clip = "off")+
    theme_bw() +
    theme(panel.grid = element_blank(), 
          strip.background = element_blank(), 
          strip.text = element_text(size = 12, colour = "black", hjust = 0),
          axis.title = element_text(size = 12, colour = "black"),
          axis.text = element_text(size = 9), 
          legend.justification = "bottom", 
          panel.spacing = unit(0,'lines'), 
          legend.box.margin = margin(t = 0, r = 0, b = 0, l = 10))+  
    guides(color = guide_legend(title = "Window type", order = 1, hjust = 0.5)
    ))

ggsave('./figures/MS/Parameter coefficient timeseries.png', p1,
       dpi = 300, units = 'mm', height = 500, width = 350, scale = 0.5)



param_table <- out_all %>% 
  select(id_covar, timeperiod, start_date, value, std_error)
param_table_wide <- param_table %>% 
  pivot_wider(names_from = timeperiod, values_from = c('value', 'std_error'))

colnames(param_table_wide) <- c('Variable', 'Window start date',
                                'Paramater MW', 'Parameter full', 'Parameter DW',
                                'Std error MW', 'Std error full', 'Std error DW')

mean_params <- param_table %>% 
  group_by(id_covar, timeperiod) %>% 
  summarise(mean_param = mean(value, na.rm = TRUE))

a <- ggplot(param_table, aes(x = as.Date(start_date), y = std_error, color = timeperiod)) +
  geom_point() +
  facet_wrap(~id_covar, scales = 'free') +
  scale_color_manual(values = c('#d73027', '#fdae61', '#4575b4')) +
  theme_bw() +
  xlab('Window start date') +
  ylab('Parameter standard error')


ggsave('./figures/MS/si_figs/parameter_std_error.png', a,
       dpi = 300, units = 'mm', height = 500, width = 350, scale = 0.5)


# New plot format - RGG ----



out_all_rgg <- out_all %>%
   mutate(direction = if_else(value >=0, "Positive", "Negative"))%>%
  filter(!is.na(direction))%>%
  mutate(timeperiod2 = factor(timeperiod, levels = c("Discrete window", "Full period", "Moving window"), 
                              labels = c("Discrete", "Full", "Moving")))




