# calculate delta R2 and delta aic and plot across all time periods
library(tidyverse)
library(plotly)
library(ggpubr)

out <- read.csv('./data/model_output_moving_window.csv')

# set up labels and levels of factor
out$id_covar <- factor(out$id_covar, 
                       levels = c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                                  "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                  "sum_alum", "none"),
                       labels = c("Bottom DRP", "Bottom NH4", "Bottom Water Temp",
                                  "Mean Air Temp", "Min Windspeed", "Water Level", 
                                  "Alum Dosed", "None"))

# calculate the difference across variables
out_prop <- out %>% 
  distinct(id_covar, iter_start, .keep_all = TRUE) %>% 
  select(id_covar:iter_end, start_date, end_date, r2, p_value) %>% 
  group_by(iter_start) %>% 
  mutate(diff_from_best = max(r2) - r2,
         rank = dense_rank(desc(r2)),
         r2_none = r2[id_covar=='None'],
         diff_from_none = r2 - r2_none,
         rank_AR = dense_rank(desc(diff_from_none)),
         aic_none = aic[id_covar=='None'],
         diff_from_none_aic = aic - aic_none,
         rank_aic = dense_rank(desc(diff_from_none_aic)),
         significant = p_value < 0.05)

# create column for time period
out_prop$timeperiod <- 'Moving window'

delta_aic <- ggplot(out_prop, aes(x = as.Date(start_date), y = diff_from_none_aic)) +
  geom_rect(aes(xmin = min(as.Date(start_date)), xmax = max(as.Date(start_date)), 
                ymin = -2, ymax = 2), alpha = 0.8, fill = "grey") +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(color =  significant)) +
  facet_wrap(~id_covar) +
  theme_bw() +
  ylab(expression(Delta~AIC[c])) +
  xlab('Start of Iteration') +
  theme(text=element_text(size=14)) +
  #scale_color_manual(values = col_pal) +
  scale_color_manual(values = c('#6C5379', '#5FAD56')) 
delta_aic
ggplotly(delta_aic)
################################################################################
# repeat for three discrete window
out_three <- read.csv('./data/model_output_three_windows.csv')

out_three$id_covar <- factor(out_three$id_covar, 
                             levels = c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                                        "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                        "sum_alum", "none"),
                             labels = c("Bottom DRP", "Bottom NH4", "Bottom Water Temp",
                                        "Mean Air Temp", "Min Windspeed", "Water Level", 
                                        "Alum Dosed", "None"))

# calculate the difference across variables
out_prop3 <- out_three %>% 
  distinct(id_covar, start_date, .keep_all = TRUE) %>% 
  select(id_covar:iter_end, start_date, end_date, r2, p_value) %>% 
  group_by(start_date) %>% 
  mutate(diff_from_best = max(r2) - r2,
         rank = dense_rank(desc(r2)),
         r2_none = r2[id_covar=='None'],
         diff_from_none = r2 - r2_none,
         rank_AR = dense_rank(desc(diff_from_none)),
         aic_none = aic[id_covar=='None'],
         diff_from_none_aic = aic - aic_none,
         rank_aic = dense_rank(desc(diff_from_none_aic)),
         significant = p_value < 0.05)

# create column for time period
out_prop3$timeperiod <- 'Discrete window'

delta_aic3 <- ggplot(out_prop3, aes(x = as.factor(year(start_date)), y = diff_from_none_aic)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, 
                ymin = -2, ymax = 2), alpha = 0.8, fill = "grey") +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(color =  significant), size = 3) +
  facet_wrap(~id_covar) +
  theme_bw() +
  ylim(-20, 3) +
  ylab(expression(Delta~AIC[c])) +
  xlab('Window start date') +
  theme(text=element_text(size=14),
        #axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  #scale_color_manual(values = col_pal) +
  scale_color_manual(values = c('#6C5379', '#5FAD56')) 
delta_aic3

################################################################################
# repeat for full time series
out_full <- read.csv('./data/model_output_full.csv')

out_full$id_covar <- factor(out_full$id_covar, 
                            levels = c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                                       "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                       "sum_alum", "none"),
                            labels = c("Bottom DRP", "Bottom NH4", "Bottom Water Temp",
                                       "Mean Air Temp", "Min Windspeed", "Water Level", 
                                       "Alum Dosed", "None"))

# calculate the difference across variables
out_prop_full <- out_full %>% 
  distinct(id_covar, start_date, .keep_all = TRUE) %>% 
  group_by(start_date) %>% 
  mutate(diff_from_best = max(r2) - r2,
         rank = dense_rank(desc(r2)),
         r2_none = r2[id_covar=='None'],
         diff_from_none = r2 - r2_none,
         rank_AR = dense_rank(desc(diff_from_none)),
         aic_none = aic[id_covar=='None'],
         diff_from_none_aic = aic - aic_none,
         rank_aic = dense_rank(desc(diff_from_none_aic)),
         significant = p_value < 0.05)

# create column for time period
out_prop_full$timeperiod <- 'Full window'


delta_aic_full <- ggplot(out_prop_full, aes(x = (start_date), y = diff_from_none_aic)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, 
                ymin = -2, ymax = 2), alpha = 0.8, fill = "grey") +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(color =  significant), size = 3) +
  facet_wrap(~id_covar) +
  theme_bw() +
  ylab(expression(Delta~AIC[c])) +
  xlab('Window start date') +
  ylim(-20, 2) +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  #scale_color_manual(values = col_pal) +
  scale_color_manual(values = c('#6C5379', '#5FAD56')) 
#scale_color_manual(values = c('#08415C', '#EFCB68')) 
delta_aic_full

p_si <- ggarrange(delta_aic_full, delta_aic3, common.legend = TRUE, 
          nrow = 1, #widths = c(1.5, 3, 3),
          labels = 'auto')
p_si

ggsave('./figures/resubmission/si_figs/full_discrete_signif_aic.png', p_si,
       dpi = 300, units = 'mm', height = 300, width = 400, scale = 0.6)

################################################################################
# combine so they can all be plotted together
out_prop_all <- full_join(out_prop, out_prop3)
out_prop_all <- full_join(out_prop_all, out_prop_full)


p1 <- ggplot() +
  geom_rect(data = out_prop_all[out_prop_all$timeperiod=='Moving window' 
                                & out_prop_all$id_covar!='None',],
            aes(xmin = min(as.Date(start_date)), xmax = max(as.Date(end_date)), 
                ymin = -2, ymax = 2), alpha = 0.8, fill = "grey") +
  geom_segment(data = out_prop_all[out_prop_all$timeperiod=='Moving window'
                                   & out_prop_all$id_covar!='None',],
               aes(x = as.Date(start_date), xend = as.Date(end_date),
                   y = diff_from_none_aic, yend = diff_from_none_aic,
                   color = timeperiod)) +
  geom_segment(data = out_prop_all[out_prop_all$timeperiod!='Moving window'
                                   & out_prop_all$id_covar!='None',],
            aes(x = as.Date(start_date), xend = as.Date(end_date),
                y = diff_from_none_aic, yend = diff_from_none_aic,
                color = timeperiod), linewidth = 2, alpha = 0.9) +
  geom_hline(yintercept = 0) +
  facet_wrap(~id_covar, scales = 'free') +
  theme_bw() +
  ylab(expression(Delta~AIC[c])) +
  xlab('Date') +
  theme(text=element_text(size=14)) +
  #scale_color_manual(values = col_pal) +
  scale_color_manual(values = c('#fdae61','#d73027', '#4575b4')) 

p1

ggsave('./figures/resubmission/all_windows_delta_aic.png', p1,
       dpi = 300, units = 'mm', height = 300, width = 400, scale = 0.6)

p2 <- ggplot(out_prop_all, aes(x = as.Date(start_date), y = diff_from_none_aic)) +
  geom_rect(data = out_prop_all[out_prop_all$timeperiod=='Moving window',],
            aes(xmin = min(as.Date(start_date)), xmax = max(as.Date(start_date)), 
                ymin = -2, ymax = 2), alpha = 0.8, fill = "grey") +
  geom_point(aes(color = significant)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~id_covar, scales = 'free') +
  theme_bw() +
  ylab(expression(Delta~AIC[c])) +
  xlab('Start of Iteration') +
  theme(text=element_text(size=14)) +
  #scale_color_manual(values = col_pal) +
  scale_color_manual(values = c('#6C5379', '#5FAD56')) 

p2

ggsave('./figures/resubmission/movingwindow_delta_aic_significance.png', p2,
       dpi = 300, units = 'mm', height = 300, width = 400, scale = 0.6)
