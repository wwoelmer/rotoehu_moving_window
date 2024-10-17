# calculate delta R2 and delta aic
library(tidyverse)
library(plotly)

out <- read.csv('./data/model_output.csv')
################################################################################
# set up labels and levels of factor

out$id_covar <- factor(out$id_covar, 
                       levels = c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                                  "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                  "sum_alum", "none"),
                       labels = c("Bottom DRP", "Bottom NH4", "Bottom Water Temp",
                                  "Mean Air Temp", "Min Windspeed", "Water Level", 
                                   "Alum Dosed", "None"))

# update color pal so 'none' is the grey color
col_pal <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#F781BF", "#999999")
################################################################################
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

ggplot(out_prop, aes(x = as.Date(start_date), y = diff_from_none_aic)) +
  geom_rect(aes(xmin = min(as.Date(start_date)), xmax = max(as.Date(start_date)), 
                ymin = -2, ymax = 2), alpha = 0.8, fill = "grey") +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(color = as.factor(id_covar), shape = significant)) +
  #facet_wrap(~id_covar) +
  theme_bw() +
  ylab(expression(Delta~AIC[c])) +
  xlab('Start of Iteration') +
  theme(text=element_text(size=18)) +
  scale_color_manual(values = col_pal) +
  labs(color = 'Driver')

delta_aic <- ggplot(out_prop, aes(x = as.Date(start_date), y = diff_from_none_aic)) +
  geom_rect(aes(xmin = min(as.Date(start_date)), xmax = max(as.Date(start_date)), 
                ymin = -2, ymax = 2), alpha = 0.8, fill = "grey") +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(color =  significant)) +
  facet_wrap(~id_covar) +
  theme_bw() +
  ylab(expression(Delta~AIC[c])) +
  xlab('Start of Iteration') +
  theme(text=element_text(size=18)) +
  #scale_color_manual(values = col_pal) +
  scale_color_manual(values = c('#6C5379', '#5FAD56')) 
  #scale_color_manual(values = c('#08415C', '#EFCB68')) 
delta_aic
ggplotly(delta_aic)

ggsave('./figures/figure5_delta_aic_significance.png', delta_aic,
       dpi = 300, units = 'mm', height = 300, width = 400, scale = 0.6)

#######################################################################################
### these might be needed as SI figs
#######################################################################################
diff_r2 <- ggplot(out_prop, aes(x = as.Date(start_date), y = diff_from_best, color = id_covar)) +
  geom_point(size = 2) +
  scale_color_manual(values = col_pal) +
  theme_bw() +
  ylab('Difference from Best Performing Model') +
  xlab('Start of Iteration') +
  labs(color = 'Driver') +
  theme(text=element_text(size=18))

ggplotly(diff_r2)
ggsave('./figures/moving_window/diff_from_best_r2_timeseries.png', diff_r2,
       dpi = 300, units = 'mm', height = 300, width = 600, scale = 0.5)

diff_r2_panels <- ggplot(out_prop, aes(x = as.Date(start_date), y = as.factor(rank), color = as.factor(id_covar))) +
  geom_point() +
  facet_wrap(~id_covar) +
  theme_bw() +
  ylab('Rank') +
  xlab('Start of Iteration') +
  theme(text=element_text(size=18)) +
  scale_color_manual(values = col_pal) +
  labs(color = 'Driver')
ggplotly(diff_r2_panels)

ggsave('./figures/moving_window/r2_rank_timeseries.png', diff_r2_panels,
       dpi = 300, units = 'mm', height = 300, width = 550, scale = 0.4)
diff_r2_figs <- ggarrange(diff_r2, diff_r2_panels, 
                          common.legend = TRUE, labels = 'AUTO',
                          widths = c(1, 1.2))
diff_r2_figs

ggsave('./figures/moving_window/r2_diff_both.png', diff_r2_figs,
       dpi = 300, units = 'mm', height = 400, width = 900, scale = 0.4)

##################################################################################################
# for manuscript but not report
#### look at difference from none model
# positive values indicate that model was better than the none model (better than autoregression alone)
ggplotly(ggplot(out_prop, aes(x = as.Date(start_date), y = diff_from_none, color = id_covar)) +
           geom_point(size = 2) +
           scale_color_manual(values = col_pal) +
           theme_bw() +
           ylab('Difference from "none" model') +
           xlab('Start of Iteration') +
           labs(color = 'Covariate') +
           theme(text=element_text(size=18)))

diff_none <- ggplot(out_prop, aes(x = as.Date(start_date), y = diff_from_none, color = id_covar)) +
  geom_point() +
  scale_color_manual(values = col_pal) +
  theme_bw() +
  facet_wrap(~id_covar) +
  geom_hline(yintercept = 0) +
  ylab('Difference from "none" model') +
  xlab('Start of Iteration') +
  labs(color = 'Covariate') +
  theme(text=element_text(size=18))
diff_none

ggsave('./figures/moving_window/r2_diff_from_none.png', diff_none,
       dpi = 300, units = 'mm', height = 400, width = 600, scale = 0.4)