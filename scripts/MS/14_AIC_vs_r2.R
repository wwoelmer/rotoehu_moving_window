
###############################################################################
### compare difference between aic and r2 output

library(tidyverse)
library(ggplot2)
library(ggpubr)

out <- read.csv('./data/processed_data/moving_window/model_output.csv')

##################################################################################
# set up color palette
out$id_covar <- factor(out$id_covar, 
                       levels = c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                                  "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                  "schmidt_stability", "sum_alum", "none"),
                       labels = c("bottom DRP", "bottom NH4", "bottom water temp",
                                  "mean air temp", "min windspeed", "monthly water level", 
                                  "schmidt stability", "alum dosed", "none"))
col_no <- length(unique(out$id_covar))
col_pal <- colorRampPalette(brewer.pal(9, "Set1"))(col_no)
###################################################################################

ggplot(out, aes(x = iter_start, y = r2, color = id_covar)) +
  geom_line() +
  geom_point(size = 2) +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('Start date of iteration (+100 obs)') +
  ylab('R2') +
  scale_color_manual(values = col_pal) +
  labs(color = 'Covariate')

aicc <- ggplot(out, aes(x = iter_start, y = aic, color = id_covar)) +
  geom_line() +
  geom_point(size = 2) +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('Start date of iteration (+100 obs)') +
  ylab('AICc') +
  scale_color_manual(values = col_pal) +
  labs(color = 'Covariate')

aicc


aic_r2_compare <- ggplot(out, aes(x = aic, y = r2, color = id_covar)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('AICc') +
  ylab('R2') +
  scale_color_manual(values = col_pal) +
  labs(color = 'Covariate')

combined_fig <- ggarrange(aicc, aic_r2_compare, common.legend = TRUE, labels = 'AUTO')
combined_fig
ggsave('./figures/moving_window/MS/SI_figs/aic_r2_comparison.png', combined_fig,
       dpi = 300, units = 'mm', height = 300, width = 700, scale = 0.5)


out <- out %>% 
  distinct(id_covar, iter_start, .keep_all = TRUE) %>% 
  select(id_covar:iter_end, start_date, end_date, r2, aic) %>% 
  group_by(iter_start) %>% 
  mutate(aic_none = aic[id_covar=='none'],
         diff_from_none = aic - aic_none)

aic <- out %>% 
 # filter(id_covar!='none') %>% 
  ggplot(aes(x = as.Date(start_date), y = diff_from_none, color = id_covar)) +
  geom_point(size = 2) +
  scale_color_manual(values = col_pal) +
  geom_hline(aes(yintercept = 0)) +
  theme_bw() +
  facet_wrap(~id_covar) +
  ylab('Difference from AR only model (AIC units)') +
  xlab('Start of Iteration') +
  labs(color = 'Driver') +
  theme(text=element_text(size=16))
aic
ggsave('./figures/moving_window/aic_diff_from_none.png', aic,
       dpi = 300, units = 'mm', height = 300, width = 400, scale = 0.5)
