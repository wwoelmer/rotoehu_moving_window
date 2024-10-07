library(tidyverse)
library(RColorBrewer)

out <- read.csv('./data/processed_data/moving_window/model_output.csv')
################################################################################
# set up labels and levels of factor

out$id_covar <- factor(out$id_covar, 
                       levels = c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                                  "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                  "schmidt_stability", "sum_alum", "none"),
                       labels = c("bottom DRP", "bottom NH4", "bottom water temp",
                                  "mean air temp", "min windspeed", "monthly water level", 
                                  "schmidt stability", "alum dosed", "none"))
col_no <- length(unique(out$id_covar))
col_pal <- colorRampPalette(brewer.pal(9, "Set1"))(col_no)
################################################################################
# look at R2 results
r2_results <- ggplot(out, aes(x = as.Date(start_date), y = r2, color = id_covar)) +
  geom_line() +
  geom_point(size = 2) +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('Start date of iteration (+100 obs)') +
  ylab(bquote(~R^2)) +
  scale_color_manual(values = col_pal) +
  labs(color = 'Driver')
r2_results

t <- out %>% 
  #filter(id_covar=='none') %>% 
  mutate(significant = ifelse(p_value < 0.05, TRUE, FALSE)) %>% 
  distinct(id_covar, start_date, .keep_all = TRUE)

all_with_signif <- ggplot(t, aes(x = as.Date(start_date), y = r2, group = id_covar, color = (significant))) +
  geom_line() +
  geom_point(size = 2) +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('Start date of iteration (+100 obs)') +
  ylab(bquote(~R^2)) +
  scale_color_manual(values = c('#6C5379', '#5FAD56')) +
  labs(color = 'Significant')
all_with_signif
ggsave('./figures/moving_window/MS/figS6_r2_with_signif.png', all_with_signif,
       dpi = 300, units = 'mm', height = 300, width = 600, scale = 0.4)

out_mean <- out %>% 
  group_by(iter_start) %>% 
  summarize(mean_val = mean(r2, na.rm = TRUE),
            max_pval = max(p_value, na.rm = TRUE),
            date = unique(as.Date(start_date)),
            sd_val = sd(r2, na.rm = TRUE),
            n = n(),
            t_score = qt((1 + 0.95) / 2, df = n - 1),
            margin_error = t_score * (sd_val / sqrt(n))) %>%
  mutate(lower_bound = mean_val - margin_error,
         upper_bound = mean_val + margin_error)


r2_mean <- ggplot(out_mean, aes(x = date, y = mean_val)) +
#  geom_line(data = out, aes(x = as.Date(start_date), y = r2, color = id_covar, alpha = 0.2)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), 
              fill = 'blue', alpha = 0.5) +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('Start date of iteration (+100 obs)') +
  ylab(bquote(~R^2)) +
  scale_color_manual(values = col_pal) +
  labs(color = 'Driver')
r2_mean

ggsave('./figures/moving_window/MS/SI_figs/MS/r2_timeseries_all_vars.png', r2_results,
       dpi = 300, units = 'mm', height = 300, width = 600, scale = 0.4)

ggsave('./figures/moving_window/MS/fig3_r2_mean_CI.png', r2_mean,
       dpi = 300, units = 'mm', height = 300, width = 475, scale = 0.4)

