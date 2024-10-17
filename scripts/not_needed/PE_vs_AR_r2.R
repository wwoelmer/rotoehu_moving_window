# look at relationship between PE and AR model results

out <- read.csv('./data/processed_data/moving_window/model_output.csv')


## define color palettes for the right number of variables
col_no <- length(unique(out$id_covar))
col_pal <- colorRampPalette(brewer.pal(9, "Set1"))(col_no)

out$id_covar <- factor(out$id_covar, 
                       levels = c("avg_level_m", "bottom_DRP_ugL", "bottom_NH4_ugL",
                                  "monthly_avg_level_m", "sum_alum", "windspeed_min", 
                                  "area_pct_hp_exotic_grassland", "none", "temp_C_8", 
                                  "air_temp_mean"),
                       labels = c("daily water level", "bottom DRP", "bottom NH4",
                                  "monthly water level", "alum dosed", "min windspeed",
                                  "% exotic grassland", "none", "bottom water temp",
                                  "air temp"))

################################################################################################################
# analyze trends with permutation entropy
out2 <- out %>% 
  distinct(id_covar, iter_start, .keep_all = TRUE)

ggplot(out2, aes(x = as.Date(start_date), y = pe)) +
  geom_point(size = 2) +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('Start date of iteration') +
  ylab('Permutation Entropy')


ggplot(out2, aes(x = pe, y = r2, color = as.factor(year(start_date)))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~id_covar) +
  guides(color = FALSE)

ggplot(out2, aes(x = pe, y = r2, color = as.factor(id_covar))) +
  geom_point() +
  geom_smooth(method = 'lm') 

ggplot(out2, aes(x = pe, y = r2, color = as.factor(id_covar))) +
  geom_point() +
  geom_smooth() 

# take the mean across models because they are not independent?
#  but what about the autocorrelation between simulations?
out2 %>% 
  group_by(start_date) %>% 
  mutate(mean_r2 = mean(r2)) %>% 
  ggplot(aes(x = pe, y = mean_r2)) +
  geom_point(size = 2) +
  theme(text=element_text(size=18)) +
  geom_smooth(method = 'lm') +
  ggtitle('Average across covariate models')

out2 %>% 
  group_by(start_date) %>% 
  mutate(mean_r2 = mean(r2)) %>% 
  ggplot(aes(x = pe, y = mean_r2)) +
  geom_point(size = 2) +
  theme(text=element_text(size=18)) +
  geom_smooth() +
  ggtitle('Average across covariate models')

summary(lm(r2 ~ pe, data = out2))
library(lme4)
summary(lmer(r2 ~ pe + (pe | start_date), data = out2))
