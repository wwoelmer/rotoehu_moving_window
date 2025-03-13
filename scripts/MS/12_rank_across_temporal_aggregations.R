# calculate model rank
library(tidyverse)
library(RColorBrewer)
library(ggridges)

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
  select(id_covar:iter_end, start_date, end_date, r2) %>% 
  group_by(iter_start) %>% 
  mutate(diff_from_best = max(r2) - r2,
         rank = dense_rank(desc(r2)),
         r2_none = r2[id_covar=='None'],
         diff_from_none = r2 - r2_none,
         rank_AR = dense_rank(desc(diff_from_none)),
         aic_none = aic[id_covar=='None'],
         diff_from_none_aic = aic - aic_none,
         rank_aic = dense_rank(desc(diff_from_none_aic*-1))) #multiply by -1 to change the sign so positive is good for ranking purposes

# rank variables based on differences in R2 and AICc

out_prop_AR <- out_prop %>% 
  select(id_covar:rank_AR)

out_rank <- plyr::ddply(out_prop_AR, c("id_covar", "rank_AR"), \(x) {
  n <- nrow(x)
  pct <- round(n/length(unique(out_prop$iter_start))*100)
  return(data.frame(pct = pct))
})


# define colors for the right number of ranks
## define color palettes for the right number of variables
num_ranks <- length(unique(out_rank$rank_AR))
rank_pal <- colorRampPalette(brewer.pal(9, "YlGnBu"))(num_ranks)

col_pal <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#F781BF", "#999999")

b <- ggplot(out_prop, aes(x = rank_AR, y = id_covar, fill = id_covar)) +
  geom_boxplot( 
    size = 1) +
  geom_jitter(data = out_prop, aes(x = rank_AR, y = id_covar), alpha = 0.1) +
  scale_fill_manual(values = col_pal) +
  theme_bw() +
  scale_x_continuous(breaks = (1:9),  # Specify breaks for y-axis
                     labels = (1:9)) +
  labs(fill = 'Driver') +
  xlab('Rank') +
  ylab("") +
  ggtitle('Moving windows') +
  theme(legend.position = 'none')
b

################################################################################
## repeat for 3 windows

out3 <- read.csv('./data/model_output_three_windows.csv')

# set up labels and levels of factor
out3$id_covar <- factor(out3$id_covar, 
                       levels = c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                                  "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                  "sum_alum", "none"),
                       labels = c("Bottom DRP", "Bottom NH4", "Bottom Water Temp",
                                  "Mean Air Temp", "Min Windspeed", "Water Level", 
                                  "Alum Dosed", "None"))

# calculate the difference across variables
out_prop3 <- out3 %>% 
  distinct(id_covar, start_date, .keep_all = TRUE) %>% 
  select(id_covar:iter_end, start_date, end_date, r2) %>% 
  group_by(start_date) %>% 
  mutate(diff_from_best = max(r2) - r2,
         rank = dense_rank(desc(r2)),
         r2_none = r2[id_covar=='None'],
         diff_from_none = r2 - r2_none,
         rank_AR = dense_rank(desc(diff_from_none)),
         aic_none = aic[id_covar=='None'],
         diff_from_none_aic = aic - aic_none,
         rank_aic = dense_rank(desc(diff_from_none_aic*-1))) #multiply by -1 to change the sign so positive is good for ranking purposes

# rank variables based on differences in R2 and AICc

out_prop_AR3 <- out_prop3 %>% 
  select(id_covar:rank_AR)

out_rank3 <- plyr::ddply(out_prop_AR3, c("id_covar", "rank_AR"), \(x) {
  n <- nrow(x)
  pct <- round(n/length(unique(out_prop$iter_start))*100)
  return(data.frame(pct = pct))
})


# define colors for the right number of ranks
## define color palettes for the right number of variables
num_ranks <- length(unique(out_rank3$rank_AR))
rank_pal <- colorRampPalette(brewer.pal(9, "YlGnBu"))(num_ranks)

out_rank3 <- out_rank3 %>% 
  group_by(rank_AR) %>% 
  arrange(pct) %>% 
  group_by(id_covar) %>% 
  mutate(sum_r2 = sum(pct*rank_AR))


col_pal <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#F781BF", "#999999")

b3 <- ggplot(out_prop3, aes(x = rank_AR, y = id_covar, color = id_covar)) +
  #geom_point(data = out_prop, aes(x = rank_AR, y = id_covar), alpha = 0.1) +
  geom_point(size = 4) +
  scale_color_manual(values = col_pal) +
  theme_bw() +
  facet_wrap(~start_date) +
  scale_x_continuous(breaks = (1:9),  # Specify breaks for y-axis
                     labels = (1:9)) +
  labs(fill = 'Driver') +
  xlab('Rank') +
  ylab("") +
  theme(legend.position = 'none') +
  ggtitle('Three windows')
b3

################################################################################
## repeat for full time period

outfull <- read.csv('./data/model_output_full.csv')
# set up labels and levels of factor

outfull$id_covar <- factor(outfull$id_covar, 
                           levels = c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                                      "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                      "sum_alum", "none"),
                           labels = c("Bottom DRP", "Bottom NH4", "Bottom Water Temp",
                                      "Mean Air Temp", "Min Windspeed", "Water Level", 
                                      "Alum Dosed", "None"))


# calculate the difference across variables
out_prop_full <- outfull %>% 
  distinct(id_covar, start_date, .keep_all = TRUE) %>% 
  group_by(start_date) %>% 
  mutate(diff_from_best = max(r2) - r2,
         rank = dense_rank(desc(r2)),
         r2_none = r2[id_covar=='None'],
         diff_from_none = r2 - r2_none,
         rank_AR = dense_rank(desc(diff_from_none)),
         aic_none = aic[id_covar=='None'],
         diff_from_none_aic = aic - aic_none,
         rank_aic = dense_rank(desc(diff_from_none_aic*-1))) #multiply by -1 to change the sign so positive is good for ranking purposes

# rank variables based on differences in R2 and AICc
out_prop_AR_full <- out_prop_full %>% 
  select(id_covar:rank_AR)

out_rank_full <- plyr::ddply(out_prop_AR_full, c("id_covar", "rank_AR"), \(x) {
  n <- nrow(x)
  pct <- round(n/length(unique(out_prop$iter_start))*100)
  return(data.frame(pct = pct))
})


# define colors for the right number of ranks
## define color palettes for the right number of variables
num_ranks <- length(unique(out_rank_full$rank_AR))
rank_pal <- colorRampPalette(brewer.pal(9, "YlGnBu"))(num_ranks)

out_rank_full <- out_rank_full %>% 
  group_by(rank_AR) %>% 
  arrange(pct) %>% 
  group_by(id_covar) %>% 
  mutate(sum_r2 = sum(pct*rank_AR))


col_pal <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#F781BF", "#999999")


bfull <- ggplot(out_rank_full, aes(x = rank_AR, y = id_covar, color = id_covar)) +
  geom_point(size = 4) +
  scale_color_manual(values = col_pal) +
  theme_bw() +
  scale_x_continuous(breaks = (1:9),  # Specify breaks for y-axis
                     labels = (1:9)) +
  labs(fill = 'Driver') +
  xlab('Rank') +
  ylab("") +
  theme(legend.position = 'none') +
  ggtitle('Full time series')
bfull

###############################################################################
ggarrange(bfull, b3, b, ncol = 1, labels = 'auto')
