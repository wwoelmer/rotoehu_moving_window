# calculate model rank
library(tidyverse)
library(RColorBrewer)
library(ggridges)

out <- read.csv('./data/model_output.csv')
################################################################################
# set up labels and levels of factor

out$id_covar <- factor(out$id_covar, 
                       levels = c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                                  "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                  "schmidt_stability", "sum_alum", "none"),
                       labels = c("Bottom DRP", "Bottom NH4", "Bottom Water Temp",
                                  "Mean Air Temp", "Min Windspeed", "Water Level", 
                                  "Schmidt Stability", "Alum Dosed", "None"))


################################################################################
# calculate the difference across variables
out_prop <- out %>% 
  distinct(id_covar, iter_start, .keep_all = TRUE) %>% 
  select(id_covar:iter_end, start_date, end_date, r2) %>% 
  group_by(iter_start) %>% 
  mutate(diff_from_best = max(r2) - r2,
         rank = dense_rank(desc(r2)),
         r2_none = r2[id_covar=='none'],
         diff_from_none = r2 - r2_none,
         rank_AR = dense_rank(desc(diff_from_none)),
         aic_none = aic[id_covar=='none'],
         diff_from_none_aic = aic - aic_none,
         rank_aic = dense_rank(desc(diff_from_none_aic*-1))) #multiply by -1 to change the sign so positive is good for ranking purposes


################################################################################
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

out_rank <- out_rank %>% 
  group_by(rank_AR) %>% 
  arrange(pct) %>% 
  group_by(id_covar) %>% 
  mutate(sum_r2 = sum(pct*rank_AR))

rank <- ggplot(out_rank, aes(x = reorder(id_covar, sum_r2), y = pct, fill = fct_rev(as.factor(rank_AR)))) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = rank_pal) +
  theme_bw() +
  ylab('Percent of time') +
  xlab('Driver') +
  labs(fill = 'Rank') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.55)) 
rank
#ggsave('./figures/moving_window/MS/figure6_rank_barplot.png', rank, dpi = 300, units = 'mm', height = 400, width = 600, scale = 0.3)

col_no <- length(unique(out_rank$id_covar))
col_pal <- colorRampPalette(brewer.pal(9, "Set1"))(col_no)

# calculate the median as a sorting value
out_prop <- out_prop %>% 
  group_by(id_covar) %>% 
  mutate(median_rank = median(rank_AR))

b <- ggplot(out_prop, aes(x = rank_AR, y = reorder(id_covar, median_rank), fill = id_covar)) +
  geom_boxplot() +
  geom_jitter(data = out_prop, aes(x = rank_AR, y = id_covar), alpha = 0.1) +
  scale_fill_manual(values = col_pal) +
  theme_bw() +
  scale_x_continuous(breaks = (1:9),  # Specify breaks for y-axis
                   labels = (1:9)) +
  labs(fill = 'Driver') +
  xlab('Rank') +
  ylab("") +
  theme(legend.position = 'none')
b



p1 <- ggarrange(rank, b, labels = 'auto', widths = c(0.55, 0.45))
p1
ggsave('./figures/figure6_rank_heatmap_with_boxplot.png', p1, 
       dpi = 300, units = 'mm', height = 400, width = 700, scale = 0.3)
