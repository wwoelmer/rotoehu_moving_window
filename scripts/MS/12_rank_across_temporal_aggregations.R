# calculate model rank
library(tidyverse)
library(RColorBrewer)

# Moving windows ----
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
  pct <- (n/length(unique(out_prop$iter_start))*100)
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



## repeat for full time period ----

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


order_full <- c('Bottom DRP', 'Mean Air Temp', 'Bottom Water Temp', 'Bottom NH4',
                'Min Windspeed', 'None', 'Water Level', 'Alum Dosed')

## Discrete windows ----

out3 <- read.csv('./data/model_output_three_windows.csv')

# set up labels and levels of factor
out3$id_covar <- factor(out3$id_covar, 
                        levels = c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                                   "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                   "sum_alum", "none"),
                        labels = c("Bottom DRP", "Bottom NH4", "Bottom Water Temp",
                                   "Mean Air Temp", "Min Windspeed", "Water Level", 
                                   "Alum Dosed", "None"))

# remove alum dosing results from first window which had no alum application
out3 <- out3 %>% 
  filter(!(start_date=='2000-07-11' & id_covar=='Alum Dosed'))

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




## Plotting ----


(discrete_rank <- ggplot(out_prop3,
                         aes(x = timeperiod, y = fct_rev(factor(id_covar, levels = order_full)), fill = rank)) +
    geom_tile(colour = "black") +
    scale_fill_distiller(palette ="YlGnBu" )+
    scale_x_discrete( expand = c(0,0))+
    scale_y_discrete(expand = c(0,0))+
    theme_bw() +
    labs(x = "\nWindow start date", y = NULL, title = "B. Discrete")+
    geom_text(aes(label = rank))+
    theme(legend.position = "none", panel.grid = element_blank() , 
          axis.text.y = element_blank(), axis.ticks.y =  element_blank(),
          axis.text.x = element_text(colour = "black")
    ) )

(full_rank <- ggplot(out_prop_full,
                     aes(x = timeperiod, y = fct_rev(factor(id_covar, levels = order_full)), fill = rank)) +
    geom_tile(colour = "black") +
    scale_fill_distiller(palette ="YlGnBu" )+
    
    scale_x_discrete( expand = c(0,0))+
    scale_y_discrete(expand = c(0,0))+
    theme_bw() +
    labs(x = NULL, y = NULL, title = "A. Full")+
    geom_text(aes(label = rank))+
    theme(legend.position = "none", panel.grid = element_blank() , 
          axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
          axis.text.y = element_text(colour = "black", size = 12)
    ) )

num_ranks <- length(unique(out_rank$rank_AR))
rank_pal <- colorRampPalette(brewer.pal(8, "YlGnBu"))(num_ranks)

(moving_window_rank <- out_prop%>%
    mutate(start_date = as.Date(start_date), end_date = as.Date(end_date))%>%
    group_by(id_covar)%>%
    arrange(start_date)%>%
    mutate(moving_end_date = lead(start_date))%>%
    mutate(moving_end_date = if_else(is.na(moving_end_date), as.Date("2013-04-19"), moving_end_date))%>%
    ggplot()+
    geom_rect(aes(xmin = start_date, xmax = moving_end_date, ymin = 0, ymax = 1, fill = fct_rev(as.factor(rank_AR))))+
    scale_fill_manual(values = rank_pal,  guide = guide_legend(reverse = TRUE))+
    scale_x_date(expand = c(0,0))+
    scale_y_discrete(expand = c(0,0))+
    facet_wrap(~factor(id_covar, levels = c("Bottom DRP", "Mean Air Temp", "Bottom Water Temp", "Bottom NH4",          
                                            "Min Windspeed" , "None",  "Water Level", "Alum Dosed" )), ncol = 1)+
    theme_bw() +
    labs(x = "\nWindow start date", y = NULL, title = "C. Moving", fill = "Rank")+
    theme(legend.justification = "bottom",
          panel.grid = element_blank() , 
          axis.text.y = element_blank(), 
          axis.ticks.y =  element_blank(), 
          axis.text.x = element_text(colour = "black"),
          strip.text = element_blank(), 
          panel.spacing.y = unit(0, "lines"), 
          panel.border = element_rect(linewidth = 0.3) , 
          legend.key = element_rect(color = "black", fill = NA, linewidth = 0.2) 
    ))



(rank_all_figure <- full_rank + discrete_rank + moving_window_rank+ plot_layout(ncol = 3, widths = c(1, 3, 6)))

ggsave('./figures/resubmission/rank_all_windows.png', rank_all_figure,
       dpi = 300, units = 'mm', height = 280, width = 500, scale = 0.6)

