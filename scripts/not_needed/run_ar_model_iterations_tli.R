# run simple AR model for TLI + one covariate from set of potential driver variables
# with groups defined as climatic, (air temp, rainfall, windspeed)
#                       anthropogenic, (land use change, alum dosing)
#                       climatic*anthropogenic interaction, (discharge, nutrient load)
#                       internal variability (e.g., obs complexity, anoxic factor?)

library(tidyverse)
library(MuMIn)
library(tidymodels)
library(plotly)
library(RColorBrewer)
library(patchwork)
library(ggpubr)
library(statcomp)


# read in data
#dat <- read.csv('./data/processed_data/BoP_wq_2007_2021.csv')
dat <- read.csv('./data/master_rotoehu.csv')

mean_secchi <- mean(dat$secchi_m, na.rm = TRUE)
mean_secchi

#calculate hydro year
dat$hydroyear <- as.POSIXct(dat$date) + (184*60*60*24)
dat$hydroyear <- format(dat$hydroyear,"%Y")
dat$hydroyear <- as.numeric(dat$hydroyear)
dat$hydroyear_label <- paste(dat$hydroyear-1, dat$hydroyear, sep = "-")


# remove daily water level as it is similar ot monthly (see daily_vs_monthly_water_level_comparison.R)
dat <- dat %>% 
  select(-avg_level_m)


# calculate monthly TLI
source('./scripts/R/tli_fx.R')

dat <- dat %>% 
  group_by(month, year, lake, site) %>%
  mutate(tli_monthly = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m)) %>% 
  group_by(year, lake, site) %>%
  mutate(tli_annual = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m))

hist(dat$tli_monthly)
tli <- ggplot(dat, aes(x = as.Date(date), y = tli_monthly)) +
  geom_point(size = 1.2) +
  geom_line(aes(x = as.Date(date), y = tli_annual), size = 2) +
  theme_bw()
ggplotly(tli)

mean(dat$tli_annual)

dat %>% 
  distinct(year, tli_annual, .keep_all = TRUE) %>% 
ggplot(aes(x = as.Date(date), y = tli_annual)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 3.9) +
  ylim(0, 6) +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('Date') +
  ylab('Annual TLI')

#######################################################
# run the ar model simulation
source('./scripts/R/run_ar.R')

# this set of variables comes from the decadal analysis (90s, 2000s, 2010s) plus land cover, alum, and 'none'
test_vars <- c("bottom_DRP_ugL", "bottom_NH4_ugL",
               "temp_C_8", "air_temp_mean", "windspeed_min", 
               "monthly_avg_level_m", 
               "schmidt_stability", 
               "sum_alum",
               "none")

id_var <- "tli_monthly"
window_length <- 100
n_iter <- seq(1, nrow(dat) - window_length)

out <- data.frame()

for(i in 1:length(test_vars)){
  if(test_vars[i]=='none'){
    dat_ar <- dat %>% 
      ungroup() %>% 
      select(date, id_var)  
  }else{
    dat_ar <- dat %>% 
      ungroup() %>% 
      select(date, id_var, test_vars[i])  
  }
  

  for(j in 1:length(n_iter)){
    
    # subset to the 100 observations in the iteration
    start <- j
    end <- j + window_length
    dat_sub <- dat_ar[start:end,]
    opd <-  weighted_ordinal_pattern_distribution(x = dat_sub$tli_monthly, ndemb = 4)
    pe <- permutation_entropy(opd) 
    
    # run the model
    d <- run_ar(data = dat_sub, 
                id_var = id_var, 
                id_covar = test_vars[i], 
                window_length = window_length)
    d$iter_start <- start
    d$iter_end <- end
    d$start_date <- min(dat_sub$date)
    d$end_date <- max(dat_sub$date)
    d$n <- nrow(dat_sub)
    d$pe <- pe
    out <- rbind(out, d)
    
  }
}

write.csv(out, './data/processed_data/moving_window/model_output.csv', row.names = FALSE)

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

out_mean <- out %>% 
  group_by(iter_start) %>% 
  summarize(mean_val = mean(r2, na.rm = TRUE),
            date = unique(as.Date(start_date)),
            sd_val = sd(r2, na.rm = TRUE),
            n = n(),
            t_score = qt((1 + 0.95) / 2, df = n - 1),
            margin_error = t_score * (sd_val / sqrt(n))) %>%
  mutate(lower_bound = mean_val - margin_error,
         upper_bound = mean_val + margin_error)
  

ggplot(out_mean, aes(x = date, y = mean_val)) +
  geom_line(data = out, aes(x = as.Date(start_date), y = r2, color = id_covar, alpha = 0.2)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), 
              fill = 'blue', alpha = 0.5) +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('Start date of iteration (+100 obs)') +
  ylab(bquote(~R^2)) +
  scale_color_manual(values = col_pal) +
  labs(color = 'Driver')


ggplotly(ggplot(out, aes(x = as.Date(start_date), y = r2, color = id_covar)) +
  geom_line() +
  geom_point(size = 2) +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('Start date of iteration (+100 obs)') +
  ylab('R2') +
  scale_color_manual(values = col_pal) +
  labs(color = 'Driver'))

ggsave('./figures/moving_window/r2_timeseries.png', r2_results,
       dpi = 300, units = 'mm', height = 300, width = 600, scale = 0.4)

tli + r2_results

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
         rank_aic = dense_rank(desc(diff_from_none_aic)))

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

ggsave('./figures/moving_window/r2_diff_from_none.png', diff_none,
       dpi = 300, units = 'mm', height = 400, width = 600, scale = 0.4)

################################################################################
# rank variables based on differences

out_prop <- out_prop %>% 
  select(id_covar:rank_AR)

out_rank <- plyr::ddply(out_prop, c("id_covar", "rank_AR", "rank"), \(x) {
  n <- nrow(x)
  pct <- round(n/length(unique(out_prop$iter_start))*100)
  return(data.frame(pct = pct))
})


# define colors for the right number of ranks
## define color palettes for the right number of variables
num_ranks <- length(unique(out_rank$rank))
rank_pal <- colorRampPalette(brewer.pal(9, "YlGnBu"))(num_ranks)

out_rank <- out_rank %>% 
  group_by(rank_AR, rank) %>% 
  arrange(pct) %>% 
  group_by(id_covar) %>% 
  mutate(sum_r2 = sum(pct*rank_AR),
         sum_aic = sum(pct*rank))

rank <- ggplot(out_rank, aes(x = reorder(id_covar, sum_aic), y = pct, fill = fct_rev(as.factor(rank_AR)))) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = rank_pal) +
  theme_bw() +
  ylab('Percent of time') +
  xlab('Covariate') +
  labs(fill = 'Rank') +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 0.55)) 
rank
ggsave('./figures/moving_window/rank_barplot.png', rank, dpi = 300, units = 'mm', height = 400, width = 600, scale = 0.3)

ggplot(out_rank, aes(x = reorder(id_covar, sum_aic, decreasing = TRUE), y = pct, fill = (as.factor(rank_AR)))) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = rank_pal) +
  theme_bw() +
  ylab('Percent of time') +
  xlab('Covariate') +
  labs(fill = 'Rank') +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 0.55)) 

################################################################################
# look at parameter values
params <- out %>% 
  filter(covar %in% test_vars) %>% 
  ggplot(aes(x = as.Date(start_date), y = value, color = id_covar)) +
  geom_point() +
  scale_color_manual(values = col_pal) +
  facet_wrap(~id_covar, scales = 'free_y') +
  theme_bw() +
  xlab('Start of Iteration') +
  ylab('Parameter Value') +
  labs(color = 'Covariate') +
  theme(text=element_text(size=15))

out %>% 
  filter(covar %in% test_vars) %>% 
  ggplot(aes(y = value, x = id_covar, fill = id_covar)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 0)) +
  scale_color_manual(values = col_pal) +
  scale_fill_manual(values = col_pal) +
  theme_bw() +
  #facet_wrap(~id_covar, scales = 'free_y') +
  xlab('Start of Iteration') +
  ylab('Parameter Value') +
  labs(color = 'Covariate') +
  theme(text=element_text(size=15), axis.text.x = element_text(angle = 45, hjust = 1))


ggplotly(params)
ggsave('./figures/moving_window/parameter_time_series.png', params, dpi = 300, units = 'mm', 
       height = 300, width = 550, scale = 0.4)

## select a single driving covariate and compare across model parameters
out %>% 
  filter(id_covar=='mean air temp') %>% 
  ggplot(aes(x = as.Date(start_date), y = value, color = covar)) +
  geom_point() +
  facet_wrap(~covar, scales = 'free_y') +
  theme_bw()

out %>% 
  filter(id_covar=='avg_level_m') %>% 
  ggplot(aes(x = as.Date(start_date), y = value, color = covar)) +
  geom_point() +
  facet_wrap(~covar, scales = 'free_y') +
  theme_bw()

out %>% 
  filter(id_covar=='none') %>% 
  ggplot(aes(x = as.Date(start_date), y = value, color = covar)) +
  geom_point() +
  facet_wrap(~covar, scales = 'free_y') +
  theme_bw()

## look at p-values
out %>% 
  filter(id_covar=='none',
         p_value < 0.05) %>% 
  ggplot(aes(x = as.Date(start_date), y = p_value, color = covar)) +
  geom_point() +
  facet_wrap(~covar, scales = 'free_y') +
  theme_bw()

###############################################################################################
# run the simulation on the entire dataset, without subsetting to time periods

out_all_ts <- data.frame()

for(i in 1:length(test_vars)){
  if(test_vars[i]=='none'){
    dat_ar <- dat %>% 
      ungroup() %>% 
      select(date, id_var)  
  }else{
    dat_ar <- dat %>% 
      ungroup() %>% 
      select(date, id_var, test_vars[i])  
  }
    # run the model
    d <- run_ar(data = dat_ar, id_var = id_var, id_covar = test_vars[i])
    d$iter_start <- start
    d$iter_end <- end
    d$start_date <- min(dat_sub$date)
    d$end_date <- max(dat_sub$date)
    d$n <- nrow(dat_sub)
    out_all_ts <- rbind(out_all_ts, d)
    
  
}


col_no_all <- length(unique(out_all_ts$id_covar))
col_pal_all <- colorRampPalette(brewer.pal(9, "Set1"))(col_no_all)

out_all_ts %>% 
  distinct(id_covar, .keep_all = TRUE) %>% 
ggplot(aes(x = id_covar, y = r2, color = as.factor(id_covar))) +
  geom_point(size = 4) +
  scale_color_manual(values = col_pal_all) +
  theme_bw() +
  ylab('R2') +
  labs(color = 'Covariate') +
  ylim(0.2, 0.7) +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  xlab('Covariate')


length(unique(dat$year))

############################################
# plot time series of driver variables with highlighted areas where that driver was of a high rank
source('./scripts/R/plot_date_range_rank.R')

# check a few out
plot_date_range_rank(variable = 'rain_mean', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'Rain')
plot_date_range_rank(variable = 'rain_mean', rank_plot = 2, df_rank = out_prop, df_driver = dat, ylab = 'Rain')
plot_date_range_rank(variable = 'temp_C_8', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'Bottom Water Temperature (C)')
plot_date_range_rank(variable = 'air_temp_mean', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'Mean Air Temperature (C)')
plot_date_range_rank(variable = 'bottom_DRP_ugL', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'Bottom Water DRP (ug/L)')
plot_date_range_rank(variable = 'TN_TP', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'TN:TP')
plot_date_range_rank(variable = 'thermo_depth', rank_plot = 4, df_rank = out_prop, df_driver = dat, ylab = 'thermocline depth')

dat <- dat %>% 
  mutate(diff_P = top_DRP_ugL - bottom_DRP_ugL,
         diff_NH4 = top_NH4_ugL - bottom_NH4_ugL,
         diff_NO3 = top_NO3_ugL - bottom_NO3_ugL) 
  
plot_date_range_rank(variable = 'bottom_DRP_ugL', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'diff btw surface and bottom DRP')

# some examples to save
exforest <- plot_date_range_rank(variable = 'area_pct_exotic_forest', rank_plot = 2, df_rank = out_prop, df_driver = dat, ylab = '% Exotic Forest in Watershed', title = FALSE, shading = FALSE)
lvl <- plot_date_range_rank(variable = 'avg_level_m', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'Average Water Level (m)', title = FALSE, shading = FALSE)
drp <- plot_date_range_rank(variable = 'bottom_DRP_ugL', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'Bottom Water DRP (ug/L)', title = FALSE, shading = FALSE)
btemp <- plot_date_range_rank(variable = 'temp_C_8', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'Bottom Water Temperature (C)', title = FALSE, shading = FALSE)

(exforest + lvl)/ (drp + btemp)
drp
btemp

sel_vars <- c('area_pct_exotic_forest', 'avg_level_m')
ggplot(out_prop[out_prop$id_covar %in% sel_vars,], aes(x = as.Date(start_date), y = as.factor(rank), color = as.factor(id_covar))) +
  geom_point() +
  facet_wrap(~id_covar) +
  theme_bw() +
  ylab('Rank') +
  xlab('Start of Iteration') +
  theme(text=element_text(size=12)) +
  scale_color_manual(values = col_pal) +
  labs(color = 'Covariate')

