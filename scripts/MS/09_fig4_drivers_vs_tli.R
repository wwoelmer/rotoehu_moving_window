library(tidyverse)
library(RColorBrewer)
library(broom)
library(ggpubr)

data <- read.csv('./data/master_rotoehu.csv')

# calculate monthly TLI
source('./scripts/functions/tli_fx.R')

data <- data %>% 
  group_by(month, year, lake, site) %>%
  mutate(tli_monthly = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m))

vars <- c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
          "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
          "sum_alum")

col_no <- length((vars)) + 2
col_pal <- colorRampPalette(brewer.pal(9, "Set1"))(col_no)
# update color pal so 'none' is the grey color
col_pal <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#F781BF", "#999999")

data <- data %>% 
  ungroup() %>% 
  select(date, tli_monthly, vars)

data_long <- data %>% 
  pivot_longer(bottom_DRP_ugL:sum_alum, names_to = 'variable', values_to = 'value')

data_long$variable <- factor(data_long$variable, 
                       levels = c("bottom_DRP_ugL", "bottom_NH4_ugL", "temp_C_8",
                                  "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                  "sum_alum"),
                       labels = c("Bottom DRP (µg/L)", "Bottom NH4 (µg/L)", "Bottom Water Temp (°C)",
                                  "Mean Air Temp (°C)", "Min Windspeed (m/s)", "Water Level (m)", 
                                  "Alum Dosed (L/day)"))

# check for normality
ggplot(data_long, aes(x = value, color = variable)) +
  geom_histogram() +
  facet_wrap(~variable, scales = 'free') +
  scale_color_manual(values = col_pal) +
  theme_bw()



# Fit linear models for each group and get p-values
models <- data_long %>%
  group_by(variable) %>%
  do(tidy(lm(tli_monthly ~ value, data = .)))

cor_dat <- data_long %>% 
  drop_na() %>% 
  group_by(variable) %>% 
  summarise(pearson = cor(tli_monthly, value),
            cor_signif = cor.test(tli_monthly, value)$p.value)
cor_dat

# Add significance column to the original dataset
significance_info <- models %>%
  filter(term == "value") %>%
  select(variable, p.value) %>%
  mutate(significant = p.value < 0.05)

data_long <- data_long %>%
  left_join(significance_info, by = "variable") %>%
  left_join(cor_dat, by = 'variable') %>% 
  mutate(significant = ifelse(is.na(significant), FALSE, significant))

# Plot using ggplot2 with facet_wrap
whole <- ggplot(data_long, aes(x = value, y = tli_monthly, color = variable)) +
  geom_point() +
  facet_wrap(~variable, scales = 'free') +
  geom_smooth(data = filter(data_long, cor_signif < 0.05), method = "lm") +
  scale_color_manual(values = col_pal) +
  geom_text(aes(x = Inf, y = Inf, label = round(pearson, 2)), 
            hjust = 1.1, vjust = 2, size = 4, color = "gray3") +
  theme_bw() +
  labs(x = 'Value', y = 'Monthly TLI',
       color = 'Variable') +
  theme(legend.position = 'none')
whole

ggsave('./figures/figure4_drivers_vs_tli.png', whole,
       dpi = 300, units = 'mm', height = 450, width = 450, scale = 0.4)
####################################################################################
# look at this during a time period where a non-significant variable increased in importance (e.g., water level)
dat_sub <- data_long %>% 
  filter(date > as.Date('2007-07-20') & date < as.Date('2015-11-20')) %>% 
  select(-significant, -p.value, -pearson, -cor_signif)

# Fit linear models for each group and get p-values
model_sub <- dat_sub %>%
  group_by(variable) %>%
  do(tidy(lm(tli_monthly ~ value, data = .))) 

cor_sub <- dat_sub %>% 
  drop_na() %>% 
  group_by(variable) %>% 
  summarise(pearson = cor(tli_monthly, value),
            cor_signif = cor.test(tli_monthly, value)$p.value)
cor_sub  

# Add significance column to the original dataset
significance_info <- model_sub %>%
  filter(term == "value") %>%
  select(variable, p.value) %>%
  mutate(significant = p.value < 0.05)

dat_sub2 <- dat_sub %>%
  left_join(significance_info, by = "variable") %>% 
  left_join(cor_sub, by = 'variable') %>% 
  mutate(significant = ifelse(is.na(significant), FALSE, significant)) 

# Plot using ggplot2 with facet_wrap
part <- ggplot(dat_sub2, aes(x = value, y = tli_monthly, color = variable)) +
  geom_point() +
  facet_wrap(~variable, scales = 'free') +
  geom_smooth(data = filter(dat_sub2, cor_signif < 0.05), method = "lm") +
  scale_color_manual(values = col_pal) +
  geom_text(aes(x = Inf, y = Inf, label = round(pearson, 2)), 
            hjust = 1.1, vjust = 2, size = 4, color = "gray3") +
  theme_bw() +
  labs(x = 'Value', y = 'Monthly TLI',
       color = 'Variable') +
  theme(legend.position = 'none')
part

ggsave('./figures/figureS9_drivers_vs_tli.png', part,
       dpi = 300, units = 'mm', height = 450, width = 450, scale = 0.4)
