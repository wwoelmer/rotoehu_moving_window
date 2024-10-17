#variable <- 'air_temp_mean'
#rank_plot <- 1
#df_driver <- dat #dataframe with full time series of driver variable
#df_rank <- out_prop #dataframe with the ranks of variables
#ylab <- 'Mean Air Temperature (C)'

plot_date_range_rank <- function(variable, rank_plot, df_rank, df_driver, ylab, title = TRUE){
  
  rank_dates <- df_rank %>% filter(id_covar==variable,
                                   rank<=rank_plot)
  if(title){
    title <- ifelse(nrow(rank_dates) < 1, 
                    paste0('Variable is never of rank ', rank_plot),
                    paste0('Time period where ', ylab, ' is rank ', rank_plot))
  }else{
    title <- NULL
  }
  
  ymin <- min(df_driver %>% ungroup() %>% select(variable), na.rm = TRUE)
  ymax <- max(df_driver %>% ungroup() %>% select(variable), na.rm = TRUE)
  
  p1 <- ggplot(df_driver, aes(x = as.Date(date), y = .data[[variable]])) +
    annotate('rect', xmin = as.Date(rank_dates$start_date), xmax = as.Date(rank_dates$end_date),
             ymin = ymin, ymax = ymax, alpha = 0.1) +
    geom_point()  +
    geom_line() +
    theme_bw() +
    theme(text=element_text(size=16)) +
    xlab('Date') +
    ylab(ylab) +
    ggtitle(title)
  
  if(shading){
    p1 <- p1 + annotate('rect', xmin = as.Date(rank_dates$start_date), xmax = as.Date(rank_dates$end_date),
                  ymin = ymin, ymax = ymax, alpha = 0.1) 
    }
  
  return(p1)
}

#plot_date_range_rank(variable = 'rain_mean', rank = 1, df_rank = out_prop, df_driver = dat, ylab = 'Rain')
