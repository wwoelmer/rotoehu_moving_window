yr_to_hydro_yr <- function(data){ # data = dataframe with 'date' column in as.POSIXct() format
  data$hydroyear <- data$date+(184*60*60*24)
  data$hydroyear <- format(data$hydroyear,"%Y")
  data$hydroyear <- as.numeric(data$hydroyear)
  data$hydroyear_label <- paste(data$hydroyear-1, data$hydroyear, sep = "-")
  return(data)
}
