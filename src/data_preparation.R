# 
library("tidyverse")
library("datawizard")

transform_time <- function(time_in_seconds, func_to_apply) {
  seconds_in_day <- 24 * 60 * 60
  day_time <- time_in_seconds %% seconds_in_day
  func_to_apply(day_time / seconds_in_day * 2 * pi)
}

minmax_normalize <- function(x, xmax, xmin) {
  (x - xmin) / (xmax - xmin)
}

build_dataset <- function(
    data,
    scaling_method,
    sampling,
    undersampling_count,
    oversampling_fraud_coefficient,
    smote_neighbors_count,
    selected_attributes
    ) {
  
  data <- data %>% select(selected_attributes)
    
  if("Time" %in% colnames(data)) {
    data <- data %>% transmute(Time, 
                               TimeSin = transform_time(Time), 
                               TimeCos = transform_time(Time))
  }
  
  if(scaling_method == "minmax")
    data %>% transmute(across(where(starts_with("V"))), minmax_normalize)
  else if(scaling_method == "minmax")
  
  
  scale_func <- switch (scaling_method,
    "minmax" = data %>% transmute(across(where(starts_with("V"))), scale_func),
    "zscore" = standardize,
    "robust" = standardize_robust
  )
  
  data <- data %>% transmute(across(where(starts_with("V"))), scale_func)
  
  standardise(d, select = c("Sepal.Length", "Sepal.Width"), append = TRUE)
  
  
  
    
  
}