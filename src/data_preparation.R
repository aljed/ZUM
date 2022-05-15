# 
library("tidyverse")
library("datawizard")
library("mlr")

build_class_task <- function(
    data,
    scaling_method,
    sampling,
    sampling_rate,
    smote_nn = NULL,
    selected_attributes = c()
    ) {
  
  if(!is_empty(selected_attributes))
    data <- data %>% select(selected_attributes)
    
  if("Time" %in% colnames(data)) {
    data <- data %>% mutate(TimeSin = transform_time(Time, sin), 
                            TimeCos = transform_time(Time, cos),
                            .keep = "unused")
  }
  
  if(scaling_method == "minmax")
    data <- data %>% mutate(across(starts_with("V"), minmax_normalize))
  else if(scaling_method == "zscore")
    data <- standardise(data, select = starts_with("V"))
  else if(scaling_method == "robust")
    data <- standardise(data, select = starts_with("V"), robust = TRUE)
  else
    print("Scaling method " + scaling_method + " unsupported. Data will remain unscaled.")
  
  task <- makeClassifTask(data = data, target = "Class")
  
  if(is.null(sampling_rate))
    stop("sampling_rate parameter must be filled")
    
  if(sampling == "oversampling")
    task <- oversample(task, rate = sampling_rate)
  else if(sampling == "downsampling")
    task <- undersample(task, rate = sampling_rate)
  else if(sampling == "SMOTE"){
    if(is.null(smote_nn))
      stop("smote_nn parameter must be filled if SMOTE sampling is choosen")
    task <- smote(task, rate = sampling_rate, nn = smote_nn)
  }
  else
    stop("Sampling method " + sampling + " unsupported.")
  
  task
  
}


transform_time <- function(time_in_seconds, func_to_apply) {
  seconds_in_day <- 24 * 60 * 60
  day_time <- time_in_seconds %% seconds_in_day
  func_to_apply(day_time / seconds_in_day * 2 * pi)
}


minmax_normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}