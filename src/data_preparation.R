#
library("tidyverse")
library("datawizard")
library("mlr3verse")
library("ranger")


build_class_task <- function(data,
                             scaling_method,
                             sampling,
                             sampling_rate,
                             smote_nn = NULL,
                             selected_attributes = c(),
                             feature_selection = NULL,
                             feature_selection_score = NULL) {
  if (!is_empty(selected_attributes)) {
    data <- data %>% select(selected_attributes)
  }

  if ("Time" %in% colnames(data)) {
    data <- data %>% mutate(
      TimeSin = transform_time(Time, sin),
      TimeCos = transform_time(Time, cos),
      .keep = "unused"
    )
  }

  if (scaling_method == "minmax") {
    data <- data %>% mutate(across(starts_with("V"), minmax_normalize))
  } else if (scaling_method == "zscore") {
    data <- standardise(data, select = starts_with("V"))
  } else if (scaling_method == "robust") {
    data <- standardise(data, select = starts_with("V"), robust = TRUE)
  } else {
    print("Scaling method " + scaling_method + " unsupported. Data will remain unscaled.")
  }

  task <- as_task_classif(data, target = "Class")

  if (!is.null(feature_selection_score)) {
    if (feature_selection == "correlation") {
      filter <- flt("find_correlation")
    } else if (feature_selection == "variable_importance") {
      lrn <- lrn("classif.ranger", importance = "impurity")
      filter <- flt("importance", learner = lrn)
    } else {
      print("Feature selection method unknown")
    }

    filter$calculate(task)
    filtered <- as.data.table(filter)
    filtered <- filtered[filtered$score > feature_selection_score, ]$feature
    task$select(filtered)
  }


  if (is.null(sampling_rate)) {
    stop("sampling_rate parameter must be filled")
  }

  if (sampling == "oversampling") {
    po <- po("classbalancing",
      id = "oversample", adjust = "major",
      reference = "major", shuffle = FALSE, ratio = sampling_rate
    )
  } else if (sampling == "downsampling") {
    po <- po("classbalancing",
      id = "undersample", adjust = "minor",
      reference = "minor", shuffle = FALSE, ratio = sampling_rate
    )
  } else if (sampling == "SMOTE") {
    if (is.null(smote_nn)) {
      stop("smote_nn parameter must be filled if SMOTE sampling is choosen")
    }
    po <- po("smote", dup_size = sampling_rate, K = smote_nn)
  } else {
    stop("Sampling method " + sampling + " unsupported.")
  }

  task <- po$train(list(task))[[1]]
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
