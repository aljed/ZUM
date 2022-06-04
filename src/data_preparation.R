library("tidyverse")
library("datawizard")
library("mlr3verse")
library("ranger")


build_class_task <- function(data, scaling_method, ...) {
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

  task <- as_task_classif(data, target = "Class", ...)
  task$col_roles$stratum = "Class"
  task
}

prepare_class_task <- function(task,
                               sampling,
                               undersampling_rate = NULL,
                               oversampling_rate = NULL,
                               smote_rate = NULL,
                               smote_nn = NULL,
                               selected_attributes = c(),
                               add_weights = FALSE) {
  if (!is_empty(selected_attributes)) {
    task$select(selected_attributes)
  }
    
  if (sampling == "undersampling") {
    if (is.null(smote_nn)) {
      stop("undersampling_rate parameter must be filled if undersampling is choosen")
    }
    po <- po("classbalancing",
      id = "undersample", adjust = "major",
      reference = "major", shuffle = FALSE, ratio = undersampling_rate
    )
  } else if (sampling == "oversampling") {
    if (is.null(smote_nn)) {
      stop("oversampling_rate parameter must be filled if oversampling is choosen")
    }
    po <- po("classbalancing",
      id = "oversample", adjust = "minor",
      reference = "minor", shuffle = FALSE, ratio = oversampling_rate
    )
  } else if (sampling == "SMOTE") {
    if (is.null(smote_nn)) {
      stop("smote_nn parameter must be filled if SMOTE sampling is choosen")
    }
    po <- po("smote", dup_size = smote_rate, K = smote_nn)
  } else {
    stop("Sampling method " + sampling + " unsupported.")
  }
  
  task = po$train(list(task))[[1]]
  
  if (add_weights) {
    opb = po("classweights")
    cost_m = cost_matrix(task$data(cols = "Class"))
    opb$param_set$values$minor_weight = cost_m[1, 2]
    task = opb$train(list(task))[[1L]]
  }
  
  task
}


select_features <- function(task, feature_selection, feature_selection_score) {
  if (feature_selection == "correlation") {
    filter <- flt("find_correlation")
  } else if (feature_selection == "variable_importance") {
    lrn <- lrn("classif.ranger", importance = "impurity")
    filter <- flt("importance", learner = lrn)
  } else {
    print("Feature selection method unknown")
  }
  
  if (!is.null(feature_selection_score)) {
    filter$calculate(task)
    filtered <- as.data.table(filter)
    filtered <- filtered[filtered$score > feature_selection_score, ]$feature
  }
  
  filtered
}


transform_time <- function(time_in_seconds, func_to_apply) {
  seconds_in_day <- 24 * 60 * 60
  day_time <- time_in_seconds %% seconds_in_day
  func_to_apply(day_time / seconds_in_day * 2 * pi)
}


minmax_normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

cost_matrix <- function(data) {
  frequencies <- data %>%
    count(Class) %>%
    column_to_rownames("Class")
  costs <- matrix(c(0, 1, frequencies["legit", "n"] / frequencies["fraud", "n"], 0), nrow = 2)
  dimnames(costs) <- list(predicted = c("fraud", "legit"), real = c("fraud", "legit"))
  costs
}
