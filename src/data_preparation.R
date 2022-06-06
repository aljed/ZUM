library("tidyverse")
library("datawizard")
library("mlr3verse")
library("ranger")


build_class_task <- function(data, ...) {
  if ("Time" %in% colnames(data)) {
    data <- data %>% mutate(
      TimeSin = transform_time(Time, sin),
      TimeCos = transform_time(Time, cos),
      .keep = "unused"
    )
  }

  task <- as_task_classif(data, target = "Class", positive = "fraud", ...)
  task$col_roles$stratum <- "Class"
  task
}

scaling_po <- function(scaling) {
  if (scaling == "minmax") {
    po <- po("scalerange")
  } else if (scaling == "zscore") {
    po <- po("scale")
  } else if (scaling == "robust") {
    po <- po("scale", robust = TRUE, id = "robustscale")
  }

  po
}


sampling_po <- function(sampling,
                        undersampling_rate = NULL,
                        oversampling_rate = NULL,
                        smote_rate = NULL,
                        smote_nn = NULL) {
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

  po
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
  costs <- matrix(c(0, 1, frequencies["fraud", "n"] / frequencies["legit", "n"], 0), nrow = 2)
  dimnames(costs) <- list(predicted = c("fraud", "legit"), real = c("fraud", "legit"))
  costs
}
