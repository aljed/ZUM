library("mlr3verse")
library("ggplot2")
library("scales")

learner <- function(name, predict_type = "prob", predict_sets = c("train", "test"), ...) {
  lrn(name, predict_type = predict_type, predict_sets = predict_sets, ...)
}

tunethreshold <- function(learner,
                          resampling.method = "insample",
                          resampling.folds = 3,
                          resampling.keep_response = FALSE,
                          measure = "classif.ce",
                          optimizer = "gensa",
                          log_level = "warn",
                          ...) {
  GraphLearner$new(
    po(
      "learner_cv",
      learner,
      resampling.method = resampling.method,
      resampling.folds = resampling.folds,
      resampling.keep_response = resampling.keep_response
    ) %>>% po("tunethreshold", measure = measure, optimizer = optimizer, log_level = log_level),
    ...
  )
}

tunehyperparams <- function(resampling = rsmp("cv", folds = 3),
                            terminator = trm("stagnation"),
                            tuner = tnr("random_search"),
                            ...) {
  AutoTuner$new(
    resampling = resampling,
    terminator = terminator,
    tuner = tuner,
    ...
  )
}

plot_in_tab <- function(header, plot, level) {
  cat(level, header, "\n")
  suppressMessages(print(plot))
  cat("\n\n")
}

plot_features_in_tabs <- function(task, level = "####") {
  for (feature in task$feature_names) {
    plot_in_tab(feature, autoplot(task$clone()$select(c(feature)), type = "pairs"), level)
  }
}

plot_tasks_in_tabs <- function(benchmark, measure, autolimit = TRUE, level = "####", ...) {
  scores <- bmr$score(measure)[[measure$id]]
  for (task in benchmark$tasks$task) {
    plot <- autoplot(
      benchmark$clone(deep = TRUE)$filter(task_hash = task$hash),
      measure = measure,
      ...
    )
    if (autolimit) {
      plot <- plot + ylim(0.9 * min(scores), 1.1 * max(scores))
    }
    plot_in_tab(task$id, plot, level)
  }
}


aggregate_benchmark <- function(benchmark, measure, descending = TRUE) {
  scores <- bmr$aggregate(measure)
  if (descending) scores[order(-scores[[measure$id]])] else scores[order(scores[[measure$id]])]
}

plot_box <- function(aggregated, metric='classif.costs' ){
  
  learners <- c('randomforest', 'knn', 'svm', 'tree')
  shorten_name <- function(name){
    name <- str_replace(name, "variable_importance", "vi")
    name <- str_replace(name, "_none_fs", "")
    name <- str_replace(name, "_oversampling", "over_sam")
    name <- str_replace(name, "_SMOTE", "SMOTE")
    name <- str_replace(name, "_undersampling", "under_sam")
    name
  }
  
  selected <- select(aggregated, learner_id, task_id, m=metric)
  
  for (l in learners){
    s <- filter(selected, learner_id==l)
    s <- select(s, task_id, m)
    s <- mutate(s, task_id, normalization=str_extract(task_id, "zscore|minmax|robust"), task=shorten_name(str_extract(task_id, "_.*$")))
    if(metric!='classif.costs') 
      print(ggplot(s, aes(fill=normalization, y=m, x=task)) 
            + geom_bar(position="dodge", stat="identity") + ggtitle(l)
            + scale_y_continuous(limits=c(0.95,1),oob = rescale_none) + ylab(metric)
      )
    else 
      print(ggplot(s, aes(fill=normalization, y=m, x=task)) 
            + geom_bar(position="dodge", stat="identity") + ggtitle(l) + ylab(metric)
      )
    
  }
}
