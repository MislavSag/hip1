# libs ----
library(fs)
library(mlr3batchmark)
library(batchtools)
library(data.table)
library(mlr3)
library(mlr3viz)
library(AzureStor)
# library(ggplot2)
library(gausscov)
library(paradox)
library(mlr3pipelines)
library(mlr3tuning)
library(mlr3misc)
library(future)
library(future.apply)
library(mlr3extralearners)
library(purrr)
# library(multcomp)


# Import command line arguments
args = commandArgs(trailingOnly = TRUE)

# Ensure there are enough arguments
if(length(args) < 2) {
  stop("Not enough arguments. Please provide id_1 and id_2.")
}

# Assign the arguments to variables
cat(args, sep = "\n")
id_1 = as.integer(args[1])
id_2 = as.integer(args[2])
cat("Argument 1 is ", id_1)
cat("Argument 2 is ", id_2)
if (interactive()) {
  id_1 = 1
  id_2 = 2
}

# Create directory to save files if it doesnt exist
if (!dir.exists("results_dmtest")) {
  dir.create("results_dmtest")
}

# Define save path
if (interactive()) {
  PATH = "C:/Users/Mislav/projects_r/mlspy/results"
} else {
  PATH = "experiments/results/"
}

# if file best tuned already exists stop script
file_ = file.path("results_dmtest", paste0("dmtest_", id_1, "_", id_2, ".rds"))
if (file.exists(file_)) {
  print("Already exists")
} else {
  # Read predictions
  files_ = file.path(PATH, paste0(id_1:id_2, ".rds"))
  results = lapply(files_, readRDS)
  
  # Extract id
  id_ = vapply(results, function(x) {
    grepl("filterrows", x$learner_state$model$learner$id)
  }, FUN.VALUE = logical(1))
  
  # Extract target variable horizont
  results[[1]]$learner_state$model$learner$state$train_task$id
  horizont_ = vapply(results, function(x) {
    data.table::fcase(
      grepl("task_targetRet1$", x$learner_state$model$learner$state$train_task$id), "ret1",
      grepl("task_targetRet6$", x$learner_state$model$learner$state$train_task$id), "ret6",
      grepl("task_targetRet12$", x$learner_state$model$learner$state$train_task$id), "ret12"
    )
  }, FUN.VALUE = character(1))
  
  # Clean predictions
  results = lapply(seq_along(results), function(i) {
    x = results[[i]]
    response = x$prediction$test$response
    truth = x$prediction$test$truth
    row_ids = x$prediction$test$row_ids
    data.table(
      row_ids = row_ids,
      response = response,
      truth = truth,
      residual = truth - response,
      filter = id_[i],
      horizont = horizont_[i],
      fold = (id_1:id_2)[i]
    )
  })
  
  # Save
  saveRDS(results, file_)
}
