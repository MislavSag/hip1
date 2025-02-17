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


# Import mapee measure
source("mapee.R")
mlr_measures$add("mapee", MapeWithEpsilon)

# Import command line arguments
args = commandArgs(trailingOnly = TRUE)

# Assign the arguments to variables
if (interactive()) {
  id_1 = 1
  id_2 = 2
  setwd("F:/strategies/H1") # PATH
} else {
  # Ensure there are enough arguments
  if(length(args) < 2) {
    stop("Not enough arguments. Please provide id_1 and id_2.")
  }
  cat(args, sep = "\n")
  id_1 = as.integer(args[1])
  id_2 = as.integer(args[2])
  cat("Argument 1 is ", id_1)
  cat("Argument 2 is ", id_2)
}

# Create directory to save files if it doesnt exist
if (!dir.exists("results")) {
  dir.create("results")
}

# if file best tuned already exists stop script
file_ = file.path("results", paste0("best_tuned_", id_1, "_", id_2, ".rds"))
if (file.exists(file_)) {
  print("Already exists")
} else {
  # load registry----
  # if (interactive()) {
  #   reg = loadRegistry("F:/H2", work.dir="F:/H2")
  # } else {
  #   reg = loadRegistry("experiments")
  # }
  if (interactive()) {
    reg = loadRegistry(".")
  } else {
    reg = loadRegistry("experiments") 
  }
  
  # done jobs----
  ids_done = findDone(reg=reg)
  ids_done = ids_done[job.id %in% id_1:id_2]
  ids_notdone = findNotDone(reg=reg)
  # rbind(ids_notdone, ids_done[job.id %in% results_files])
  
  # get results----
  results = lapply(ids_done[, job.id], function(id_) {
    # bmr object
    bmr_ = reduceResultsBatchmark(id_, store_backends = FALSE, reg = reg)
    # aggregated results
    aggregate = bmr_$aggregate(msrs(c("regr.mse", "regr.mae", "regr.rmse", "mapee")))
    
    # bmr_$aggregate(msr("regr.mape"))
    # bmr_$aggregate(msr("regr.mape", na_value = 0))
    # bmr_test = bmr_$clone()
    # test = as.data.table(bmr_test$resample_results$resample_result[[1]]$predictions()[[1]])
    # mlr3measures::mape(truth = test$truth, response = test$response)
    # mlr3measures::mape(truth = test$truth[test$truth != 0], response = test$response[test$truth != 0])
    
    # instances across folds
    rresults = bmr_$resample_results
    # get aggreagate and instances
    return(list(aggregate = aggregate, rresults = rresults))
  })
  
  # Aggreagates
  aggregates_l = lapply(results, function(x) x$aggregate)
  aggregates = rbindlist(aggregates_l)
  aggregates[, filter_applied := ifelse(grepl("filterrows", learner_id), "filter", "nonfilter")]
  aggregates[, cv := as.numeric(sub(".*_([0-9]+)_.*", "\\1", resampling_id))]
  aggregates[, fold := as.numeric(sub(".*_([0-9]+)$", "\\1", resampling_id))]
  aggregates_df = aggregates[,.(filter_applied,cv,fold, task_id,regr.mse,regr.rmse, regr.mae, mape_with_epsilon)]
  
  # best tuned model ----
  rresults = lapply(results, function(x) x$rresults)
  instances = lapply(rresults, function(x) x$resample_result)
  instances_unlist = unlist(instances)
  instances_weiter = lapply(instances_unlist, function(x) {
    # x = instances_unlist[[1]]
    # extract tuning_instance
    tuning_instance = x$learners[[1]]$state$model$tuning_instance
    # extract task, result , learner_id and resampling_id
    task = x$task$id
    result = tuning_instance$result
    learner_id = x$learner$id
    resampling_id = x$resampling$id
    # compute filter_applied, cv, and fold
    filter_applied = ifelse(grepl("filterrows", learner_id), "filter", "nofilter")
    cv = as.numeric(sub(".*_([0-9]+)_.*", "\\1", resampling_id))
    fold = as.numeric(sub(".*_([0-9]+)$", "\\1", resampling_id))
    # return as a list
    return(list(
      tuning_instance = tuning_instance,
      task = task,
      result = result,
      learner_id = learner_id,
      resampling_id = resampling_id,
      filter_applied = filter_applied,
      cv = cv,
      fold = fold,
      mapee = x$score(msr("mapee"))$mape_with_epsilon
    ))
  })
  best_tuned_list <- lapply(instances_weiter, function(x) {
    # x = instances_weiter[[2]]
    task = x$task
    if (!is.null(x$result)) {
      filterrows.filter_formula = if (is.null(x$result$filterrows.filter_formula)) NA else x$result$filterrows.filter_formula
      branch.selection = x$result$branch.selection
      regr.mse = x$result$regr.mse
    } else {
      filterrows.filter_formula = NA
      branch.selection = NA
      regr.mse = NA
    }
    resampling_id = x$resampling_id
    filter_applied = x$filter_applied
    cv = x$cv
    fold = x$fold
    
    return(data.frame(task, filterrows.filter_formula, branch.selection, regr.mse,
                      filter_applied, cv, fold, mapee = x$mapee))
  })
  # convert to single data.frame
  best_tuned_df <- do.call(rbind, best_tuned_list)
  best_tuned_df <- best_tuned_df[!(best_tuned_df$filter_applied == "nofilter"), ]
  
  # average tuned model ----
  instances_archives = lapply(instances_unlist, function(x) {
    
    # x = instances_unlist[[2]]
    
    # extract tuning_instance and archive data
    tuning_instance = x$learners[[1]]$state$model$tuning_instance
    archive_data = tuning_instance$archive$data
    
    # if archive_data is NULL
    if (is.null(archive_data)) {
      return(NULL)
    }
    
    # additional information
    task = x$task$id
    result = tuning_instance$result
    learner_id = x$learner$id
    resampling_id = x$resampling$id
    
    # compute pca_applied, cv, and fold
    filter_applied = ifelse(grepl("filterrows", learner_id), "filter", "nofilter")
    cv = as.numeric(sub(".*_([0-9]+)_.*", "\\1", resampling_id))
    fold = as.numeric(sub(".*_([0-9]+)$", "\\1", resampling_id))
    
    # number of PC
    
    # ncp = ifelse(!is.null(x$learners[[1]]$state$model$learner$state$model$pca_explained$rotation),
    #              ncol(x$learners[[1]]$state$model$learner$state$model$pca_explained$rotation),
    #              NA)
    #
    # # extract selected columns
    # if (is.na(ncp)) {
    #   return(NULL)
    # } else {
    #   selected_data = archive_data[, .(pca_explained.var., branch.selection, regr.mse)]
    # }
    
    
    # return list
    return(list(
      #      selected_data = selected_data,
      task = task,
      result = result,
      learner_id = learner_id,
      resampling_id = resampling_id,
      filter_applied = filter_applied,
      cv = cv,
      fold = fold
      #     ncp = ncp
    ))
  })
  # function for mode----
  get_mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  average_tuned_list <- lapply(instances_archives, function(x) {
    if (is.null(x)) return(NULL)
    
    # Check if filterrows.filter_formula exists and is not empty
    filter_explained_result <- if (!is.null(x$result$filterrows.filter_formula) && length(x$result$filterrows.filter_formula) > 0) {
      x$result$filterrows.filter_formula
    } else {
      NA  # or any default value you want
    }
    
    branch_selection_result <- if (!is.null(x$result$branch.selection) && length(x$result$branch.selection) > 0) {
      x$result$branch.selection
    } else {
      NA
    }
    
    regr_mse_result <- if (!is.null(x$result$regr.mse) && length(x$result$regr.mse) > 0) {
      x$result$regr.mse
    } else {
      NA
    }
    
    data.frame(
      task = x$task,
      filter_applied = x$filter_applied,
      cv = x$cv,
      fold = x$fold,
      filter_explained_result_best = filter_explained_result,
      branch_selection_result_best = branch_selection_result,
      regr_mse_result_best = regr_mse_result,
      row.names = NULL
    )
  })
  average_tuned_df <- do.call(rbind, average_tuned_list)
  
  # average tuned model descriptives----
  transform_data <- function(x) {
    if (is.null(x)) return(NULL)  # for NULL entries
    
    df <- x$selected_data
    df$task <- x$task
    df$cv <- x$cv
    df$fold <- x$fold
    
    return(df)
  }
  average_tuned_model_descriptives_list <- lapply(instances_archives, transform_data)
  average_tuned_model_descriptives_df <- do.call(rbind, average_tuned_model_descriptives_list)
  
  # save files
  save_file = function(tag, id1, id2) paste0("results/", tag, "_", id1, "_", id2, ".rds")
  saveRDS(aggregates_df, save_file("aggregates", id_1, id_2))
  saveRDS(average_tuned_df, save_file("average_tuned", id_1, id_2))
  saveRDS(average_tuned_model_descriptives_df, save_file("average_tuned_model_descriptives", id_1, id_2))
  saveRDS(best_tuned_df, save_file("best_tuned", id_1, id_2))
}
