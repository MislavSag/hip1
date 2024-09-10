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

# Assign the arguments to variables
if (interactive()) {
  id_1 = 1
  id_2 = 2
  setwd("F:/strategies/H1")
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
    aggregate = bmr_$aggregate(msrs(c("regr.mse", "regr.mae", "regr.rmse")))
    # instances across folds
    rresults = bmr_$resample_results
    # get aggreagate and instances
    return(list(aggregate = aggregate, rresults = rresults))
  })
  
  # aggregated results for each testing period (add average and sd in reporting table)----
  aggregates_l = lapply(results, function(x) x$aggregate)
  aggregates = rbindlist(aggregates_l)
  aggregates[, pca_applied := ifelse(grepl("pca_explained", learner_id), "pca", "nopca")]
  aggregates[, cv := as.numeric(sub(".*_([0-9]+)_.*", "\\1", resampling_id))]
  aggregates[, fold := as.numeric(sub(".*_([0-9]+)$", "\\1", resampling_id))]
  aggregates_df = aggregates[,.(pca_applied,cv,fold, task_id,regr.mse,regr.rmse, regr.mae)]
  
  #(zanemari)
  # report results ----
  aggregates_mse  <- dcast(aggregates, cv + task_id + fold ~ pca_applied, value.var = "regr.mse")
  aggregates_rmse <- dcast(aggregates, cv + task_id + fold ~ pca_applied, value.var = "regr.rmse")
  aggregates_mae  <- dcast(aggregates, cv + task_id + fold ~ pca_applied, value.var = "regr.mae")
  # rename columns for clarity ----
  setnames(aggregates_mse, c("pca", "nopca"), c("mse_pca", "mse_nopca"))
  setnames(aggregates_rmse, c("pca", "nopca"), c("rmse_pca", "rmse_nopca"))
  setnames(aggregates_mae, c("pca", "nopca"), c("mae_pca", "mae_nopca"))
  # extract by cv ----
  cv_list_of_tables_mse <- aggregates[, .(table = list(dcast(.SD, task_id + fold ~ pca_applied, value.var = "regr.mse"))), by = cv]$table
  cv_list_of_tables_rmse <- aggregates[, .(table = list(dcast(.SD, task_id + fold ~ pca_applied, value.var = "regr.rmse"))), by = cv]$table
  cv_list_of_tables_mae <- aggregates[, .(table = list(dcast(.SD, task_id + fold ~ pca_applied, value.var = "regr.mae"))), by = cv]$table
  # extract by task ----
  task_list_of_tables_mse <- aggregates[, .(table = list(dcast(.SD,  fold ~ pca_applied, value.var = "regr.mse"))), by = task_id]$table
  task_list_of_tables_rmse <- aggregates[, .(table = list(dcast(.SD,  fold ~ pca_applied, value.var = "regr.rmse"))), by = task_id]$table
  task_list_of_tables_mae <- aggregates[, .(table = list(dcast(.SD,  fold ~ pca_applied, value.var = "regr.mae"))), by = task_id]$table
  
  # best tuned model ----
  rresults = lapply(results, function(x) x$rresults)
  instances = lapply(rresults, function(x) x$resample_result)
  instances_unlist = unlist(instances)
  instances_weiter = lapply(instances_unlist, function(x) {
    # extract tuning_instance
    tuning_instance = x$learners[[1]]$state$model$tuning_instance
    # extract task, result , learner_id and resampling_id
    task = x$task$id
    result = tuning_instance$result
    learner_id = x$learner$id
    resampling_id = x$resampling$id
    # compute pca_applied, cv, and fold
    pca_applied = ifelse(grepl("pca_explained", learner_id), "pca", "nopca")
    cv = as.numeric(sub(".*_([0-9]+)_.*", "\\1", resampling_id))
    fold = as.numeric(sub(".*_([0-9]+)$", "\\1", resampling_id))
    # return as a list
    return(list(
      tuning_instance = tuning_instance,
      task = task,
      result = result,
      learner_id = learner_id,
      resampling_id = resampling_id,
      pca_applied = pca_applied,
      cv = cv,
      fold = fold
    ))
  })
  best_tuned_list <- lapply(instances_weiter, function(x) {
    # x = instances_weiter[[2]]
    task = x$task
    if (!is.null(x$result)) {
      pca_explained.var. = if (is.null(x$result$pca_explained.var.)) NA else x$result$pca_explained.var.
      branch.selection = x$result$branch.selection
      regr.mse = x$result$regr.mse
    } else {
      pca_explained.var. = NA
      branch.selection = NA
      regr.mse = NA
    }
    resampling_id = x$resampling_id
    pca_applied = x$pca_applied
    cv = x$cv
    fold = x$fold
    
    return(data.frame(task, pca_explained.var., branch.selection, regr.mse,
                      pca_applied, cv, fold))
  })
  # convert to single data.frame
  best_tuned_df <- do.call(rbind, best_tuned_list)
  best_tuned_df <- best_tuned_df[!(best_tuned_df$pca_applied == "nopca"), ]
  
  
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
    pca_applied = ifelse(grepl("pca_explained", learner_id), "pca", "nopca")
    cv = as.numeric(sub(".*_([0-9]+)_.*", "\\1", resampling_id))
    fold = as.numeric(sub(".*_([0-9]+)$", "\\1", resampling_id))
    
    # number of PC
    ncp = ifelse(!is.null(x$learners[[1]]$state$model$learner$state$model$pca_explained$rotation),
                 ncol(x$learners[[1]]$state$model$learner$state$model$pca_explained$rotation),
                 NA)
    
    # extract selected columns
    if (is.na(ncp)) {
      return(NULL)
    } else {
      selected_data = archive_data[, .(`pca_explained.var.`, branch.selection, regr.mse)]
    }
    
    
    # return list
    return(list(
      selected_data = selected_data,
      task = task,
      result = result,
      learner_id = learner_id,
      resampling_id = resampling_id,
      pca_applied = pca_applied,
      cv = cv,
      fold = fold,
      ncp = ncp
    ))
  })
  # function for mode----
  get_mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  average_tuned_list <- lapply(instances_archives, function(x) {
    
    # x = instances_archives[[2]]
    
    # for NULL entries
    if (is.null(x)) return(NULL)
    
    # calculate mode for pca_explained.var.
    mode_pca_explained = get_mode(x$selected_data$pca_explained.var.)
    if (is.na(mode_pca_explained)) return(NULL)
    mean_regr_mse = mean(as.numeric(as.character(x$selected_data$regr.mse)), na.rm = TRUE)  # need to convert regr.mse to numeric
    
    # extract details
    pca_explained_result = x$result$pca_explained.var.
    branch_selection_result = x$result$branch.selection
    regr_mse_result = x$result$regr.mse
    
    # makew data frame
    data.frame(
      task = x$task,
      pca_applied = x$pca_applied,
      cv = x$cv,
      fold = x$fold,
      mode_pca_explained = mode_pca_explained,
      mean_regr_mse = mean_regr_mse,
      pca_explained_result_best = pca_explained_result,
      branch_selection_result_best = branch_selection_result,
      regr_mse_result_best = regr_mse_result,
      ncp = x$ncp,  #
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
