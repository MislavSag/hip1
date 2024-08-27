library(data.table)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(lubridate)
library(mlr3verse)
library(future.apply)
library(batchtools)
library(mlr3batchmark)
library(finautoml)
library(paradox)
library(mlr3extralearners)
library(batchtools)
library(mlr3batchmark)
library(torch)
library(mlr3torch)


# COMMAND LINE ARGUMENTS --------------------------------------------------
if (interactive()) {
  LIVE = FALSE
} else {
  # Import command line arguments
  args = commandArgs(trailingOnly = TRUE)
  
  # Ensure there are enough arguments
  if (length(args) < 1) {
    stop("Not enough arguments. Please provide LIVE as TRUE or FALSE.")
  }
  
  # Assign the arguments to variables
  cat(args, sep = "\n")
  LIVE = as.logical(as.integer(args[1]))
  cat("Argument 1 is ", LIVE)
  
  # Nuber of cores used in learning
  ncores = 8
}


# SETUP -------------------------------------------------------------------
# utils https://stackoverflow.com/questions/1995933/number-of-months-between-two-dates
monnb <- function(d) {
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
  lt$year*12 + lt$mon }
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
diff_in_weeks = function(d1, d2) difftime(d2, d1, units = "weeks") # weeks

# # Azure creditentials
# endpoint = "https://snpmarketdata.blob.core.windows.net/"
# key = "0M4WRlV0/1b6b3ZpFKJvevg4xbC/gaNBcdtVZW+zOZcRi0ZLfOm1v/j2FZ4v+o8lycJLu1wVE6HT+ASt0DdAPQ=="
# BLOBENDPOINT = storage_endpoint(endpoint, key=key)
# cont = storage_container(BLOBENDPOINT, "jphd")

# Check cuda
cuda_is_available()
cuda_device_count()


# PREPARE DATA ------------------------------------------------------------
print("Prepare data")

# read predictors
if (interactive()) {
  library(fs)
  pead_file_local = list.files("F:/predictors/spyml_dt", full.names = TRUE)
  dates = as.Date(gsub(".*-", "", path_ext_remove(path_file(pead_file_local))),
                  format = "%Y%m%d")
  DT = fread(pead_file_local[which.max(dates)])
} else {
  DT = fread("spyml-predictors-20240129.csv")
}

# Define predictors
cols_non_features = c("date", "open", "high", "low", "close", "volume")
targets = c("targetRet1", "targetRet6", "targetRet12")
cols_features = setdiff(colnames(DT), c(cols_non_features, targets))

# Convert columns to numeric. This is important only if we import existing features
chr_to_num_cols = setdiff(colnames(DT[, .SD, .SDcols = is.character]), c("symbol"))
print(chr_to_num_cols)
if (length(chr_to_num_cols) > 0) {
  DT[, (chr_to_num_cols) := lapply(.SD, as.numeric), .SDcols = chr_to_num_cols]  
}
# Remove observations with missing target
# Probably different for LIVE
DT = na.omit(DT, cols = targets)


# TASKS -------------------------------------------------------------------
# Create separate tasks for every target variable
create_task = function(id_cols, target_ = "targetRet1") {
  cols_ = c(id_cols, target_, cols_features)
  task_ = as_task_regr(DT[, ..cols_],
                       id = paste0("task_", target_),
                       target = target_)
  task_$col_roles$feature = setdiff(task_$col_roles$feature, id_cols)
  return(task_)
}
colnames(DT)[grep("target", colnames(DT))]
id_cols = c("date")
tasks = list(
  task_1  = create_task(id_cols, "targetRet1"),
  task_3  = create_task(id_cols, "targetRet6"),
  task_12 = create_task(id_cols, "targetRet12")
)


# CROSS VALIDATIONS -------------------------------------------------------
create_custom_rolling_windows = function(task,
                                         duration_unit = "day",
                                         train_duration,
                                         gap_duration,
                                         tune_duration,
                                         test_duration) {
  # DEBUG
  # task = tasks[[1]]
  # duration_unit = "week"
  # train_duration = 52 * 2
  # gap_duration = 1
  # tune_duration = 4
  # test_duration = 1
  
  # Function to convert durations to the appropriate period
  convert_duration = function(duration) {
    if (duration_unit == "day") {
      days(duration)
    } else if (duration_unit == "week") {
      weeks(duration)
    } else {
      months(duration)
    }
  }
  
  # Define row ids
  data = task$backend$data(cols = c("date", "..row_id"),
                           rows = 1:task$nrow)
  setnames(data, "..row_id", "row_id")
  stopifnot(all(task$row_ids == data[, row_id]))
  
  # Create date column
  data[, date_col := as.Date(date)]
  
  # Initialize start and end dates based on duration unit
  start_date = if (duration_unit == "day") {
    floor_date(min(data$date_col), "day")
  } else if (duration_unit == "week") {
    floor_date(min(data$date_col), "week")
  } else {
    floor_date(min(data$date_col), "month")
  }
  end_date = if (duration_unit == "day") {
    ceiling_date(max(data$date_col), "day")
  } else if(duration_unit == "week") {
    ceiling_date(max(data$date_col), "week") - days(1)
  } else {
    ceiling_date(max(data$date_col), "month") - days(1)
  }
  
  # Initialize folds list
  folds = list()
  
  while (start_date < end_date) {
    if (duration_unit %in% c("week", "month")) {
      train_end = start_date %m+% convert_duration(train_duration) - days(1)
    } else {
      train_end = start_date %m+% convert_duration(train_duration)  
    }
    gap1_end  = train_end %m+% convert_duration(gap_duration)
    tune_end  = gap1_end %m+% convert_duration(tune_duration)
    gap2_end  = tune_end %m+% convert_duration(gap_duration)
    if (duration_unit %in% c("week", "month")) {
      test_end  = gap2_end %m+% convert_duration(test_duration) - days(1)
    } else {
      test_end  = gap2_end %m+% convert_duration(test_duration)
    }
    
    # Ensure the fold does not exceed the data range
    if (test_end > end_date) {
      break
    }
    
    # Extracting indices for each set
    train_indices = data[date_col %between% c(start_date, train_end), row_id]
    tune_indices  = data[date_col %between% c(gap1_end + days(1), tune_end), row_id]
    test_indices  = data[date_col %between% c(gap2_end + days(1), test_end), row_id]
    
    folds[[length(folds) + 1]] <- list(train = train_indices, tune = tune_indices, test = test_indices)
    
    # Update the start date for the next fold
    start_date = if (duration_unit == "day") {
      start_date %m+% days(1)
    } else if (duration_unit == "week") {
      start_date %m+% weeks(1)
    } else {
      start_date %m+% months(1)
    }
  }
  
  # Prepare sets for inner and outer resampling
  train_sets = lapply(folds, function(fold) fold$train)
  tune_sets  = lapply(folds, function(fold) fold$tune)
  test_sets  = lapply(folds, function(fold) fold$test)
  
  # Combine train and tune sets for outer resampling
  inner_sets = lapply(seq_along(train_sets), function(i) {
    c(train_sets[[i]], tune_sets[[i]])
  })
  
  # Instantiate custom inner resampling (train: train, test: tune)
  custom_inner = rsmp("custom", id = paste0(task$id, "-inner"))
  custom_inner$instantiate(task, train_sets, tune_sets)
  
  # Instantiate custom outer resampling (train: train+tune, test: test)
  custom_outer = rsmp("custom", id = paste0(task$id, "-outer"))
  custom_outer$instantiate(task, inner_sets, test_sets)
  
  return(list(outer = custom_outer, inner = custom_inner))
}

# simplifed cv functoin
task_check = function(task) {
  if (grepl("1$", task$id)) {
    return(1)
  } else if (grepl("6$", task$id)) {
    return(6)
  } else if (grepl("12$", task$id)) {
    return(12)
  }
}
cv_split = function(task) {
  create_custom_rolling_windows(
    task = task$clone(),
    duration_unit = "week",
    train_duration = 52*2,
    gap_duration = task_check(task$clone()),
    tune_duration = 4*3,
    test_duration = 1
  )
}
custom_cvs = lapply(tasks, function(tsk) {
  cv_split(tsk)
})

# visualize test
if (interactive()) {
  library(ggplot2)
  library(patchwork)
  prepare_cv_plot = function(x, set = "train") {
    x = lapply(x, function(x) data.table(ID = x))
    x = rbindlist(x, idcol = "fold")
    x[, fold := as.factor(fold)]
    x[, set := as.factor(set)]
    x[, ID := as.numeric(ID)]
  }
  plot_cv = function(cv, n = 5) {
    # cv = custom_cvs[[1]]
    print(cv)
    cv_test_inner = cv$inner
    cv_test_outer = cv$outer
    
    # prepare train, tune and test folds
    train_sets = cv_test_inner$instance$train[1:n]
    train_sets = prepare_cv_plot(train_sets)
    tune_sets = cv_test_inner$instance$test[1:n]
    tune_sets = prepare_cv_plot(tune_sets, set = "tune")
    test_sets = cv_test_outer$instance$test[1:n]
    test_sets = prepare_cv_plot(test_sets, set = "test")
    dt_vis = rbind(train_sets[seq(1, nrow(train_sets), 500)],
                   tune_sets[seq(1, nrow(tune_sets), 100)],
                   test_sets)
    substr(colnames(dt_vis), 1, 1) = toupper(substr(colnames(dt_vis), 1, 1))
    ggplot(dt_vis, aes(x = Fold, y = ID, color = Set)) +
      geom_point() +
      theme_minimal() +
      coord_flip() +
      labs(x = "", y = '',
           title = paste0(gsub("-.*", "", cv_test_outer$id)))
  }
  plots = lapply(custom_cvs, plot_cv, n = 30)
  wp = wrap_plots(plots)
  ggsave("plot_cv.png", plot = wp, width = 10, height = 8, dpi = 300)
}


# LEARNERS ----------------------------------------------------------------
# add my pipes to mlr dictionary
mlr_pipeops$add("uniformization", finautoml::PipeOpUniform)
# mlr_pipeops$add("winsorize", PipeOpWinsorize)
mlr_pipeops$add("winsorizesimple", finautoml::PipeOpWinsorizeSimple)
# mlr_pipeops$add("winsorizesimplegroup", PipeOpWinsorizeSimpleGroup)
mlr_pipeops$add("dropna", finautoml::PipeOpDropNA)
mlr_pipeops$add("dropnacol", finautoml::PipeOpDropNACol)
mlr_pipeops$add("dropcorr", finautoml::PipeOpDropCorr)
# mlr_pipeops$add("pca_explained", PipeOpPCAExplained)
mlr_pipeops$add("filter_rows", finautoml::PipeOpFilterRows)
mlr_pipeops$add("filter_target", finautoml::PipeOpFilterRegrTarget)
mlr_filters$add("gausscov_f1st", finautoml::FilterGausscovF1st)
# mlr_filters$add("gausscov_f3st", FilterGausscovF3st)
mlr_measures$add("linex", finautoml::Linex)
mlr_measures$add("adjloss2", finautoml::AdjLoss2)
# learners - THIS IS TEMPORARY, IT WILL BE ADDED TO EXTRALEARNERS
source("regr_tabnet.R")
mlr_learners$add("regr_tabnet", LearnerRegrTabNet)


# LEARNERS ----------------------------------------------------------------
print("Create graph")

# test MLP learner
mlp_graph = po("torch_ingress_num") %>>%
  po("nn_linear", out_features = 20) %>>%
  po("nn_relu") %>>%
  po("nn_head") %>>%
  po("torch_loss", loss = t_loss("mse")) %>>%
  po("torch_optimizer", optimizer = t_opt("adam", lr = 0.1)) %>>%
  po("torch_callbacks", callbacks = t_clbk("history")) %>>%
  po("torch_model_regr", batch_size = 16, epochs = 50, device = "cpu")

# cretate learners graph node
learners_l = list(
  ranger  = lrn("regr.ranger", id = "ranger"),
  xgboost = lrn("regr.xgboost", id = "xgboost"),
  bart    = lrn("regr.bart", id = "bart"),
  nnet    = lrn("regr.nnet", id = "nnet", MaxNWts = 50000),
  mlp     = mlp_graph,
  tabnet  = lrn("regr_tabnet", id = "tabnet")
)

# create regression average of all learners
choices = c("ranger", "xgboost", "bart", "nnet", "mlp", "tabnet")
learners = po("branch", choices) %>>%
  gunion(learners_l) %>>%
  po("unbranch")

# non filter graph
graph_nonfilter = po("dropnacol", id = "dropnacol", cutoff = 0.05) %>>%
  po("dropna", id = "dropna") %>>%
  po("removeconstants", id = "removeconstants_1", ratio = 0)  %>>%
  po("winsorizesimple", id = "winsorizesimple", probs_low = 0.01, probs_high = 0.99, na_rm = TRUE) %>>%
  po("removeconstants", id = "removeconstants_2", ratio = 0)  %>>%
  po("dropcorr", id = "dropcorr", cutoff = 0.99) %>>%
  po("uniformization") %>>%
  po("dropna", id = "dropna_v2") %>>%
  learners
plot(graph_nonfilter)
graph_nonfilter_lrn = as_learner(graph_nonfilter)

# filter graph
graph_filter = 
  po("filter_rows", phase = "always") %>>%
  po("dropnacol", id = "dropnacol", cutoff = 0.05) %>>%
  po("dropna", id = "dropna") %>>%
  po("removeconstants", id = "removeconstants_1", ratio = 0)  %>>%
  po("winsorizesimple", id = "winsorizesimple", probs_low = 0.01, probs_high = 0.99, na_rm = TRUE) %>>%
  po("removeconstants", id = "removeconstants_2", ratio = 0)  %>>%
  po("dropcorr", id = "dropcorr", cutoff = 0.99) %>>%
  po("uniformization") %>>%
  po("dropna", id = "dropna_v2") %>>%
  learners
plot(graph_filter)
graph_filter_lrn = as_learner(graph_filter)

# threads
print("Set threads")
set_threads(graph_filter_lrn, n = ncores)
set_threads(graph_nonfilter_lrn, n = ncores)

# non filter params
search_space_nonpca = ps(
  branch.selection = p_fct(choices)
)

# filter params
as.data.table(graph_filter_lrn$param_set)[, .(id, class, lower, upper)]
as.data.table(graph_filter_lrn$param_set)[1:100, .(id, class, lower, upper)]
tasks[[1]]$feature_names[grep("event", tasks[[1]]$feature_names)]
tasks[[1]]$backend$data(rows = 1:tasks[[1]]$nrow, cols = c("date", "eventVol1001"))[eventVol1001 == 1]
search_space = ps(
  filterrows.filter_formula = p_fct(levels = as.character(1:2)),
  # learner branch
  branch.selection = p_fct(choices),
  .extra_trafo = function(x, param_set) {
    if (x$filterrows.filter_formula == "1") {
      x$filterrows.filter_formula = as.formula("~ eventVol1001 == 1")
    } 
    # else if (x$filterrows.filter_formula == "2") {
    #   x$filterrows.filter_formula = as.formula("~ eventVol1002 == 1")
    # } else if (x$filterrows.filter_formula == "3") {
    #   x$filterrows.filter_formula = as.formula("~ eventVol1003 == 1")
    # } 
    else if (x$filterrows.filter_formula == "2") {
      x$filterrows.filter_formula = as.formula("~ eventVol4501 == 1")
    } 
    # else if (x$filterrows.filter_formula == "5") {
    #   x$filterrows.filter_formula = as.formula("~ eventVol4502 == 1")
    # } else if (x$filterrows.filter_formula == "6") {
    #   x$filterrows.filter_formula = as.formula("~ eventVol4503 == 1")
    # }
    return(x)
  }
)

# Debug
if (interactive()) {
  # show all combinations from search space, like in grid
  sp_grid = generate_design_grid(search_space, 1)
  sp_grid = sp_grid$data
  sp_grid
  
  # check ids of nth cv sets
  train_ids = custom_cvs[[1]]$inner$instance$train[[1]]
  
  # Test preprocessing
  gr_test = 
    po("filter_rows", phase = "train") %>>%
    po("dropnacol", id = "dropnacol", cutoff = 0.05) %>>%
    po("dropna", id = "dropna") %>>%
    po("removeconstants", id = "removeconstants_1", ratio = 0)  %>>%
    # po("winsorizesimple", id = "winsorizesimple", probs_low = 0.01, probs_high = 0.99, na_rm = TRUE) %>>%
    # po("removeconstants", id = "removeconstants_2", ratio = 0)  %>>%
    po("dropcorr", id = "dropcorr", cutoff = 0.99) %>>%
    po("uniformization") %>>%
    po("dropna", id = "dropna_v2") %>>%
    po("removeconstants", id = "removeconstants_1", ratio = 0)
  task_ = tasks[[1]]$clone()
  task_$filter(train_ids)
  res_ = gr_test$train(task_)
  dt_ = res_$filterrows.output$data()
  tasks[[1]]$data(cols = "eventVol1001")
  table(tasks[[1]]$data(cols = "eventVol1001"))
  
  dt_[, eventVol1001]
  dt_[, eventVol4501]
  dt_[, adxAdx14]
  cor(tasks[[1]]$data(cols = c("eventVol1001", "eventVol4501")))
}

# create designs
print("Create designs")
if (interactive()) {
  from_ = 1
} else {
  from_ = seq_along(custom_cvs)
}
designs_l = lapply(from_, function(j) {
  # debug
  # j = 1
  task_ = tasks[[j]]$clone()
  cv_ = custom_cvs[[j]]
  
  # get cv inner object
  cv_inner = cv_$inner
  cv_outer = cv_$outer
  cat("Number of iterations fo cv inner is ", cv_inner$iters, "\n")
  
  # debug
  if (interactive()) {
    to_ = 2
  } else {
    to_ = cv_inner$iters
  }
  
  designs_cv_l = lapply(1:to_, function(i) { # 1:cv_inner$iters
    # debug
    # i = 1
    
    # choose task_
    print(cv_inner$id)

    # with new mlr3 version I have to clone
    task_inner = task_$clone()
    task_inner$id = "task_inner"
    task_inner$filter(c(cv_inner$train_set(i), cv_inner$test_set(i)))
    
    # inner resampling
    custom_ = rsmp("custom")
    custom_$id = paste0("custom_", cv_inner$iters, "_", i)
    custom_$instantiate(task_inner,
                        list(cv_inner$train_set(i)),
                        list(cv_inner$test_set(i)))
    
    # auto tuner
    at_nonfilter = auto_tuner(
      tuner = tnr("grid_search", resolution = 20, batch_size = 2),
      learner = graph_nonfilter_lrn,
      resampling = custom_,
      measure = msr("regr.mse"),
      search_space = search_space_nonpca
    )
    
    # auto tuner
    at_filter = auto_tuner(
      tuner = tnr("grid_search", resolution = 20, batch_size = 2),
      learner = graph_filter_lrn,
      resampling = custom_,
      measure = msr("regr.mse"),
      search_space = search_space
    )
    
    # outer resampling
    customo_ = rsmp("custom")
    customo_$id = paste0("custom_", cv_inner$iters, "_", i)
    task_outer = task_$clone()
    task_outer$id = "task_outer"
    customo_$instantiate(task_outer, 
                         list(cv_outer$train_set(i)), 
                         list(cv_outer$test_set(i)))
    
    # nested CV for one round
    design = benchmark_grid(
      tasks = task_,
      learners = list(at_filter, at_nonfilter),
      resamplings = customo_
    )
  })
  designs_cv = do.call(rbind, designs_cv_l)
  # batchmark(designs)
})
designs = do.call(rbind, designs_l)

# exp dir
if (interactive()) {
  dirname_ = "experiments_test"
  if (dir.exists(dirname_)) system(paste0("rm -r ", dirname_))
} else {
  dirname_ = "experiments"
}

# create registry
print("Create registry")
packages = c("data.table", "gausscov", "paradox", "mlr3", "mlr3pipelines",
             "mlr3tuning", "mlr3misc", "future", "future.apply",
             "mlr3extralearners")
reg = makeExperimentRegistry(file.dir = dirname_, seed = 1, packages = packages)

# populate registry with problems and algorithms to form the jobs
print("Batchmark")
batchmark(designs, reg = reg, store_models = TRUE)

# save registry
print("Save registry")
saveRegistry(reg = reg)

# If interactive train models, else create file
if (interactive()) {
  test = batchtools::testJob(1, reg = reg)
  test2 = batchtools::testJob(2, reg = reg)
} else {
  # create sh file
  sh_file = sprintf("
#!/bin/bash

#PBS -N H1
#PBS -l ncpus=%d
#PBS -l mem=16GB
#PBS -J 1-%d
#PBS -o experiments/logs
#PBS -j oe

cd ${PBS_O_WORKDIR}
apptainer run image.sif run_job.R 0
", ncores, nrow(designs))
  sh_file_name = "padobran.sh"
  file.create(sh_file_name)
  writeLines(sh_file, sh_file_name)
}

