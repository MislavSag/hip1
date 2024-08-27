library(mlr3batchmark)
library(fs)
library(matrixStats)
library(PerformanceAnalytics)
library(ggplot2)


# Creds
# blob_key = "0M4WRlV0/1b6b3ZpFKJvevg4xbC/gaNBcdtVZW+zOZcRi0ZLfOm1v/j2FZ4v+o8lycJLu1wVE6HT+ASt0DdAPQ=="
# endpoint = "https://snpmarketdata.blob.core.windows.net/"
# BLOBENDPOINT = storage_endpoint(endpoint, key=blob_key)

# Globals
# PATH = "F:/padobran/spyml"
PATH = "F:/strategies/H1"

# load registry
reg = loadRegistry(PATH, work.dir=PATH)

# Used memory
reg$status[!is.na(mem.used)]
reg$status[, max(mem.used, na.rm = TRUE)]

# Done jobs
results_files = path_ext_remove(path_file(dir_ls(path(PATH, "results"))))
ids_done = findDone(reg=reg)
ids_done = ids_done[job.id %in% results_files]
ids_notdone = findNotDone(reg=reg)
ids_notdone[`job.id` %in% 1:1000]

# Get results
tabs = batchtools::getJobTable(ids_done, reg = reg)[
  , c("job.id", "job.name", "repl", "prob.pars", "algo.pars"), with = FALSE]
predictions_meta = cbind.data.frame(
  id = tabs[, job.id],
  task = vapply(tabs$prob.pars, `[[`, character(1L), "task_id"),
  learner = gsub(".*regr.|.tuned", "", vapply(tabs$algo.pars, `[[`, character(1L), "learner_id")),
  cv = gsub("custom_|_.*", "", vapply(tabs$prob.pars, `[[`, character(1L), "resampling_id")),
  fold = gsub("custom_\\d+_", "", vapply(tabs$prob.pars, `[[`, character(1L), "resampling_id"))
)
predictions_l = lapply(unlist(ids_done), function(id_) {
  # id_ = 10035
  x = tryCatch({readRDS(fs::path(PATH, "results", id_, ext = "rds"))},
               error = function(e) NULL)
  if (is.null(x)) {
    print(id_)
    return(NULL)
  }
  x["id"] = id_
  x
})
predictions = lapply(predictions_l, function(x) {
  cbind.data.frame(
    id = x$id,
    row_ids = x$prediction$test$row_ids,
    truth = x$prediction$test$truth,
    response = x$prediction$test$response
  )
})
predictions = rbindlist(predictions)
predictions = merge(predictions_meta, predictions, by = "id")
predictions = as.data.table(predictions)

# Import tasks
tasks_files = dir_ls(fs::path(PATH, "problems"))
tasks = lapply(tasks_files, readRDS)
names(tasks) = lapply(tasks, function(t) t$data$id)

# Add backend to predictions
backend_l = lapply(tasks, function(tsk_) {
  # tsk_ = tasks[[1]]
  x = tsk_$data$backend$data(1:1000000,
                             c("date", "..row_id"))
  setnames(x, c("date", "row_ids"))
  x[, task := gsub("target_ret_", "", tsk_$data$target_names)]
  x
})
backends = rbindlist(backend_l, fill = TRUE)

# Merge predictions and backends
predictions[, task := gsub("task_", "", task)]
predictions = backends[, .(date, task, row_ids)][predictions, on = c("task", "row_ids")]

# Measures
# source("AdjLoss2.R")
# source("PortfolioRet.R")
# mlr_measures$add("linex", finautoml::Linex)
# mlr_measures$add("adjloss2", AdjLoss2)
# mlr_measures$add("portfolio_ret", PortfolioRet)


# MARKET DATA ------------------------------------------------------------
# SPY data
spy = fread("spy-ohlcv.csv")
spy[, returns := close / shift(close) - 1]


# BACKTEST ----------------------------------------------------------------
# define backtest data that merge TLT and predictions
dt = merge(spy, predictions, by = "date", all.x = TRUE, all.y = FALSE)
dt = na.omit(dt, cols = "task")

# Backtest
dt_wide = dt[, .(date, learner, response)]
dt_wide = dcast(dt_wide, date ~ learner, value.var = "response")
dt_wide[, res_sum := rowSums2(as.matrix(.SD), na.rm = TRUE), 
        .SDcols = colnames(dt_wide)[2:ncol(dt_wide)]]
cols_new = paste0("signal", colnames(dt_wide)[2:ncol(dt_wide)])
dt_wide[, (cols_new) := lapply(.SD, function(x) x >= 0),
        .SDcols = colnames(dt_wide)[2:ncol(dt_wide)]]
dt_back = merge(spy, dt_wide, by = "date", all.x = TRUE, all.y = FALSE)
model_with_least_na = dt_back[, colSums(is.na(.SD), na.rm = TRUE), .SDcols = cols_new]
model_with_least_na = names(model_with_least_na)[model_with_least_na == min(model_with_least_na)][[1]]
dt_back = na.omit(dt_back, cols = model_with_least_na)

dt_back = dt_back[, `:=`(
  strategy_bart = returns * shift(signalbart),
  strategy_ranger = returns * shift(signalranger),
  strategy_xgboost = returns * shift(signalxgboost),
  strategy_nnet = returns * shift(signalnnet),
  strategy_glmnet = returns * shift(signalglmnet),
  strategy_sum = returns * shift(signalres_sum)
)]
cols = c("date", "returns", 
         "strategy_bart",
         "strategy_ranger", "strategy_xgboost",
         "strategy_nnet", "strategy_glmnet",
         "strategy_sum", "signalranger", "signalxgboost",
         "signalnnet", "signalglmnet", "signalres_sum")
dt_back = dt_back[, ..cols]
dt_back = na.omit(dt_back)

# Portfolio performance
xts_ = as.xts.data.table(dt_back[, .SD, .SDcols = 1:8])
cumRetx = Return.cumulative(xts_)
# annRetx = Return.annualized(xts_, scale=12)
# sharpex = SharpeRatio.annualized(xts_, scale=12)
# winpctx = as.matrix(as.data.table(lapply(xts_, function(x) length(x[x > 0])/length(x[x != 0]))))
# rownames(winpctx) = "Win rate"
# annSDx = sd.annualized(xts_, scale=12)
DDs = lapply(xts_, findDrawdowns)
maxDDx = as.matrix(as.data.table(lapply(DDs, function(x) min(x$return))))
rownames(maxDDx) = "Max DD"
# maxLx = as.matrix(as.data.table(lapply(DDs, function(x) max(x$length))))
# rownames(maxLx) = "Max DD days"
# res = rbind(cumRetx, annRetx, sharpex, winpctx, annSDx, maxDDx, maxLx)
res = rbind(cumRetx, maxDDx)
res = as.data.table(res, keep.rownames = "var")

# visualize histogram of perfromances by signal_sum and add vertical line for benchmark
melt(res[var == "Cumulative Return"], id.vars = "var") |>
  ggplot(aes(value)) +
  geom_histogram() +
  geom_vline(xintercept = benchmark_performance$returns, color = "red")

# Backtests
charts.PerformanceSummary(xts_)


# ENSAMBLE METHODS --------------------------------------------------------
# ensamble for one month
predictions_ensamble = backtest_dt[, .(month, learner, response)]
predictions_ensamble = predictions_ensamble[, .(response = mean(response, na.rm = TRUE)), by = month]
predictions_ensamble[, signal_strat := response >= 0]
dt_back_ansambl = merge(dtm, predictions_ensamble, by = "month", all.x = TRUE, all.y = FALSE)
dt_back_ansambl = dt_back_ansambl[, `:=`(strategy = returns * shift(signal_strat))]
cols = c("month", "returns", "strategy")
dt_back_ansambl = dt_back_ansambl[, ..cols]
dt_back_ansambl = na.omit(dt_back_ansambl)
charts.PerformanceSummary(as.xts.data.table(dt_back_ansambl))


# QC PREPARE --------------------------------------------------------------
# preapre data or QC and save
qc_data = predictions_wide[, .(month, strategy = signal_nnet)]
qc_data = na.omit(qc_data)
qc_data[, sum(abs(diff(strategy)))] # number of trades
endpoint = "https://snpmarketdata.blob.core.windows.net/"
BLOBENDPOINT = storage_endpoint(endpoint, key="0M4WRlV0/1b6b3ZpFKJvevg4xbC/gaNBcdtVZW+zOZcRi0ZLfOm1v/j2FZ4v+o8lycJLu1wVE6HT+ASt0DdAPQ==")
cont = storage_container(BLOBENDPOINT, "qc-backtest")
storage_write_csv(qc_data, cont, "gold_forecasts.csv")
