library(data.table)
library(gausscov)
library(fs)
library(runner)
library(glue)
library(PerformanceAnalytics)
library(lubridate)
library(AzureStor)


# SETUP -------------------------------------------------------------------
# defin paths
RESULTS = "F:/strategies/spyml"
if (!dir_exists("RESULTS")) dir_create(RESULTS)


# DATA --------------------------------------------------------------------
# read predictors
pead_file_local = list.files("F:/predictors/spyml_dt", full.names = TRUE)
dates = as.Date(gsub(".*-", "", path_ext_remove(path_file(pead_file_local))),
                format = "%Y%m%d")
DT = fread(pead_file_local[which.max(dates)])

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

# Check timezone
attributes(DT$date)


# GAUSSCOV ----------------------------------------------------------------
# Prepare data
# cols_predictors_new = paste0("diff_", cols_predictors)
# dt[, (cols_predictors_new) := lapply(.SD, function(x) c(NA, diff(x))), .SDcols = cols_predictors]
# cols_predictors_pct = paste0("pct_", cols_predictors)
# dt[, (cols_predictors_pct) := lapply(.SD, function(x) x / shift(x) - 1), .SDcols = cols_predictors]
# cols_predictors_all = c(cols_predictors, cols_predictors_new, cols_predictors_pct)
# dt = na.omit(dt)

# Check gausscov functions
X = as.matrix(DT[, .SD, .SDcols = cols_features])
y = as.matrix(DT[, .(targetRet1)])
plot(y)
system.time({f1st_res = f1st(y = y, x = X, p0 = 0.001)})
colnames(DT)[f1st_res[[1]][, 1]]

# Rolling f1st
f1st_res_l_roll = runner(
  x = DT,
  f = function(x) {
    # x = DT[50000:55000]
    cbind.data.frame(
      date = x[, last(date)],
      f1st(y = as.matrix(x[, .(targetRet1)]),
           x = as.matrix(x[, ..cols_features]),
           p0 = 0.05)[[1]]
    )
    # colnames(x)[c(2119, 2290)]
  },
  k = 7 * 22 * 36,
  na_pad = TRUE,
  simplify = FALSE
)
saveRDS(f1st_res_l_roll, file.path(RESULTS, glue("f1st_res_l_roll.rds")))

# Expanding f1st
system.time({
  f1st_res_l = runner(
    x = dt,
    f = function(x) {
      cbind.data.frame(
        date = x[, last(date)],
        f1st(y = as.matrix(x[, .(y)]),
             x = as.matrix(x[, .SD, .SDcols = -c("date", "y", "close", "returns")]),
             p0 = 0.6)[[1]]
      )
    },
    at = (7 * 22 * 6):nrow(dt),
    na_pad = TRUE,
    simplify = FALSE
  )
})
# saveRDS(f1st_res_l, file.path(RESULTS, glue("f1st_res_l.rds")))

# Import results
file_ = file.path(RESULTS, glue("f1st_res_l_roll.rds"))
f1st_res_l = readRDS(file_)
lengths(f1st_res_l)
last(lengths(f1st_res_l), 100)
f1st_res_l = f1st_res_l[lengths(f1st_res_l) > 1]
f1st_res_dt = rbindlist(f1st_res_l)
setnames(f1st_res_dt, c("date", "col_ind", "reg_coeff", "p", "standard_p"))
columns_dt = data.table(col_ind = seq_along(colnames(DT)), cols = colnames(DT))
f1st_res_dt = columns_dt[f1st_res_dt, on = "col_ind"]

# Calculate rolling regressions
dates = f1st_res_dt[, unique(date)]
min_date = DT[, min(date)]
predictions = vector("numeric", length(dates))
for (i in seq_along(dates)) {
  # i = 1
  sample_dt = DT[date %between% c(min_date, dates[i]),
                 .SD,
                 .SDcols = c("targetRet1", na.omit(f1st_res_dt[date == dates[i]]$cols))]
  setDF(sample_dt)
  predictions[i] = last(predict(lm(sample_dt, formula = targetRet1 ~ .)))
}

# Merge predictions and dates and than predictions and dt
predictions_dt = data.table(date = dates, predictions = predictions)
predictions_dt = merge(DT[, .(date, close)], predictions_dt, by = "date")

# Backtest
predictions_dt[, signal := predictions > -0.00001]
predictions_dt[, spy := close / shift(close) - 1]
predictions_dt[, strategy := spy * shift(signal, 2)]
backtest_xts = as.xts.data.table(predictions_dt[, .(date, spy, strategy)])
charts.PerformanceSummary(backtest_xts)
charts.PerformanceSummary(backtest_xts[1:20000])
charts.PerformanceSummary(backtest_xts[20001:40000])
charts.PerformanceSummary(backtest_xts[25001:30000])
charts.PerformanceSummary(backtest_xts[40001:60000])
charts.PerformanceSummary(backtest_xts[60001:80000])
charts.PerformanceSummary(backtest_xts[80001:nrow(backtest_xts)])

# Important predictors by frequency
first(f1st_res_dt[!is.na(cols), .N, by = cols][order(-N)], 20)

# Important predictors by frequency as set
importance_group = f1st_res_dt[!is.na(cols)][, list(list(cols)), by = date]

# number of same lements in list column V1 of data table

# Save for Quantconnect
qc_data = predictions_dt[, .(date = with_tz(date, tzone = "America/New_York"), signal)]
qc_data = na.omit(qc_data)
qc_data[, let(
  date = as.character(date),
  signal = as.integer(signal)
)]
# bl_endp_key = storage_endpoint(
#   Sys.getenv("BLOB-ENDPOINT-SNP"), 
#   Sys.getenv("BLOB-KEY-SNP"))
bl_endp_key = storage_endpoint(
  "https://snpmarketdata.blob.core.windows.net", 
  "0M4WRlV0/1b6b3ZpFKJvevg4xbC/gaNBcdtVZW+zOZcRi0ZLfOm1v/j2FZ4v+o8lycJLu1wVE6HT+ASt0DdAPQ==")
cont = storage_container(bl_endp_key, "qc-backtest")
storage_write_csv(qc_data, cont, "mlspy.csv", col_names = FALSE)
