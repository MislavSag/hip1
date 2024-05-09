library(data.table)
library(AzureStor)
library(finfeatures)


# DATA --------------------------------------------------------------------
# Import OHLCV data for SPY
spy = fread("spy-ohlcv.csv")

# import predictors generated on padobran
import_predictors = function(id = "exuber") {
  files = list.files("F:/predictors/spyml_predictors", full.names = TRUE, pattern = id)
  predictors = lapply(files, fread)
  predictors = rbindlist(predictors, fill = TRUE)
  predictors[, let(symbol = NULL)] 
  return(predictors)
}
ids = c("exuber", "backcusum", "theftr", "theftpy", "forecasts", "tsfeatures",
        "waveletarima", "fracdiff")
predictors = lapply(ids, import_predictors)
predictors = Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), 
                    predictors)
setorder(predictors, date)

# Fix column names
# TODO: Add this to finfeatures package
setnames(predictors, gsub('\"|\\(|\\)|,', '', colnames(predictors)))

# Merge ohlcv and predictors
dt = merge(spy, predictors, by = "date", all.x = TRUE, all.y = FALSE)

# Generate OHLCV predictors
ohlcv = Ohlcv$new(spy[, .(symbol = "SPY", date, open, high, low, close, volume)])
ohlcv_predictors_init = OhlcvFeaturesDaily$new(
  windows = c(12, 24, 48, 78, 78*5, 78*10, 78*22),
  quantile_divergence_window = c(78*5, 78*10, 78*22)
)
ohlcv_predictors = ohlcv_predictors_init$get_ohlcv_features(copy(ohlcv$X))

# Merge ohlcv and predictors
cols_remove = c("symbol", "open", "high", "low", "close", "volume", "returns")
ohlcv_predictors = ohlcv_predictors[, .SD, .SDcols = -cols_remove]
dt = merge(dt, ohlcv_predictors, by = "date", all.x = TRUE, all.y = FALSE)

# Check data table with all predictors
dim(dt)
dt[, 1:10]

# Check number of missing values
cols_na_sum = dt[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = colnames(dt)]
cols_na_sum = melt(cols_na_sum, id.vars = "date")
cols_na_sum[value > 2000]


# TARGETS -----------------------------------------------------------------
# Simple future returns as targets
setorder(dt, date)
dt[, target_ret_1 := shift(close, -1, type = "shift") / close -1]
dt[, target_ret_6 := shift(close, -6, type = "shift") / close -1]
dt[, target_ret_12 := shift(close, -12, type = "shift") / close -1]


# CLEAN DATA --------------------------------------------------------------
# Convert columns to numeric. This is important only if we import existing features
chr_to_num_cols = colnames(dt[, .SD, .SDcols = is.character])
if (length(chr_to_num_cols) > 0) {
  dt = clf_data[, (chr_to_num_cols) := lapply(.SD, as.numeric), .SDcols = chr_to_num_cols]  
}
log_to_num_cols = colnames(dt[, .SD, .SDcols = is.logical])
dt[, (log_to_num_cols) := lapply(.SD, as.numeric), .SDcols = log_to_num_cols]

# Remove duplicates
dup_test  = anyDuplicated(dt, by = "date")
if (dup_test != 0L) {
  dt = unique(dt, by = "date")
}

# Remove columns with many NA
keep_cols = names(which(colMeans(!is.na(dt)) > 0.5))
print(paste0("Removing columns with many NA values: ", 
             setdiff(colnames(dt), c(keep_cols, "right_time"))))
dt = dt[, .SD, .SDcols = keep_cols]

# Remove Inf and Nan values if they exists
is.infinite.data.frame = function(x) do.call(cbind, lapply(x, is.infinite))
keep_cols = names(which(colMeans(!is.infinite(as.data.frame(dt))) > 0.98))
print(paste0("Removing columns with Inf values: ", setdiff(colnames(dt), keep_cols)))
dt = dt[, .SD, .SDcols = keep_cols]

# Remove inf values
n_0 = nrow(dt)
dt = dt[is.finite(rowSums(dt[, .SD, .SDcols = is.numeric], na.rm = TRUE))]
n_1 = nrow(dt)
print(paste0("Removing ", n_0 - n_1, " rows because of Inf values"))

# Final checks
dt[, .(min_date = min(date), max_date = max(date), .N)]

# Define predictors
cols_non_features = c("date", "open", "high", "low", "close", "volume")
targets = c("target_ret_1", "target_ret_6", "target_ret_12")
cols_features = setdiff(colnames(dt), c(cols_non_features, targets))

# Change feature and targets columns names due to lighgbm
snakeToCamel = function(snake_str) {
  # Replace underscores with spaces
  spaced_str = gsub("_", " ", snake_str)
  
  # Convert to title case using tools::toTitleCase
  title_case_str = tools::toTitleCase(spaced_str)
  
  # Remove spaces and make the first character lowercase
  camel_case_str = gsub(" ", "", title_case_str)
  camel_case_str = sub("^.", tolower(substr(camel_case_str, 1, 1)), camel_case_str)
  
  # I haeve added this to remove dot
  camel_case_str = gsub("\\.", "", camel_case_str)
  
  return(camel_case_str)
}
cols_features_new = vapply(cols_features, snakeToCamel, FUN.VALUE = character(1L), USE.NAMES = FALSE)
setnames(dt, cols_features, cols_features_new)
cols_features = cols_features_new
targets_new = vapply(targets, snakeToCamel, FUN.VALUE = character(1L), USE.NAMES = FALSE)
setnames(dt, targets, targets_new)
targets = targets_new

# Remove constant columns in set
features_ = dt[, ..cols_features]
remove_cols = colnames(features_)[apply(features_, 2, var, na.rm=TRUE) == 0]
print(paste0("Removing feature with 0 standard deviation: ", remove_cols))
dt = dt[, .SD, .SDcols = -remove_cols]
cols_features = setdiff(cols_features, remove_cols)

# Convert variables with low number of unique values to factors
int_numbers = na.omit(dt[, ..cols_features])[, lapply(.SD, function(x) all(floor(x) == x))]
int_cols = colnames(dt[, ..cols_features])[as.matrix(int_numbers)[1,]]
factor_cols = dt[, ..int_cols][, lapply(.SD, function(x) length(unique(x)))]
factor_cols = as.matrix(factor_cols)[1, ]
factor_cols = factor_cols[factor_cols <= 100]
dt = dt[, (names(factor_cols)) := lapply(.SD, as.factor), .SD = names(factor_cols)]

# Sort
setorder(dt, date)

# Save features
last_date = strftime(dt[, max(date)], "%Y%m%d")
file_name = paste0("spyml-predictors-", last_date, ".csv")
file_name_local = fs::path("F:/predictors/spyml_dt", file_name)
fwrite(dt, file_name_local)
 
# Save to Azure blob
endpoint = "https://snpmarketdata.blob.core.windows.net/"
blob_key = readLines('./blob_key.txt')
BLOBENDPOINT = storage_endpoint(endpoint, key=blob_key)
cont = storage_container(BLOBENDPOINT, "jphd")
storage_write_csv(dt, cont, file_name)

# TODO: Check this columns in finfetures. They have lots of missing values
# [1] "tsfresh_values__query_similarity_count__query_None__threshold_0_0_400"
# [2] "d_fdGPH_0_1_400"
# [3] "sd_reg_fdGPH_0_1_400"
# [4] "d_fdSperio_0_1_400"
# [5] "sd_reg_fdSperio_0_1_400"
