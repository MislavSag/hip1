library(data.table)
library(finfeatures)
library(reticulate)


# python environment
# reticulate::use_virtualenv("C:/Users/Mislav/projects_py/pyquant", required = TRUE)
# theftms::init_theft("/opt/venv")
reticulate::use_virtualenv("/opt/venv")
tsfel = reticulate::import("tsfel")
# tsfresh = reticulate::import("tsfresh", convert = FALSE)
warnigns = reticulate::import("warnings", convert = FALSE)
warnigns$filterwarnings('ignore')

# paths
PATH_PREDICTORS = file.path("./predictors")

# Create directory if it doesnt exists
if (!dir.exists(PATH_PREDICTORS)) {
  dir.create(PATH_PREDICTORS)
}

# Get index
i = as.integer(Sys.getenv('PBS_ARRAY_INDEX'))
# i = 1

# Get SPY OHLCV data
spy = fread("spy-ohlcv.csv")

# Divide data on 1000 rows
split_vector_into_chunks <- function(vector, chunks = 1000) {
  n <- length(vector)
  
  # Number of elements in most chunks
  base_size <- floor(n / chunks)
  
  # Calculate how many chunks need one extra element to account for the remainder
  remainder <- n %% chunks
  extra_sizes <- ifelse(seq_len(chunks) <= remainder, 1, 0)
  
  # Total elements per chunk
  chunk_sizes <- base_size + extra_sizes
  
  # Create the breaks for the chunks
  breaks <- c(0, cumsum(chunk_sizes))
  
  # Split the vector into chunks
  subsamples <- vector("list", length = chunks)
  for (i in seq_len(chunks)) {
    subsamples[[i]] <- vector[length = chunk_sizes[i]]
    subsamples[[i]] <- vector[(breaks[i] + 1):breaks[i + 1]]
  }
  
  return(subsamples)
}
at_sets = split_vector_into_chunks(1:nrow(spy), chunks = 1000)
at = at_sets[[i]]

# Create Ohlcv object
ohlcv = Ohlcv$new(spy[, .(symbol = "SPY", date, open, high, low, close, volume)])

# utils
create_path = function(name) {
  file.path(PATH_PREDICTORS, paste0(name, "-", i, ".csv"))
}
workers = 1L

# Exuber
path_ =   create_path("exuber")
windows = c(400, 400*2)
if (max(at) > min(windows)) {
  exuber_init = RollingExuber$new(
    windows = c(400, 400*2),
    workers = workers,
    at = at,
    lag = 0L,
    exuber_lag = 1L
  )
  exuber = exuber_init$get_rolling_features(ohlcv, TRUE)
  fwrite(exuber, path_)
}

# Backcusum
path_ = create_path("backcusum")
windows = 200
if (max(at) > min(windows)) {
  backcusum_init = RollingBackcusum$new(
    windows = windows,
    workers = workers,
    at = at,
    lag = 0L,
    alternative = c("greater", "two.sided"),
    return_power = c(1, 2))
  backcusum = backcusum_init$get_rolling_features(ohlcv)
  fwrite(backcusum, path_) 
}

# Theft r
path_ = create_path("theftr")
windows = 400
if (max(at) > min(windows)) {
  theft_init = RollingTheft$new(
    windows = windows,
    workers = workers,
    at = at,
    lag = 0L,
    features_set = c("catch22", "feasts"))
  theft_r = theft_init$get_rolling_features(ohlcv)
  fwrite(theft_r, path_)
}

# Theft py
path_ = create_path("theftpy")
windows = 400
if (max(at) > min(windows)) {
  theft_init = RollingTheft$new(
    windows = windows,
    workers = 1L,
    at = at,
    lag = 0L,
    features_set = c("tsfel", "tsfresh"))
  theft_py = suppressMessages(theft_init$get_rolling_features(ohlcv))
  fwrite(theft_py, path_)
}

# Forecasts
path_ = create_path("forecasts")
windows = 400
if (max(at) > min(windows)) {
  forecasts_init = RollingForecats$new(
    windows = windows,
    workers = workers,
    at = at,
    lag = 0L,
    forecast_type = c("autoarima", "nnetar", "ets"),
    h = 22)
  forecasts = suppressMessages(forecasts_init$get_rolling_features(ohlcv))
  fwrite(forecasts, path_)
}
  
# Tsfeatures
path_ = create_path("tsfeatures")
windows = 400
if (max(at) > min(windows)) {
  tsfeatures_init = RollingTsfeatures$new(
    windows = windows,
    workers = workers,
    at = at,
    lag = 0L,
    scale = TRUE)
  tsfeatures = suppressMessages(tsfeatures_init$get_rolling_features(ohlcv))
  fwrite(tsfeatures, path_)
}

# WaveletArima
path_ = create_path("waveletarima")
windows = 400
if (max(at) > min(windows)) {
  waveletarima_init = RollingWaveletArima$new(
    windows = windows,
    workers = workers,
    at = at,
    lag = 0L,
    filter = "haar")
  waveletarima = suppressMessages(waveletarima_init$get_rolling_features(ohlcv))
  fwrite(waveletarima, path_)
}

# FracDiff
path_ = create_path("fracdiff")
windows = 400
if (max(at) > min(windows)) {
  fracdiff_init = RollingFracdiff$new(
    windows = windows,
    workers = workers,
    at = at,
    lag = 0L,
    nar = c(1), 
    nma = c(1),
    bandw_exp = c(0.1, 0.5, 0.9))
  fracdiff = suppressMessages(fracdiff_init$get_rolling_features(ohlcv))
  fwrite(fracdiff, path_)
}