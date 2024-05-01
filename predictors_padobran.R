library(data.table)
library(finfeatures)
library(reticulate)


# python environment
# reticulate::use_virtualenv("C:/Users/Mislav/projects_py/pyquant", required = TRUE)
tsfel = reticulate::import("tsfel", convert = FALSE)
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
# i = 100

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

# # Exuber
# path_ =   create_path("exuber")
# exuber_init = RollingExuber$new(
#   windows = c(400, 400*2),
#   workers = 4L,
#   at = at,
#   lag = 0L,
#   exuber_lag = 1L
# )
# system.time({
#   exuber = exuber_init$get_rolling_features(ohlcv, TRUE)
# })
# fwrite(exuber, path_)
# 
# # Backcusum
# path_ = create_path("backcusum")
# backcusum_init = RollingBackcusum$new(
#   windows = c(200),
#   workers = 4L,
#   at = at,
#   lag = 0L,
#   alternative = c("greater", "two.sided"),
#   return_power = c(1, 2))
# backcusum = backcusum_init$get_rolling_features(ohlcv)
# fwrite(backcusum, path_)
# 
# # Theft r
# path_ = create_path("theftr")
# theft_init = RollingTheft$new(
#   windows = 400,
#   workers = 4L,
#   at = at,
#   lag = 0L,
#   features_set = c("catch22", "feasts"))
# theft_r = theft_init$get_rolling_features(ohlcv)
# fwrite(theft_r, path_)

# Theft py
path_ = create_path("theftpy")
theft_init = RollingTheft$new(
  windows = 400,
  workers = 1L,
  at = at,
  lag = 0L,
  features_set = c("tsfel"))
theft_r = theft_init$get_rolling_features(ohlcv)
fwrite(theft_r, path_)

