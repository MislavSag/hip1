library(data.table)
library(arrow)
library(duckdb)
library(lubridate)


# Import SPY data using duckdb
get_symbol = function(symbol) {
  con <- dbConnect(duckdb::duckdb())
  query <- sprintf("
  SELECT *
  FROM 'F:/databento/minute.parquet'
  WHERE Symbol = '%s'
", symbol)
  data_ <- dbGetQuery(con, query)
  dbDisconnect(con)
  setDT(data_)
  return(na.omit(data_))
}
spy = get_symbol("SPY")
spy[, ts_event := with_tz(ts_event, tz = "America/New_York")]

# Filter trading minutes
spy = spy[as.ITime(ts_event) %between% c(as.ITime("09:30:00"), as.ITime("16:00:00"))]

# Keep columns we need
spy = spy[, .(date = ts_event, open, high, low, close, volume)]

# Upsample to 5 minute frequency
spy = spy[, .(
  open = first(open),
  high = max(high),
  low = min(low),
  close = last(close),
  volume = sum(volume)
), by = .(date = round_date(date, "5 mins"))]
gc()

# Save
fwrite(spy, "spy-ohlcv.csv")
