# stock_functions.R
# Utility functions for Assignment 2: load stock data and compute statistics.
# Packages: quantmod (data import), TTR (moving averages)

library(quantmod)
library(TTR)

# Read ticker symbols from a text file, one per line.
read_portfolio <- function(path = "portfolio.txt") {
  symbols <- trimws(readLines(path, warn = FALSE))
  symbols[symbols != ""]
}

# Import daily stock data for every symbol in portfolio.txt.
# Returns a named list with one data frame per symbol.
load_stock_data <- function(path = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date()) {
  symbols <- read_portfolio(path)
  stocks <- list()
  for (sym in symbols) {
    xts_data <- tryCatch(
      getSymbols(sym, src = "yahoo", from = from, to = to, auto.assign = FALSE),
      error = function(e) {
        warning("Could not download ", sym, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(xts_data)) next
    df <- data.frame(Date = index(xts_data), coredata(xts_data))
    colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    stocks[[sym]] <- df
  }
  stocks
}

# Mode of a numeric vector (most frequent value, rounded to 2 decimals).
get_mode <- function(x) {
  x <- round(x, 2)
  counts <- table(x)
  as.numeric(names(counts)[which.max(counts)])
}

# Moving average of the Close price (simple, window n).
moving_average <- function(df, n = 20) {
  as.numeric(SMA(df$Close, n = n))
}

# Compute basic statistics for one stock data frame.
calculate_statistics <- function(df, ma_window = 20) {
  ma <- moving_average(df, ma_window)
  data.frame(
    Observations = nrow(df),
    Mean = mean(df$Close, na.rm = TRUE),
    Median = median(df$Close, na.rm = TRUE),
    Mode = get_mode(df$Close),
    StdDev = sd(df$Close, na.rm = TRUE),
    MovingAvg = tail(ma[!is.na(ma)], 1),
    MA_Window = ma_window
  )
}

# Compute statistics for every stock; returns one combined data frame.
calculate_all_statistics <- function(stocks, ma_window = 20) {
  rows <- lapply(names(stocks), function(sym) {
    cbind(Symbol = sym, calculate_statistics(stocks[[sym]], ma_window))
  })
  do.call(rbind, rows)
}
