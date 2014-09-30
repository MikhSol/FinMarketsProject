library(quantmod)
library(MASS)

tickers <- "^DJIA"

symbs <- getSymbols(tickers, src="yahoo", from = "2011-01-01", to = Sys.Date())