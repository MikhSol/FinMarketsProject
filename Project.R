#########################################################################
### Problem #3
### For the 30 stocks that constitute the Dow Jones Industrial Average
### (Yahoo: ^DJIA) at the present day: using daily data, estimate the 
### cross correlation over the following periods: 2006-2007, 2008-2009,
### 2010-2011 and 2012-2013. In all four periods select two stocks that
### have minimal correlation, construct the equally-weighted portfolio
### of them and calculate the Sharpe ratio the portfolio.
#########################################################################

###Proxy settings
Sys.setenv(http_proxy="http://proxy.prognoz.ru:8080")
Sys.getenv("http_proxy")

###Upload libraries
library("MASS")
library("quantmod")

###Data downloading
tickers <- c("AXP", "BA", "CAT", "CSCO", "CVX", "DD", "DIS", "GE", "GS", "HD",
 "IBM", "INTC", "JNJ", "JPM", "KO", "MCD", "MMM", "MRK", "MSFT", "NKE", "PFE",
 "PG", "T", "TRV", "UNH", "UTX", "V", "VZ", "WMT", "XOM")

symbs <- getSymbols(tickers, src="yahoo", from="2011-01-01", to=Sys.Date())

###
symbs




