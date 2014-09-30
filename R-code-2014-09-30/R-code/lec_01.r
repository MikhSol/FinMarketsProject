
library(quantmod)
library(MASS)


######### 1. Get the data from Yahoo! Finance

getSymbols("AAPL", src="yahoo", from = "2005-01-01", to = Sys.Date())

######### 1. Calculate log-returns

p <- Ad(AAPL)
r <- diff(log(p)) 

#remove NA values
temp <- rowSums(is.na(r)) == (NCOL(r))
r <- r[!temp]

######### 2. Plot monthly price and daily log-returns

chartSeries(to.monthly(AAPL),up.col='white',dn.col='blue')

dev.new()
plot(r)

######### 3. fit Gaussian distribution

mu <- mean(r)
sigma <- sd(r) 

print(mu)
print(sigma)

# the same results using Maximum Likelihood
params<-fitdistr(r,"normal")
print(params$estimate)

######### 4. calculate histogram

hist(r, breaks=200, freq=FALSE) #NB! we need to plot Density (Freq=FALSE) and not just frequency

######### 5. plot gaussian distribution

x <- seq(from=min(r),to=max(r),by=0.001)
f <- 1/(sqrt(2*pi*sigma^2)) * exp(-(x-mu)^2/(2*sigma^2))

#dev.new()
#plot(pdf$mids, pdf$density)
lines(x,f,col='red')

######### 6. annualized volatility

vol <- sd(r) * sqrt(252)
print(vol)

######### 7. R&S measure of daily volatility

h = log(Hi(AAPL)/Op(AAPL))
l = log(Lo(AAPL)/Op(AAPL))
c = log(Cl(AAPL)/Op(AAPL))

vol_RS <- sqrt(h*(h-c)+l*(l-c))

dev.new()
plot(vol_RS)

######### 8. VaR

#=== Historical simulation
VaR_90 <- quantile(r, 0.1)
VaR_95 <- quantile(r, 0.05)
VaR_99 <- quantile(r, 0.01)

#=== Variance-Covariance
VaR_90_norm <- qnorm(0.1, mean = mu, sd = sigma)
VaR_95_norm <- qnorm(0.05, mean = mu, sd = sigma)
VaR_99_norm <- qnorm(0.01, mean = mu, sd = sigma)

print(c(VaR_90,VaR_95,VaR_99))
print(c(VaR_90_norm,VaR_95_norm,VaR_99_norm))

######### 9. ES

ES_90 <- mean(r[r<VaR_90])
ES_95 <- mean(r[r<VaR_95])
ES_99 <- mean(r[r<VaR_99])

############################################################
# Correlations
############################################################

######### 10. fetch data from Yahoo

getSymbols(c("AAPL","^IXIC"), src="yahoo", from = "2009-01-01", to = Sys.Date())

p <- cbind(Ad(AAPL),Ad(IXIC))
r <- diff(log(p))

temp <- rowSums(is.na(r)) == (NCOL(r)) 
r <- r[!temp,]

######### 11. calculate pearson correlation

c_pearson <- cor(r, method="pearson")
c_spearman <- cor(r, method="spearman")
c_kendall <- cor(r, method="kendall")

print(c_pearson)
print(c_spearman)
print(c_kendall)

dev.new()
plot(as.numeric(r$AAPL),as.numeric(r$IXIC))
lines(x, x*c_pearson[1,2], col='red')
lines(x, x*c_spearman[1,2], col='blue')
lines(x, x*c_kendall[1,2], col='green')

######### 11a. Add an outlier

r_new <- r
r_new[90,1]=+0.45

c_pearson <- cor(r_new, method="pearson")
c_spearman <- cor(r_new, method="spearman")
c_kendall <- cor(r_new, method="kendall")

print(c_pearson)
print(c_spearman)
print(c_kendall)

dev.new()
plot(as.numeric(r$AAPL),as.numeric(r$IXIC))
lines(x, x*c_pearson[1,2], col='red')
lines(x, x*c_spearman[1,2], col='blue')
lines(x, x*c_kendall[1,2], col='green')


######### 12. calculate correlation matrix

tickers   <- c("AAPL","IBM", "INTC","CSCO","GOOG","T","VZ")
getSymbols(tickers, src="yahoo", from = "2009-01-01", to = Sys.Date())

p <- cbind(Cl(AAPL),Cl(IBM),Cl(INTC),Cl(CSCO),Cl(GOOG),Cl(T),Cl(VZ))
r <- diff(log(p))

temp <- rowSums(is.na(r)) == (NCOL(r)) 
r <- r[!temp,]

c_spearman <- cor(r, method="spearman")
print(c_spearman)

library(lattice)
dev.new()
rgb.palette <- colorRampPalette(c("red","white","blue"))
levelplot(c_spearman, col.regions=rgb.palette)

######### 12a. corrgram 
library(corrgram)
rrr <- cbind(as.numeric(r[,1]),as.numeric(r[,2]),as.numeric(r[,3]),as.numeric(r[,4]),as.numeric(r[,5]),as.numeric(r[,6]))

dev.new()
corrgram(rrr, lower.panel=panel.shade, upper.panel=panel.pts)

###########################
#library(PerformanceAnalytics)
#chart.Correlation(r)

