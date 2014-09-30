
library(quantmod)
library(MASS)

### We will need some packages
#install.packages("VGAM")
#install packages("tseries")

source("plfit.r")

############################################################
# Power-law fit
############################################################

######### 1. Get the data from Yahoo! Finance

getSymbols("AAPL", src="yahoo", from = "2009-01-01", to = Sys.Date())

p <- Ad(AAPL)
r <- diff(log(p)) 

#remove NA values
r <- na.omit(r)

######### 2. Ranking plot

# Losses
losses <- -r[r<0]
losses <- as.numeric(losses)

t_ret <- sort(losses)
t_rank <- (length(losses):1)/length(losses)

#dev.new()
#plot(t_ret, t_rank)

dev.new()
plot(t_ret, t_rank, log = "xy")

######### 3. Fitting plower law tail

res <- plfit(losses)

alpha <- res$alpha - 1  #NB! differences in definition of alpha 
xmin <- res$xmin

#theoretical distribution
tr <- seq(from=0, to=0.3, by=0.01)
tf <- (xmin/tr)^alpha

#plotting
abline(v = rmin, col = "red")
lines(tr, tf, col="red")

#accounting for the rmin
coef <- sum(losses>xmin)/length(losses)
lines(tr, coef*tf, col="red")

######### 4. Fitting Student-t distribution

r <- diff(log(p)) 
r <- na.omit(r)
returns <- as.numeric(r)

tpars <- fitdistr(returns, "t")

mydt <- function(x, m, s, df) dt((x-m)/s, df)/s
mypt <- function(x, m, s, df) pt((x-m)/s, df)

#plotting
t_ret <- sort(returns)
t_rank <- (length(returns):1)/length(returns)

t_stud_pdf <- mydt(t_ret, tpars$estimate[1], tpars$estimate[2], tpars$estimate[3])
t_stud_ccdf <- 1-mypt(t_ret, tpars$estimate[1], tpars$estimate[2], tpars$estimate[3])

dev.new()
plot(t_ret, t_rank, log = "xy")
lines(t_ret, t_stud_ccdf, col="red")

######### 5. Calculating VaR

my_qt <- function(p, m, s, df) qt(p,df)*s + m

VaR_90 <- quantile(returns, 0.1)
VaR_95 <- quantile(returns, 0.05)
VaR_99 <- quantile(returns, 0.01)

mu <- mean(returns)
sigma <- sd(returns)

VaR_90_norm <- qnorm(0.1, mean = mu, sd = sigma)
VaR_95_norm <- qnorm(0.05, mean = mu, sd = sigma)
VaR_99_norm <- qnorm(0.01, mean = mu, sd = sigma)

VaR_90_t <- my_qt(0.1, tpars$estimate[1], tpars$estimate[2], tpars$estimate[3])
VaR_95_t <- my_qt(0.05, tpars$estimate[1], tpars$estimate[2], tpars$estimate[3])
VaR_99_t <- my_qt(0.01, tpars$estimate[1], tpars$estimate[2], tpars$estimate[3])

print(c(VaR_90, VaR_90_norm, VaR_90_t))
print(c(VaR_95, VaR_95_norm, VaR_95_t))
print(c(VaR_99, VaR_99_norm, VaR_99_t))

######### 6. Calculating ES

#library(PerformanceAnalytics)
#ES(returns, p=0.95,method="historical")
#ES(returns, p=0.95,method="gaussian")

ES_90 <- mean(r[r<VaR_90])
ES_95 <- mean(r[r<VaR_95])
ES_99 <- mean(r[r<VaR_99])

ff <- function(x) x*dnorm(x, mu, sigma)

ES_90_norm <- integrate(ff, lower=-Inf, upper=VaR_90_norm)$value / 0.1
ES_95_norm <- integrate(ff, lower=-Inf, upper=VaR_95_norm)$value / 0.05
ES_99_norm <- integrate(ff, lower=-Inf, upper=VaR_99_norm)$value / 0.01

ff <- function(x) x*mydt(x, tpars$estimate[1], tpars$estimate[2], tpars$estimate[3])

ES_90_t <- integrate(ff, lower=-Inf, upper=VaR_90_t)$value / 0.1
ES_95_t <- integrate(ff, lower=-Inf, upper=VaR_95_t)$value / 0.05
ES_99_t <- integrate(ff, lower=-Inf, upper=VaR_99_t)$value / 0.01

print(c(ES_90, ES_90_norm, ES_90_t))
print(c(ES_95, ES_95_norm, ES_95_t))
print(c(ES_99, ES_99_norm, ES_99_t))

######### 7. Drawdowns

library(tseries)

p <- as.numeric(p)
mdd <- maxdrawdown(p)
mdd

plot(p,type='l')
segments(time(p)[mdd$from], p[mdd$from],
         time(p)[mdd$to], p[mdd$from], col="grey")
segments(time(p)[mdd$from], p[mdd$to],
         time(p)[mdd$to], p[mdd$to], col="grey")
mid <- time(p)[(mdd$from + mdd$to)/2]
arrows(mid, p[mdd$from], mid, p[mdd$to], col="red", length = 0.16)



