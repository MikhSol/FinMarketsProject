#########################################################################
###Problem #4
###Construct the minimum variance portfolio and tangent portfolio using 15 best
###performing stocks in US at the present day (use Yahoo! Finance: Market Data 
###-> Market Stats -> Market Movers -> % Gainers). Assume that short-selling is
###allowed. For the historical data consider the horizon of the last 2 years. 
###Estimate the tail exponent for the returns of this portfolio. Compare with 
###the tail exponent of the S&P 500 index considered at the same horizon of 2 years.
#########################################################################

###Proxy settings
#Sys.setenv(http_proxy="http://proxy.prognoz.ru:8080")
#Sys.getenv("http_proxy")


install.packages("VGAM")
install.packages("tseries")

source("plfit.r")


###Upload libraries
library("MASS")
library("quantmod")
library("lattice")
library("corrgram")
library("PerformanceAnalytics")

###Data downloading
tickers = c("LSBI", "KELYB", "NQ", "THRD", "VNDA", "TOPS", "DRWI",
            "HOTR", "CLDN", "PL", "ANAD", "OMEX", "WAVX", "ASBCW", "JRJC")

symbs = getSymbols(tickers, src="yahoo", from="2012-05-01", to="2014-05-01")
getSymbols("^GSPC", src="yahoo", from="2012-05-01", to="2014-05-01")

###Returns calculation
p = cbind()
for (i in 1:length(symbs)) {
  p = cbind(p, Ad(get(symbs[i])))
}

r = diff(log(p))
r = na.omit(r)

snp = Ad(GSPC)
rS = diff(log(snp))
rS = na.omit(rS)

###Ranking plot

# Losses
losses <- -rS[rS<0]
losses <- as.numeric(losses)

t_ret <- sort(losses)
t_rank <- (length(losses):1)/length(losses)

dev.new()
plot(t_ret, t_rank)

dev.new()
pdf("LogLogRankingPlotSnPNegRet.pdf")
plot(t_ret, t_rank, log = "xy", xlab="Log of S&P 500 negative returns",
	ylab="Log of rank", pch=1, col="red", cex=1.2)
dev.off()
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

###Covariance matrix and mu-sigma plot

cov = cov(r, method="pearson")
nameOfFileP = "COVARIANCE4.xlsx"
write.table(cov, file = nameOfFileP, row.names = TRUE, col.names = TRUE)
#cov_spearman = cov(r, method="spearman")


mu = colMeans(r)
sigma = sqrt(diag(cov))

muS = mean(rS)
sigmaS = var(rS)

plot(as.numeric(sigma), as.numeric(mu), xlab="Variance", ylab="Expected returns", pch=20,
     col="red", xlim=c(0,0.07), ylim=c(-0.015, 0.010), cex=2)
lines(as.numeric(sigmaS), as.numeric(muS), type="p", col="darkblue", pch=16, cex=1.5)
###Minimum variance set

ones = rep(1, length(mu))
icov = solve(cov)

A = ones %*% icov %*% ones
B = ones %*% icov %*% mu
C = mu   %*% icov %*% mu
D = A*C-B^2

mu0 = seq(from=-0.015, to=0.01, by=0.0001)
MVP = (A*mu0^2 - 2*B*mu0 + C) / D

plot(sqrt(MVP), mu0, col='red', type="l", pch=20, xlab="Variance", ylab="Expected returns",
     xlim=c(0,0.07), ylim=c(-0.015, 0.010), lwd=2)
lines(as.numeric(sigma), as.numeric(mu), type="p", col="darkblue", pch=16, cex=1.5)

###Remove non-performing assets

r2 = r[,mu>0]
cov2 = cov(r2, method="pearson")

mu2 = colMeans(r2)
sigma2 = sqrt(diag(cov2))

ones2 = rep(1, length(mu2))
icov2 = solve(cov2)

A2 = ones2 %*% icov2 %*% ones2
B2 = ones2 %*% icov2 %*% mu2
C2 = mu2   %*% icov2 %*% mu2
D2 = A2*C2-B2^2

MVP2 = (A2*mu0^2 - 2*B2*mu0 + C2) / D2

lines(sqrt(MVP2), mu0, col="darkgreen", lwd=2)

###Solution for weights

mu_target = 0.001

gamma = (A*mu_target-B)/D
kappa = (C-B*mu_target)/D

a = gamma[1] * icov %*% mu + kappa[1] * icov %*% ones

gamma = (A2*mu_target-B2)/D2
kappa = (C2-B2*mu_target)/D2

a = gamma[1] * icov2 %*% mu2 + kappa[1] * icov2 %*% ones2

###Tangent portfolio

rf = 0.41 / 100. / 365.

Sharpe = function(r0) (r0-rf)/sqrt((A*r0^2-2*B*r0+C)/D)
cost = function(x) -Sharpe(x)

res = nlm(cost,0.002)

mu_T = res$estimate
sigma_T = sqrt((A* mu_T ^2 - 2*B* mu_T + C) / D)

Sharpe = function(r0) (r0-rf)/sqrt((A2*r0^2-2*B2*r0+C2)/D2)
cost = function(x) -Sharpe(x)
res2 = nlm(cost,0.002)

mu_T2 = res2$estimate
sigma_T2 = sqrt((A2* mu_T2 ^2 - 2*B2* mu_T2 + C2) / D2)

lines(c(0,sigma_T[1]),c(rf, mu_T), lwd=2, col="dodgerblue3",)
lines(c(0,sigma_T2[1]),c(rf, mu_T2), lwd=2, col="maroon4")

