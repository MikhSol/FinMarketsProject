
library(quantmod)
library(MASS)

############################################################
# Mean-Variance Portfolio Theory
############################################################

######### 1. fetch data from Yahoo

#tickers <- c("AC.PA","AI.PA","BN.PA","CA.PA","CS.PA","EAD.PA","EI.PA","FP.PA","GLE.PA","GTO.PA","LR.PA","ML.PA","OR.PA","PUB.PA","RNO.PA","SAN.PA","SOLB.PA","SU.PA","UL.PA","VIV.PA")

tickers <- c("AC.PA","AI.PA","BN.PA","CA.PA","CS.PA","EAD.PA")

symbs <- getSymbols(tickers, src="yahoo", from = "2011-01-01", to = Sys.Date())

p <- cbind()
for (i in 1:length(symbs)) {
	p <- cbind(p, Ad(get(symbs[i])))
}

r <- diff(log(p))
r <- na.omit(r)

######### 1a. remove holidays (with zero volume)

r
vol <- 0
for (i in 1:length(symbs)) {
	vol <- vol + Vo(get(symbs[i]))
}

r <- diff(log(p))
r <- r[vol>0]
r <- na.omit(r)

######### 2. calculate correlation matrix

c_pearson  <- cor(r, method="pearson")
c_spearman <- cor(r, method="spearman")

print(c_pearson)
print(c_spearman)

library(lattice)
dev.new()
rgb.palette <- colorRampPalette(c("red","white","blue"))
levelplot(c_spearman, col.regions=rgb.palette)

######### 2a. Correlogramm

r_numeric <- cbind()
for (i in 1:length(r[1,])) {
	r_numeric <- cbind(r_numeric, as.numeric(r[,i]))
}

library(corrgram)
dev.new()
corrgram(r_numeric, lower.panel=panel.shade, upper.panel=panel.pts)

######### 3. Covariance matrix and mu-sigma plot

cov <- cov(r, method="pearson")
cov_spearman <- cov(r, method="spearman")
#NB! this is not a covariance matrix! 

mu <- colMeans(r)
sigma <- sqrt(diag(cov))

plot(as.numeric(sigma), as.numeric(mu))

######### 4. Minimum variance set

ones <- rep(1, length(mu))
icov <- solve(cov)

A <- ones %*% icov %*% ones
B <- ones %*% icov %*% mu
C <- mu   %*% icov %*% mu
D <- A*C-B^2

mu0 <- seq(from=-0.003, to=0.003, by=0.0001)
MVP <- (A*mu0^2 - 2*B*mu0 + C) / D

plot(sqrt(MVP), mu0, col='red', type="l", xlim=c(0,0.03))
lines(as.numeric(sigma), as.numeric(mu), type="p")

######### 5. Remove non-performing assets

r2 <- r[,mu>0]
cov2 <- cov(r2, method="pearson")

mu2 <- colMeans(r2)
sigma2 <- sqrt(diag(cov2))

ones2 <- rep(1, length(mu2))
icov2 <- solve(cov2)

A2 <- ones2 %*% icov2 %*% ones2
B2 <- ones2 %*% icov2 %*% mu2
C2 <- mu2   %*% icov2 %*% mu2
D2 <- A2*C2-B2^2

MVP2 <- (A2*mu0^2 - 2*B2*mu0 + C2) / D2

lines(sqrt(MVP2), mu0, col='blue')

######### 6. Solution for weights

mu_target <- 0.001

gamma <- (A*mu_target-B)/D
kappa <- (C-B*mu_target)/D

a <- gamma[1] * icov %*% mu + kappa[1] * icov %*% ones

gamma <- (A2*mu_target-B2)/D2
kappa <- (C2-B2*mu_target)/D2

a <- gamma[1] * icov2 %*% mu2 + kappa[1] * icov2 %*% ones2

######### 6. Tangent portfolio

rf <- 1.78 / 100 / 365

Sharpe <- function(r0) (r0-rf)/sqrt((A*r0^2-2*B*r0+C)/D)
cost <- function(x) -Sharpe(x)

#x<-seq(from=0, to=0.01,by=0.0001)
#plot(x,Sharpe(x), type='l')

res <- nlm(cost,0.002)

mu_T <- res$estimate
sigma_T <- sqrt((A* mu_T ^2 - 2*B* mu_T + C) / D)

Sharpe <- function(r0) (r0-rf)/sqrt((A2*r0^2-2*B2*r0+C2)/D2)
cost <- function(x) -Sharpe(x)
res2 <- nlm(cost,0.002)

mu_T2 <- res2$estimate
sigma_T2 <- sqrt((A2* mu_T2 ^2 - 2*B2* mu_T2 + C2) / D2)

lines(c(0,sigma_T[1]),c(rf, mu_T))
lines(c(0,sigma_T2[1]),c(rf, mu_T2))


