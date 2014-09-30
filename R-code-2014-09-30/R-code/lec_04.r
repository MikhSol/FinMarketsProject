
library(quantmod)
library(MASS)

############################################################
# Capital Asset Pricing Theory
############################################################

######### 1. fetch data from Yahoo

#tickers <- c("^GSPC","IDPIX")
tickers <- c("^GSPC","FDSAX")

symbs <- getSymbols(tickers, src="yahoo", from = "2011-01-01", to = Sys.Date())

p <- cbind()
for (i in 1:length(symbs)) {
	p <- cbind(p, Ad(get(symbs[i])))
}

r <- diff(log(p))
r <- na.omit(r)

######### 2. calculate correlation matrix

c_pearson  <- cor(r, method="pearson")
c_spearman <- cor(r, method="spearman")

print(c_pearson)
print(c_spearman)

######### 3. Calc alpha and beta

rf <- 1.78 / 100 / 252

plot(as.numeric(r$GSPC),as.numeric(r$FDSAX))

rmarket <- as.numeric(r$GSPC) - rf
rfund   <- as.numeric(r$FDSAX) - rf

CAPM <- lm(rfund ~ rmarket)
print(CAPM)

alpha <- coefficients(CAPM)[1]
beta <- coefficients(CAPM)[2]

x <- seq(from=-0.07, to=0.07, by=0.001)
lines(x, rf+alpha+beta*(x-rf), col='red')

######### 4. load SMB and HML

source("ken_french.r")

ff_daily_factors[1:10,]

######### 5. Extract data for our dates

rSMB <- rep(NA,length(r[,1]))
rHML <- rep(NA,length(r[,1]))
rmarket <- rep(NA,length(r[,1]))
rf <- rep(NA,length(r[,1]))
rfund <- rep(NA,length(r[,1]))
for (i in 1:length(r[,1])){
	element <- which(ff_daily_factors$date==index(r[i]))
	if (length(element)==1){
		rSMB[i] = ff_daily_factors[element,]$smb
		rHML[i] = ff_daily_factors[element,]$hml
		rmarket[i] = ff_daily_factors[element,]$mktrf
		rf[i] = ff_daily_factors[element,]$rf
		rfund[i] = as.numeric(r[i,]$IDPIX)
	} 
}

inds <- is.na(rSMB) | is.na(rHML) | is.na(rmarket) | is.na(rf)

rSMB <- rSMB[!inds]
rHML <- rHML[!inds]
rmarket <- rmarket[!inds]
rf <- rf[!inds]

rfund <- rfund[!inds] - rf

######### 6. Fit three-factor-model

FF_CAPM <- lm(rfund ~ rmarket + rSMB + rHML)
print(FF_CAPM)

ff_alpha <- coefficients(FF_CAPM)[1]
ff_beta <- coefficients(FF_CAPM)[2]
ff_beta_s <- coefficients(FF_CAPM)[3]
ff_beta_h <- coefficients(FF_CAPM)[4]


