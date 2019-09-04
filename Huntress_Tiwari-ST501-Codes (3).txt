
### Load packages
# install.packages("gdata")
# install.packages("fitdistrplus")
# install.packages("goftest")

require(fitdistrplus)
require(goftest)

### Load data
print("Loading Data:")

### Load xlsx
require(gdata)
google = read.xls("GOOGL.xlsx")
amazon = read.xls("AMZN.xlsx")

### load csv ### edit names slightly
##google = read.csv("GOOGL.csv")
##amazon = read.csv("AMZN.csv")
##google$AdjClose = google$Adj.Close
##amazon$AdjClose = amazon$Adj.Close


Rw = function (stock1,stock2,w) { ### Calculate log return of Adj Close portfolio
Pw = (stock1$AdjClose * w) + (stock2$AdjClose * (1-w))
logreturn=c(diff(log(Pw),lag=1))
return(logreturn)
}

ExploreData = function (stock) { ### Function for repeat use on different stocks

stock250 = stock[-1,]
stock250$logreturn = Rw(stock,stock,1)
### A.
q005 = quantile(stock250$logreturn,0.05) ### Get 0.05 quantile
q005.mean = mean(stock250[stock250$logreturn < q005,]$logreturn) ### Get mean of R values less than 0.05 quanitle
print("Estimated log returns:")
print(paste("0.05 quantile:", q005),sep="")
print(paste("Expecation R < Q(0.05):", q005.mean),sep="")


### B.
fitdistrplus::plotdist(as.numeric(stock250$logreturn)) ### Plot R data
# guessFit = fitdistrplus::descdist(as.numeric(stock250$logreturn)) ### Plot of possible distributions to fit with. Our data is close to logistic and lognormal
guessFit = fitdistrplus::descdist(as.numeric(stock250$logreturn),discrete = FALSE,boot = 1000,method = "unbiased",graph=TRUE,obs.col ="red",obs.pch = 12,boot.col = "orange")
### C.
# fitdistrplus::fitdist(as.numeric(stock250$logreturn), distr=dlnorm) ### Error: values must be positive to fit lognormal distribution
logisticFit = fitdistrplus::fitdist(as.numeric(stock250$logreturn), distr=dlogis) ### Returns location and scale parameters
print(logisticFit)



testFit = goftest::ad.test(as.numeric(stock250$logreturn),"plogis",logisticFit$estimate[1],logisticFit$estimate[2])
print(testFit)

### Monte Carlo
n = 100000 ### Samples
MC = rlogis(n, location = logisticFit$estimate[1], scale = logisticFit$estimate[2]) ### generate samples based on fit estimates
MC.q005 = quantile(MC,0.05) ### Get 0.05 quantile
MC.mean = mean(MC[MC < MC.q005])

print("Compare 0.05 quantiles:")
print(paste("Empirical Q(0.05):", q005),sep="")
print(paste("Empirical E[R|R<0.05]:", q005.mean),sep="")
print("===")
print(paste("Monte Carlo Q(0.05):", MC.q005),sep="")
print(paste("Monte Carlo Estimated E[R|R<0.05]:", MC.mean),sep="")
print("===")

print(paste("Absolute difference of Empirical VS Estimated E[R|R<0.05]:", abs(q005.mean - MC.mean)),sep="")
}

print("For Google Data:")
ExploreData(google)
print("===========================================================")
print("For Amazon Data:")
ExploreData(amazon)
print("===========================================================")

### Question 3

results = NULL
for (w in seq(0,1,1/50)) { ### Grid search over many weights
portfolio = Rw(google,amazon,w) ### Get log return for portfolio based on weight
portfolio.05 = quantile(portfolio,0.05) ### Calculate quantile
estimatedReturn = mean(subset(portfolio,subset = portfolio < portfolio.05)) ### Estimate Return

print(paste(w, portfolio.05, estimatedReturn,sep=","))
results = rbind(results,c(w, portfolio.05, estimatedReturn))
}
plot(x=results[,1],y=results[,2], main = "Portfolio 0.05 Sample Quantile", xlab = "Weight \n (1 = Google, 0 = Amazon)")
plot(x=results[,1],y=results[,3], main = "Portfolio ES(w)", xlab = "Weight \n (1 = Google, 0 = Amazon)")
print(paste("Weight with minimum ES:, ", results[results[,3]==min(results[,3]),1]))
print(paste("Weight with Maximum ES:, ", results[results[,3]==max(results[,3]),1]))
dev.off() ### Save PDF

