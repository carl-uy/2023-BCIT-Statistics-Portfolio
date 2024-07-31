install.packages()
install.pacakages("data.table")
library("data.table")
#3 import the dataset bitcoin.csv from Learning Hub

confidenceIntervals <- function(rawData, range){
  cleanData <- ifelse(rawData<3000,rawData,0)
  x<- cleanData[range]
  plot(range,x,type='o')
  mu = mean(x)
  s = sd(x)
  n = length(x)
  cat("mean =", round(mu,digits = 2), "sd =",round(s,digits=2),'\n')
  xg <- seq(mu-3*s/sqrt(n),mu+3*s/sqrt(n), by=0.06*s/sqrt(n)) # x for graphing
  zg <- (xg-mu)/s*sqrt(n) # z for graphing
  
  # 95% confidencne interval using Normal distribution
  En <- qnorm(0.975)*s/sqrt(n)
  cat("Normal distribution: E=", En, ", ",mu - En, "<mu<",mu + En, '\n')
  
  # 95% confidence interval using t distribution
  Et <- qt(0.975, df=n-1)*s/sqrt(n)
  cat("t-distribution: E=", Et, ", ",mu-Et, "<mu<", mu+Et, '\n')
  
  # graphs of normal and t-distribution
  plot(xg, dnorm(zg), type="1", xlab="x", ylab="Prob. density")
  points(xg, dt(zg, df=n-1),type="1",col="green")
}

confidenceIntervals(bitcoin$Close, 1295:1300)

