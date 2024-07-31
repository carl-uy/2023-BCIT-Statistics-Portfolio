# question number 1 Compute the linear correlation coefficient.
C=c(314,317,320,326,331,339,346,354,361,369)
T=c(13.9,14.3,13.9,14.1,14.0,14.3,14.1,14.5,14.5,14.4)
N=length(C)
DF=N-2
r=cor(C,T)

# 2 Is there a positive correlation between CO2 concentration and temperature? 
# Conduct a hypothesis test at a 0.01-significance level.
talpha= qt(0.99,DF)
t=(r-0)/sqrt((1-r^2)/DF)

# 3 find the regression line (T hat)
M = cov(C,T)/var(C)
T0= mean(T)-M*mean(C)
# 4
ta= qt(0.99,DF)
Se= sqrt(sum((T-(T0+M*C))^2)/DF)
Sb= (1/sqrt(N-1))*(Se/sd(C))
E = ta*Sb