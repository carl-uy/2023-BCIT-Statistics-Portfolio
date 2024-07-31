# things to do before step 1
x= c(53,67.5,72)
y= c(80,344,348)
N= length(x)
DF = N-2
r = cor(x,y)
alpha= 0.05
# 1st step: identify which tail
# 2nd step : critical points
talpha= qt(1-(alpha/2),DF)
# 3rd step: test stat
t=(r-0)/sqrt((1-r^2)/DF)

#Regression Line

b1 = cov(x,y)/var(x)
b0= mean(y)-b1*mean(x)

# interval
ta= qt(1-(0.10/2),DF)
Se= sqrt(sum((y-(b0+b1*x))^2)/DF)
Sb= (1/sqrt(N-1))*(Se/sd(x))
E = ta*Sb

b1 + E
b1 - E