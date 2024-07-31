beaver <- read.csv("C:/Users/carlu/OneDrive/Desktop/Math 1060/beaver.csv")
View(beaver)
x=beaver$temp
r=range(x)
xmin=round(r[1],1)-0.1
xmax=round(r[2],1)+0.1
N = 10
step =(xmax-xmin)/N
boundaries = seq(xmin,xmax,by=step)
xcut=cut(x,breaks = boundaries,right = FALSE)
ft = table(xcut)
plot(ft, type = 'h')
total - sum(ft)
total = sum(ft)
prob = function(i){sum(ft[1:i])/total}
cp = sapply(1:N,prob)
plot(boundaries[2:(N+1)],cp,type="1",main = "Cummulative Probability:P(X<x)",xlab="x",ylab = "probability")
plot(boundaries[2:(N+1)], cp, type="l", main="Cumulative Prob-
+ ability: P(X < x)", xlab="x", ylab="Probability")
x = rnorm(1000)
