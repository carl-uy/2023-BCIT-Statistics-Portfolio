mu <- 2
n <- 100
p <- mu/n
ns <- 1000 
x <- rep(0,lens=ns) 
for (i in 1:ns) {
  x[i] <- sum(ifelse(runif(n)<p,1,0))
}
rf <- table(x)/ns
plot(rf,ylim=c(0,1.3*max(rf)))
xlabels <- 0:ns
Ptheory <- choose(n,xlabels)*p^(xlabels)*(1-p)^(n-xlabels)
points(xlabels,Ptheory)

c(mean(x),n*p)
c(sd(x),sqrt(n*p*(1-p)))

