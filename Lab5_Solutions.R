#Normal Distribution
x <- seq(0,1, by=0.25)
density01 <- dnorm(x,mean=0, sd=1)
plot(x, density01, type= "1")
densityA<-dnorm(x,mean=0, sd=2)
points(x,densityA,type = "1",col="red")

#Sampling Distribution
population <- runif(1000000,0,10) #underlying population
mu <- mean(population) #population standard deviation
sigma <- sd(population) #POPULATION STANDARD DEVIATION

ns <- 3 #sample size of each sample
N <- 1000 # number of samples
xbars <- rep(0,N) # initialize vector x
for(i in 1:N){
  xbars[i] <- mean(sample(population,ns,replace=TRUE))  
}
hist(xbars,xlim=c(0,10),breaks=50,probability=TRUE)
