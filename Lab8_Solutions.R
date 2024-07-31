#Confidence interval for population mean
xb0 = 21.3 # Sample mean
s0 = 0.7 # Sample s.d. Assume s0 is approximately equal to sigma
n = 4 # sample size (number per sample)
alpha = 0.05 # significance level
ns = 1000 # number of samples to be generated
population = rnorm(3000,mean=xb0,sd=s0) # generate normally distributed random numbers
hist(population,breaks=30)

s =rep(0,ns) # initialize s's
for(i in 1:ns) s[i] = mean( sample(population, n) ) # Repeat ns times
hist(s,breaks=30) # Display sampling distribution

left = round(ns*alpha/2); right = round(ns*(1-alpha/2))
s.sorted = sort(s,method="quick")
cat("Simulation: ", s.sorted[left], " < mu < ", s.sorted[right])

#Theoretical result for comparison
E = qnorm(1-alpha/2)*s0/sqrt(n) # Margin of error
cat("Theoretical: ", xb0-E, " < mu < ", xb0+E)