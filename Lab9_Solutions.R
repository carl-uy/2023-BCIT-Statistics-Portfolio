# F distribution
pop = rnorm(20000, mean=0, sd=1) # Population
n1 = 40; n2 = 30 # sample sizes
k=2 # number of categories
ntot = n1+n2 # total number of values
alpha=0.10 # significance level
f = c() # container for F values
for(i in 1:1000){ # repeat the block to obtain 1000 F values
sample1 = sample(pop, n1)
xb1 = mean(sample1); v1 = var(sample1)
sample2 = sample(pop, n2)
xb2 = mean(sample2); v2 = var(sample2)
xb = c(xb1,xb2)
xbb = mean(c(sample1,sample2))
denominator = ((n1-1)*v1+(n2-1)*v2)/(ntot - k)
numerator = sum( c(n1,n2)*(xb - xbb)^2 )/(k-1)
f = append(f, numerator/denominator)
}
# Plot density function
hist(f, breaks=100, freq=FALSE) # Simulation
xf =seq(0,10,len=100)
lines(xf,df(xf, df1=k-1, df2=ntot-k)) # Theoretical
# Compute critical F value
f.sorted = sort(f)
cat("Critical F value for alpha = ", alpha)
cat("Simulation: ", f.sorted[round((1-alpha)*length(f))] )
cat("Theoretical: ", qf(1-alpha,df1=k-1, df2=ntot-k) )