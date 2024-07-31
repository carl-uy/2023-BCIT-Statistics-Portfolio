#coin toss: probability of getting heads
n = 1:1000
p=rep(0,times=length(n))
for (m in n) {
  x = sample(c(1,2,3,4,5,6)m,replace=TRUE)
  p[m]=sum(x)/m
}
plot(n,p)
p[length(n)]


