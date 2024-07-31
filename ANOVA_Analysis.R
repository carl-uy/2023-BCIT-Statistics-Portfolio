#ANOVA
#VARIABLES
#Number of population means being compared
k=3
#Sample sizes
n1=3
n2=3
n3=3
#The samples
xa=c(643,655,702)
xb=c(469,427,525)
xc=c(484,456,402)
alpha=0.05
#Totals of all the sample sizes 
N=n1+n2+n3
#Variances 
v1= var(xa)
v2= var(xb)
v3= var(xc)
#mean
x1=mean(xa)
x2=mean(xb)
x3=mean(xc)
df1 = k-1
df2= N-k
#Step1: Hypothesis 
#Step2: critical point
fc= qf(1-alpha,df1,df2)
#mean of all samples combined
xbb=(x1+x2+x3)/3
#variability between samples
B=(n1*(x1-xbb)^2+n2*(x2-xbb)^2+n3*(x3-xbb)^2)/(k-1)
#average variability within samples 
W=((n1-1)*v1+(n2-1)*v2+(n3-1)*v3)/((n1-1)+(n2-1)+(n3-1))
#Test Statistic: [Var bet Samples (B) / Average Var Bet Samples (W)]
f = B/W
