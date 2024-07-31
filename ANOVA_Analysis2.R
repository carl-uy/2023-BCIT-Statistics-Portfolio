#ANOVA
#VARIABLES
#Number of population means being compared
k=3
#Sample sizes
n1=4
n2=4
n3=3
s1= 38.58
s2= 84.71
s3 = 41.68
alpha=0.05
#Totals of all the sample sizes 
N=n1+n2+n3
#Variances 
v1= s1^2
v2= s2^2
v3= s3^2
#mean
x1= 607
x2= 506.5
x3= 447.33
df1 = k-1
df2= N-k
#Step1: Hypothesis 
#Step2: critical point
fc= qf(1-alpha,df1,df2)
#mean of all samples combined
xbb=(x1+x2+x3)/3
#variability between samples
B=((n1*(x1-xbb)^2)+(n2*(x2-xbb)^2)+(n3*(x3-xbb)^2))/(k-1)
#average variability within samples 
W=((n1-1)*v1+(n2-1)*v2+(n3-1)*v3)/((n1-1)+(n2-1)+(n3-1))
#Test Statistic: [Var bet Samples (B) / Average Var Bet Samples (W)]
f = B/W