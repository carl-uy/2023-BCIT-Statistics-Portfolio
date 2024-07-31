#Inferences about difference between 2 means INDEPENDENT samples
# u: pop mean, n: sample size, xbar: sample mean, s: sample sd
#variables
x1=c(356,614,628,1088,943,367,536)
x2=c(344,230,264,464,278,318,280)
n1= 7
n2=7
xbar1= mean(x1)
xbar2= mean(x2)
s1= sd(x1)
s2=sd(x2)
alpha= 0.01
#end
df = n1-1
#z table (make sure to change based on upper / lower)
za= qnorm(1-(alpha/2))
e= za*(sqrt((s1^2/n1)+(s2^2/n2)))
#t table (make sure to change based on upper / lower)
ta= qt(1-(alpha/2),df)
e= ta*(sqrt((s1^2/n1)+(s2^2/n2)))
#interval
(xbar1-xbar2)-e
(xbar1-xbar2)+e
#Hypothesis test
#u1-u2 = 0 ; u1-u2 >< 0
alpha= 0.01
#step 2: critical point 
#lower tail test
tc= qt(alpha,df)
#upper tail test
tc= qt(1-alpha,df)
#step 3: test stat
t= (xbar1-xbar2)/sqrt((s1^2/n1)+(s2^2/n2))
#step 4
#step 5: conclusion
##################################################################

#Inferences about 2 means: DEPENDENT samples (matched pairs)
  #use the difference of the 2 samples and input to r under var d
  n=6
  d= c(0.6,1.9,1.5,2.2,0.5,1.2)
  alpha=0.01
  df=n-1
  dbar= mean(d)
  s= sd(d)
  #step 1: write the hypothesis (upper,lower,two-tailed)
  #step 2: upertail
    tc= qt(1-alpha,df)
  #step 3: test stat
    t=dbar/(s/sqrt(n))
###################################################################

#ANOVA
#VARIABLES
  #Number of population means being compared
    k=3
  #Sample sizes
    n1=4
    n2=4
    n3=4
  #The samples
    xa=c(7,3,6,6)
    xb=c(6,5,5,8)
    xc=c(4,7,6,7)
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