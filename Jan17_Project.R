x=c(1:99,151)
boxplot(x,horizontal = TRUE)
q2=median(x)
length(x)
q1=median(x[1:50])
q3=median(x[51:100])
i=q3-q1
wl=q1-1.5*i
wr=q3+1.5*i
