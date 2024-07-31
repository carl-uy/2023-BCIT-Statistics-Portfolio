x=mtcars$wt
a=sort(x)
boxplot(a,horizontal = TRUE)
length(a)
q2 = median(a)
q1 = median(a[1:25])
q3 = median(a[25:50])
i = q3-q1
wl = q1-1.5*i
wr = q3+1.5*i

