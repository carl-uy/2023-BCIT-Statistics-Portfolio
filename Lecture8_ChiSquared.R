#LECTURE 8 KAI SQ DIST
n = 12
df = n-1
xbar = 38.3
s = 4.4
alpha = 0.02

XR= qchisq(alpha/2,df,lower.tail = FALSE)
XL= qchisq(1-alpha/2,df,lower.tail = FALSE)

L= s*(sqrt((n-1)/XR))
R= s*(sqrt((n-1)/XL))

# Goodness of fit

o = c(7,14,6,10,8,4,5,6,12,8)
k = length(o)
E = (sum(o)/k)
n = sum(o)
df = k - 1
XSQ = sum(((o)-(E))^2/E)
Xc = qchisq(alpha,df,lower.tail = FALSE)