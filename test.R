library(AER)
x=1:100
y=5*x+rnorm(100)
fm.tobit <- tobit(y~x)
summary(fm.tobit)
