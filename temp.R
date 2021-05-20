library(quantreg)
df=read.table("./data/infant-birthweight.txt",header = TRUE)
y=df$weight
x=df[c(4,5,6,16)]
x=as.matrix(x)
lad=rq(y~x)
summary(lad)