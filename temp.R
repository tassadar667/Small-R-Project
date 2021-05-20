library(quantreg)
df=read.table("./data/infant-birthweight.txt",header = TRUE)
y=df$weight
x=df[3:21]
x=data.table(x)
lad=rq(y~x)
summary(lad)