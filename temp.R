library(foreign)
library(quantreg)
df=read.dta("./data/grilic.dta")
x=cbind("s"=df$s,"iq"=df$iq,"expr"=df$expr,"tenure"=df$tenure,
        "rns"=df$rns,"smas"=df$smsa)
y=cbind("lw"=df$lw)
osl=lm(y~x)
summary(osl)

lad=rq(y~x,tau=0.5)
summary(lad)


ans=c()
for (i in seq(0.1,0.9,0.1)) {
  temp=rq(y~x,tau=i)
  ans=rbind(ans,temp$coefficients)
}
ans=cbind(ans,"x"=seq(0.1,0.9,0.1))
library(ggplot2)
library(data.table)
ans=data.table(ans)
ans=melt(ans,id="x")
p=ggplot(ans,aes(x=x,y=value,group=variable,color=variable))+
  geom_line()
p

ans=c()
for (i in seq(0.1,0.9,0.1)) {
  temp=rq(y~x,tau=i)
  ans=rbind(ans,temp$coefficients[-1])
}
ans=cbind(ans,"x"=seq(0.1,0.9,0.1))
ans=data.table(ans)
ans=melt(ans,id="x")
p=ggplot(ans,aes(x=x,y=value,group=variable,color=variable))+
  geom_line()
p