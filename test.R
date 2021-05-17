x=seq(-10,10,0.1)
norm_y=dnorm(x)
logi_y=exp(-x)/(1+exp(-x))^2
cauc_y=1/3.1415926/(1+x^2)
stud_y=dt(x,5)
norm1_y=dnorm(x,1,2)

g=function(x){
  dt(x,5)
}
g_sample=function(n){
  rt(n,5)
}

f=function(x){
  dnorm(x)
}

h=function(x){
  g(x)/f(x)
}


reject_sample=function(n=1000){
  u=runif(n)
  g_sam=g_sample(n)
  m=1/optimize(h, c(-10, 10), tol = 0.000001)$objective
  temp=f(g_sam)/(m*g(g_sam))
  target_sample=g_sam[u<=temp]
  u1=u[u<=temp]
  #print(head(temp))
  list(accept_rate=1/m,target_sample=target_sample,
       real_accept_rate=length(target_sample)/n,
       g_sample=g_sam,u=u,m=m,u1=u1)
}

temp=reject_sample(1000)
f_sample=density(temp$target_sample)
library(ggplot2)
p=ggplot()+geom_line(aes(x,temp$m*g(x)),colour="red")+
  geom_line(aes(x=x,norm_y))+
  geom_point(aes(temp$g_sample,temp$m*temp$u*g(temp$g_sample)),alpha=0.3,colour="grey")+
  geom_point(aes(temp$target_sample,temp$m*temp$u1*g(temp$target_sample)),alpha=0.7,colour="green")
  
print(p)
cat(temp$accept_rate,temp$real_accept_rate)