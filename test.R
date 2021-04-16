x=c(4.12,5.81,7.63,9.74,10.39,11.92,12.32,12.89,13.54,14.45)
r=rank(abs(x-8))
print(r)
s=c()
sgn=function(x){
  if(x>0){
    return(1)
  }
  else{
    return(-1)
  }
}
for (i in 1:length(x)) {
  s=c(s,pnorm((1+r[i])/(length(x))+1))*sgn(x[i]-8)
}
print(s)
t=sum(s)/sum(s^2)
p=(1-pnorm(t))*2
cat("t=",t,'\n',sep = '')
cat('p-value=',p,'\n',sep = '')