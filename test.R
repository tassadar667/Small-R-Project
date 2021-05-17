reject_sample=function(n=1000){
  u=runif(n)
  norm_sample=rnorm(n)
  c=optimize(f(x),c(-10,10))
  
}
f=function(x){
  dnorm(x)/g(x)
}
g=function(x){
  exp(-x)/(1+exp(-x))^2
}
