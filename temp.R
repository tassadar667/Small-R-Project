g=function(x){
  dt(x,5)
}

f=function(x){
  g(x)/dnorm(x)
}


n=1000
u=runif(n)
norm_sample=rnorm(n)
m=optimize(f, c(-10, 10), tol = 0.000001)$objective
temp=m*dnorm(norm_sample)/g(norm_sample)
target_sample=norm_sample[u<=temp]
print(head(temp))
