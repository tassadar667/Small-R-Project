f=function(x){
  (x[1]-0.2)^2+x[2]^2
}

nm=function(x_list){
  n=length(x_list)
  y=c()
  for (i in x_list) {
    y=c(y,f(i))
  }
  x_order=order(y)
  worst_point=x_list[[x_order[n]]]
  center_point=0*x_list[[1]]
  for (i in x_order[1:n-1]) {
    center_point=center_point+x_list[[i]]/(num-1)
  }
  reflect_point=center_point+(center_point-worst_point)
  if (f(reflect_point)>=y[x_order[1]]&f(reflect_point)<=y[x_order[n]]){
    return(list(worst_point,reflect_point))
  }
  if (f(reflect_point)<=y[x_order[1]]){
    expand_point=center_point+2*(center_point-worst_point)
    if (f(expand_point)<=f(reflect_point)) {
      return(list(worst_point,expand_point))
    }
    else{
      return(list(worst_point,reflect_point))
    }
  }
  if(f(reflect_point)>=y[x_order[n]]){
    contract_point=center_point-0.5*(center_point-worst_point)
    return(list(worst_point,contract_point))
  }
}

stop_point=function(x_list){
  n=length(x_list)
  mean_point=0*x_list[[1]]
  for(i in x_list){
    mean_point=mean_point+i/n
  }
  for(i in x_list){
    
  }
}
step=0
x_list=list(c(1,0),c(-2,0),c(0,3))
while(step<=142){
  if (length(x_list)==2){print(step)}
  temp=nm(x_list)
  step=step+1
  x_list_new=list()
  for (i in x_list) {
    if (i!=temp[[1]]){
      x_list_new=c(x_list_new,list(i))
    }
  }
  x_list_new=c(x_list_new,temp[2])
  x_list=x_list_new
}
