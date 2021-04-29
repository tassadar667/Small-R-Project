x=read.table('employee.txt',header = TRUE)
kruskal.test(x$salary,x$educ)
edu=unique(x$educ)
y=list()
for (i in edu) {
  y=c(y,list(c(x$salary[x$educ==i])))
}
dunn(y)