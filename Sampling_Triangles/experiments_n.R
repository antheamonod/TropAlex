# This code samples tropical triangles by type and returns the proportion of their curvature type as a percentage
# Author: Carlos Améndola

library(hitandrun)
n <- 2
set.seed(25)
constr <- simplexConstraints(3*n)
state <- har.init(constr)
N <- 10000
result <- har.run(state, n.samples=N)
curvaturesN = c()
for (i in 1:N){
  a = c(0,result$sample[i,1:n])
  b = c(0,result$sample[i,(n+1):(2*n)])
  c = c(0,result$sample[i,(2*n+1):(3*n)])
  P = matrix(c(a,b,c),nrow=length(a))
  cv = curvatureN(P)
  curvaturesN = c(curvaturesN, cv)
}
100*sum(curvaturesN==0)/length(curvaturesN) 
100*sum(curvaturesN==1)/length(curvaturesN) 
100*sum(curvaturesN==-1)/length(curvaturesN) 
100*sum(curvaturesN=="undefined")/length(curvaturesN) 

