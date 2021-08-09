# sampling type 1 and type 5 triangles with bidimensional coordinates
# and compute the curvature

library(hitandrun)

sampleType = function (type, N){
  n = 6
  if (type == 1){ # type 1
    
    c1 =  list(constr=t(c(-1,1, 0,1, -1,0 )), rhs=c(0), dir=c("<="))
    c2 =  list(constr=t(c(1,0, -1,-1, 0,1 )), rhs=c(0), dir=c("<="))
    c_a1 =  list(constr=t(c(-1,0, 0,0, 0,0 )), rhs=c(0), dir=c("<="))
    c_a2 =  list(constr=t(c(0,0, 0,-1, 0,0 )), rhs=c(0), dir=c("<="))
    c_1 =  list(constr=t(c(1,1, 1,1, 1,1 )), rhs=c(1), dir=c("="))
    
    c <- mergeConstraints(
      ordinalConstraint(n, 2, 1),
      ordinalConstraint(n, 3, 2),
      ordinalConstraint(n, 6, 4),
      ordinalConstraint(n, 5, 6), c1, c2, c_a1, c_a2, c_1)
    
    state <- har.init(c)
    result <- har.run(state, n.samples=N)
  }
  
  if (type == 5){ # type 5
    
    c_cb =  list(constr=t(c(0,-1, 1,0, 1,-1 )), rhs=c(0), dir=c("<="))
    c_ac =  list(constr=t(c(1,0, -1,-1, 0,1 )), rhs=c(0), dir=c("<="))
    c_a1 =  list(constr=t(c(-1,0, 0,0, 0,0 )), rhs=c(0), dir=c("<="))
    c_b2 =  list(constr=t(c(0,0, 0,0, -1,0 )), rhs=c(0), dir=c("<="))
    c_1 =  list(constr=t(c(1,1, 1,1, 1,1 )), rhs=c(1), dir=c("="))
    
    c <- mergeConstraints(
      ordinalConstraint(n, 2, 1),
      ordinalConstraint(n, 3, 2),
      ordinalConstraint(n, 6, 4),
      ordinalConstraint(n, 4, 5), c_cb, c_ac, c_a1, c_b2, c_1)
    
    state <- har.init(c)
    result <- har.run(state, n.samples=N)
    
  }
  return (result)
}


# compute curvature

curvaturesN = c()
N = 100
result = sampleType(5, N)
for (i in 1:N){
  a = c(result$samples[i,1], result$samples[i,4])
  b = c(result$samples[i,2], result$samples[i,5])
  c = c(result$samples[i,3], result$samples[i,6])
  P = matrix(c(a,b, c),nrow=length(a))
  cv = curvature(P)
  curvaturesN = c(curvaturesN, cv)
}
sum(curvaturesN==0)/length(curvaturesN) 
sum(curvaturesN==1)/length(curvaturesN) 
sum(curvaturesN==-1)/length(curvaturesN) 
sum(curvaturesN=="undefined")/length(curvaturesN) 



# plotting

for (i in 1:10){
  a = c(result$samples[i,1], result$samples[i,4])
  b = c(result$samples[i,2], result$samples[i,5])
  c = c(result$samples[i,3], result$samples[i,6])
  P = matrix(c(a,b, c),nrow=length(a))
  cv = curvature(P)
  plotTR2(P, cv)
}



