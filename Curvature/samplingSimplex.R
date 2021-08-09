# sampling 3 bidimensional points from the simplex N times
# and compute the type of sampled triangles

library(hitandrun)

sampleSimplex = function (N){
  n = 6
  c = list(constr=t(c(1,1, 1,1, 1,1 )), rhs=c(1), dir=c("="))
  c_1 =  list(constr=t(c(-1,0,0,0,0,0 )), rhs=c(0), dir=c("<="))
  c_2 =  list(constr=t(c(0,-1,0,0,0,0 )), rhs=c(0), dir=c("<="))
  c_3 =  list(constr=t(c(0,0,-1,0,0,0 )), rhs=c(0), dir=c("<="))
  c_4 =  list(constr=t(c(0,0,0,-1,0,0 )), rhs=c(0), dir=c("<="))
  c_5 =  list(constr=t(c(0,0,0,0,-1,0 )), rhs=c(0), dir=c("<="))
  c_6 =  list(constr=t(c(0,0,0,0,0,-1 )), rhs=c(0), dir=c("<="))
  allc <- mergeConstraints(c, c_1, c_2, c_3, c_4, c_5, c_6)
  state <- har.init(allc)
  result <- har.run(state, n.samples=N)
}

types = c()
N = 10000
result = sampleSimplex(N)
for (i in 1:N){
  x = c(result$samples[i,1], result$samples[i,4])
  y = c(result$samples[i,2], result$samples[i,5])
  z = c(result$samples[i,3], result$samples[i,6])
  types = c(types , type(x,y,z))
}
mean(types == 1)
mean(types == 2)
mean(types == 3)
mean(types == 4)
mean(types == 5)