# this code uses Rudy's code to generate Ultrametric trees
# then I sample from the generated batch and compute the curvature

generateUltra <- function(split) {
  D = c()
  delta <- seq(0.41, 2, by = 0.01)
  if (split == 1){
    for(i in delta){
    u1 <- c(0.2,i,2,i,2,2)
    u2 <- c(2,2,2,0.4,0.4,0.2)
    u3 <- c(0.4,2,2,2,2,0.4)
    D <- rbind(D,u1,u2,u3)
    }
  }
    
  if (split == 2){
    for(i in delta){
      u1 <- c(0.2,0.4,2,0.4,2,2)
      u2 <- c(2,2,2,0.4,0.4,0.2)
      u3 <- c(i,2,2,2,2,0.4)
      D <- rbind(D,u1,u2,u3)
    }
  }
  return (D)
}

D = generateUltra(1)
set.seed(110898)
curvatures = c()
for (i in 1:100){
  a = D[sample(1:nrow(D), size = 1),]
  b = D[sample(1:nrow(D), size = 1),]
  c = D[sample(1:nrow(D), size = 1),]
  while((identical(a,b)) | (identical(b,c)) | (identical(a,c))){
    a = D[sample(1:nrow(D), size = 1),]
    b = D[sample(1:nrow(D), size = 1),]
    c = D[sample(1:nrow(D), size = 1),]
  }
  P = matrix(c(a,b, c),nrow=length(a))
  curvatures = c(curvatures, curvatureN(P))
}
sum(curvatures==0)/length(curvatures) 
sum(curvatures==1)/length(curvatures) 
sum(curvatures==-1)/length(curvatures) 
sum(curvatures=="undefined")/length(curvatures) 
