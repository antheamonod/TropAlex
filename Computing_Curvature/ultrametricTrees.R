# This code generates ultrametric trees and samples from the generated batch and computes curvature
# Author: Beatrice Matteo; code to generate the ultrametric trees was provided by Ruriko Yoshida

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

curvatures = c()
for (i in 1:1000){
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
