# sampling triangles with random coordinates, integer coordinates and also triangles
# from both the tree space and ultrametric tree space
# computing the curvature and comparing results of curvatureND and curvature2D 
# which should give the same result on R^3/R1

# random coordinates
set.seed(110898)
curvatures = c()
curvaturesN = c()
for (i in 1:100){
  x= runif(4, min = 0, max = 10)
  y= runif(4, min = 0, max = 10)
  z= runif(4, min = 0, max = 10)
  xN = c(0,x)
  yN = c(0,y)
  zN = c(0,z)
  P = matrix(c(x,y, z),nrow=length(x))
  #PN = matrix(c(xN,yN, zN),nrow=length(xN))
  cv = curvatureN(P)
  #cvN = curvatureN(PN)
  #curvatures = c(curvatures, cv)
  curvaturesN = c(curvaturesN, cv)
  #print(P)
  #plotTR2(P, cv)
}
sum(curvatures==0)/length(curvatures) 
sum(curvatures==1)/length(curvatures) 
sum(curvatures==-1)/length(curvatures) 
sum(curvatures=="undefined")/length(curvatures) 
sum(curvaturesN==0)/length(curvaturesN) 
sum(curvaturesN==1)/length(curvaturesN) 
sum(curvaturesN==-1)/length(curvaturesN) 
sum(curvaturesN=="undefined")/length(curvaturesN) 




# integer coordinates
set.seed(110898)
curvatures = c()
curvaturesN = c()
for (i in 1:100){
  x = sample(0:10, size = 4, replace = TRUE)
  y  = sample(c(0:10), size = 4, replace = TRUE)
  z  = sample(c(0:10), size = 3, replace = TRUE)
  while ((identical(x,y)) | (identical(x,z)) | (identical(z,y))){ 
    x = c(sample(0:10, size = 4, replace = TRUE))
    y = c(sample(c(0:10), size = 4, replace = TRUE))
    z  = c(sample(c(0:10), size = 4, replace = TRUE))
  }
  #xN = c(0,x)
  #yN = c(0,y)
  #zN = c(0,z)
  PN = matrix(c(x,y, z),nrow=length(x))
  #PN = matrix(c(xN,yN, zN),nrow=length(xN))
  #cv = curvature(P)
  cvN = curvatureN(PN)
  #curvatures = c(curvatures, cv)
  curvaturesN = c(curvaturesN, cvN)
  #print(P)
  #plotTR2(P, cv)
  #plotTRN(PN, cvN)
}

#sum(curvatures==0)/length(curvatures) 
#sum(curvatures==1)/length(curvatures) 
#sum(curvatures==-1)/length(curvatures) 
#sum(curvatures=="undefined")/length(curvatures) 
mean(curvaturesN==0) 
mean(curvaturesN==1)
mean(curvaturesN==-1) 
mean(curvaturesN=="undefined")

# integer coordinates
set.seed(110898)
curvatures = c()
curvaturesN = c()
for (i in 1:100){
  x = sample(0:10, size = 2, replace = TRUE)
  y  = sample(c(0:10), size = 2, replace = TRUE)
  z  = sample(c(0:10), size = 2, replace = TRUE)
  while ((identical(x,y)) | (identical(x,z)) | (identical(z,y))){ 
    x = c(sample(0:10, size = 2, replace = TRUE))
    y = c(sample(c(0:10), size = 2, replace = TRUE))
    z  = c(sample(c(0:10), size = 2, replace = TRUE))
  }
  #xN = c(0,x)
  #yN = c(0,y)
  #zN = c(0,z)
  PN = matrix(c(x,y, z),nrow=length(x))
  #PN = matrix(c(xN,yN, zN),nrow=length(xN))
  #cv = curvature(P)
  cvN = curvature(PN)
  #curvatures = c(curvatures, cv)
  curvaturesN = c(curvaturesN, cvN)
  #print(P)
  #plotTR2(P, cv)
  #plotTRN(PN, cvN)
}

mean(curvaturesN==0) 
mean(curvaturesN==1)
mean(curvaturesN==-1) 
mean(curvaturesN=="undefined")







library(ape)
library(adephylo)
Tr = rtree(5)
Tr = as.phylo(as.hclust(chronos(Tr, lambda=0, quiet=TRUE) ))

projTroursPt <- function (leaves, ultra = F){
  Tr = rtree(leaves)
  if (ultra) Tr = as.phylo(as.hclust(chronos(Tr, lambda=0, quiet=TRUE) ))
  Trd = distTips(Tr, tips = "all", method = "patristic", useC = TRUE)
  return( Trd)
}
set.seed(110898)
curvatures = c()
for (i in 1:1000){
  a = round(as.numeric(projTroursPt(6, T)), 8)
  b = round(as.numeric(projTroursPt(6, T)), 8)
  c = round(as.numeric(projTroursPt(6, T)), 8)
  while((identical(a,b)) | (identical(b,c)) | (identical(a,c))){
    a = round(as.numeric(projTroursPt(6, T)), 8)
    b = round(as.numeric(projTroursPt(6, T)), 8)
    c = round(as.numeric(projTroursPt(6, T)), 8)
  }

  P = matrix(c(a,b, c),nrow=length(a))
  #print(P)
  #plotDistanN(P)
  curvatures = c(curvatures, curvatureN(P))
}
sum(curvatures==0)/length(curvatures) 
sum(curvatures==1)/length(curvatures) 
sum(curvatures==-1)/length(curvatures) 
sum(curvatures=="undefined")/length(curvatures) 


a = c(0.0000000, 0.6666667, 1.3333333,1.3333333 ,0.6666667, 1.3333333, 1.3333333, 1.3333333, 1.3333333, 0.3333333)
b = c( 0.0,  0.0 , 1.0 , 1.0, -0.5 , 1.0,  1.0 , 1.0  ,1.0  ,0.0)
c= c(0.0000000 , 0.0000000,  0.0000000,  0.0000000, -1.3333333 ,-0.6666667 ,-0.6666667 ,-0.6666667 ,-0.6666667 ,-1.3333333)



