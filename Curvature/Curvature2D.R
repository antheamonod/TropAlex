# This code computes the curvature of a tropical triangle with bidimensional coordinates
# in the tropical projective torus R^3/R1

dtr2 <- function (x, y){
  x = c(0,x)
  y = c(0,y)
  return (max(x-y) - min(x-y))
}
deu <- function (x, y) return (sqrt(sum((x-y)^2)))

tropicalLine <-  function (x,y, tk) {
  if (identical(x,y)) {
    mu =  (x)
    return (mu)
  }
  if ((x[1] == y[1]) | (x[2] == y[2])) {
    mu = ((1-tk)*x+y*tk)
    return (mu)
  }
  
  if ((x[1] == x[2]) & (y[1] == y[2])){
    mu = ((1-tk)*x+y*tk)
    return (mu)
  }
  
  if (x[1]<y[1]) {
    a = x
    b = y
  }
  if (x[1]>y[1]) {
    a = y
    b = x
  }
  if ((a[2]<b[2]) & ((a[1]-a[2])<=(b[1]-b[2]))){
    t = (b[2]-a[2])/(b[1]-a[1])
    if (tk <= t) {
      mu = c(a[1]+tk*(b[1]-a[1]), a[2]+tk*(b[1]-a[1]))
    }
    if (t < tk) {
      mu = c(a[1]+tk*(b[1]-a[1]), b[2])
      
    }
  }
  
  if ((a[2]<b[2]) & ((a[1]-a[2])>(b[1]-b[2]))){
    t = (b[1]-a[1])/(b[2]-a[2])
    if (t >= tk) {
      mu = c(a[1]+tk*(b[2]-a[2]), a[2]+tk*(b[2]-a[2]))
    }
    if (t < tk) {
      mu = c(b[1],a[2]+tk*(b[2]-a[2]))
      
    }
  }
  
  if (a[2]>b[2]){
    t = (a[2]-b[2])/(a[2]-b[2]+b[1]-a[1])
    if (t >= tk) {
      mu = c(a[1],a[2]-tk*(a[2]-b[2]+b[1]-a[1]))
    }
    if (t < tk) {
      mu = c(a[1]+b[2]-a[2]+tk*(a[2]-b[2]+b[1]-a[1]),b[2])
    }
  }
  return (mu)
}

distances <- function (x,y,z,x1,y1,c1){
  t <- seq(0,1, length.out = 467)
  distan = matrix(NA, nrow = length(t),ncol = 2)
  if (x1[1]>=y1[1]) {
    a1 = y1
    b1 = x1
  }
  else {
    a1=x1
    b1=y1
  }
  EUsegment = c()
  for (i in 1:length(t)){
    tk = t[i]
    
    muTR = tropicalLine (x,y,tk)
    
    muE= (1-tk)*a1+b1*tk
    EUsegment = rbind(EUsegment, muE)
    distan[i,] =  c( dtr2(muTR, z), deu(muE, c1))
  }
  distan = round(distan , 8)
  if ((distan[1,1] ==distan[length(t),2]) & (distan[1,2] ==distan[length(t),1])){
    distan[,1] = rev(distan[,1])
  }
  return (distan)
}

#dist2 = distances(b[-1],c[-1],a[-1],P1[,2],P1[,3],P1[,1])

findEuclidean <- function (P){
  a = P[,1]
  b = P[,2]
  c = P[,3]
  dab = dtr2(a,b)
  dbc = dtr2(b,c)
  dac = dtr2(c,a)
  a1 = c(0,0)
  b1 = c(dab,0)
  xc = (dac^2-dbc^2+dab^2)/(2*dab)
  if (abs(round(xc, 8)) == abs(round(dac, 8))) yc = 0
  else yc = sqrt(dac^2-xc^2)
  c1 = c(xc, yc)
  P1 <- matrix(c(a1,b1, c1),nrow=length(a1))
  return (P1)
}


curvature <- function (P){
  P1 <- findEuclidean(P)
  curvS = 0
  curvF = 0
  sameDist = 0
  perm = matrix(c(1,2,3,2,3,1,1,3,2), nrow = 3, byrow = TRUE)
  for (j in 1:3){
    one = perm[j,1]
    two = perm[j,2]
    three = perm[j,3]
    a = P[,one]
    b = P[,two]
    c = P[,three]
    a1 = P1[,one]
    b1 = P1[,two]
    c1 = P1[,three]
    distan = round(distances (a,b,c,a1,b1,c1), 8)
    if (all(distan[,1] == distan[,2])) sameDist = sameDist +1
    else if (all(distan[,1] <= distan[,2])) curvS = curvS +1 
    else if (all(distan[,1] >= distan[,2])) curvF = curvF +1
    else (return ("undefined"))
  }
  if (sameDist == 3) return (0)
  else if ((sameDist+curvS) == 3) return (-1)
  else if ((sameDist+curvF) == 3) return (1)
  else return ("undefined")
}
plotTR2 <- function (P, cv){
  x = P[,1]
  y = P[,2]
  z = P[,3]
  xl = c(min(x[1], y[1], z[1]),max(x[1], y[1], z[1]) )
  yl = c(min(x[2], y[2], z[2]),max(x[2], y[2], z[2]) )
  trLine = c()
  t = seq(0,1, length.out = 200)
  for (ti in t){
    trLine = rbind(trLine, tropicalLine(x,y,ti))
  }
  par(mfrow=c(2,2), mar = rep(2, 4))
  plot(trLine[,1], trLine[,2], type = 'l', col = 'red', xlim = xl, ylim = yl, main = paste("Curvature: ",cv))
  trLine = c()
  for (ti in t){
    trLine = rbind(trLine, tropicalLine(y,z,ti))
  }
  lines(trLine[,1], trLine[,2], type = 'l', col = 'blue' )
  trLine = c()
  for (ti in t){
    trLine = rbind(trLine, tropicalLine(x,z,ti))
  }
  lines(trLine[,1], trLine[,2], type = 'l', col = 'green')
  points(x[1], x[2], pch = 16)
  points(y[1], y[2], pch = 16)
  points(z[1], z[2], pch = 16)
  P1 = findEuclidean(P)
  distan = distances (x,y,z,P1[,1],P1[,2],P1[, 3])
  yl = c(min(distan[,2], distan[,1]), max(distan[,2], distan[,1]))
  plot(distan[,2], type = 'l', col = 'purple', main = 'Distances from C to AB', ylim = yl)
  lines( distan[,1], type = 'l', col = 'orange')
  
  legend(1, 11, legend=c("Tropical", "Euclidean"),
         col=c("orange", "purple"), lty=c(1,1))
  distan = distances (x,z,y,P1[,1],P1[,3],P1[,2])
  yl = c(min(distan[,2], distan[,1]), max(distan[,2], distan[,1]))
  plot(distan[,2], type = 'l', col = 'purple', main = 'Distances from B to AC', ylim = yl)
  lines( distan[,1], type = 'l', col = 'orange')
  distan = distances (y,z,x,P1[,2],P1[,3],P1[,1])
  yl = c(min(distan[,2], distan[,1]), max(distan[,2], distan[,1]))
  plot(distan[,2], type = 'l', col = 'purple', main = 'Distances from A to BC', ylim = yl)
  lines( distan[,1], type = 'l', col = 'orange')
}

x = c(10,2)    
y = c(6,9)  
z = c(5,0)
cv = -1
