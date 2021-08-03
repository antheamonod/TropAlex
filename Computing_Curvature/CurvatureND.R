# This code computes the curvature of a tropical triangle with N-dimensional coordinates
# Author: Beatrice Matteo


dtr <- function (x, y) return (max(x-y) - min(x-y))
deu <- function (x, y) return (sqrt(sum((x-y)^2)))

trSegmentN <- function (u, v){
  N = length(u) 
  lambda = v-u
  sortLambda = sort(lambda)
  L = matrix(NA, nrow = N, ncol = N)
  y = v 
  L[1,] = y
  for (i in 2:(N-1)){
    li = sortLambda[i]
    y = pmin(u + li,v ) 
    y = y - y[1]
    L [i,] = y
  }
  L[N,] = u
  a = 0
  TrSegment = c()
  for (j in 1:(N-1)){
    byt = 0.001
    y1 = L[j,]
    y2 = L[j+1,]
    b = a+dtr(y1, y2)
    if (dtr(y1,y2) < byt) byt = dtr(y1,y2)/2
    t = seq(a,b,byt)
    t = t[-length(t)]
    segment = c()
    if (a == b) a = b
    else{
      segment = c()
      for (i in 1:length(t)){
        tk = t[i]
        segment =  rbind(segment, (1-(tk-a)/(b-a))*y1+(tk-a)*y2/(b-a))
      }
      TrSegment = rbind(TrSegment, segment)
    }
  }
  TrSegment = rbind(TrSegment, u)
  return (TrSegment)
}



distancesN <- function(a, b, c){
  P1 = findEuclideanN(a,b,c)
  c1 = P1[,3]
  trSeg = trSegmentN(b,a)
  n = nrow(trSeg) # they should be the same
  distanN = matrix(NA, nrow = n, ncol = 2)
  for (i in 1: n){
    ptTR = trSeg[i,]
    distanN[i,1] = dtr(ptTR, c)
    ptEU =  c(dtr(ptTR, a),0)
    distanN[i,2] = deu (ptEU, c1)
  }
  return (distanN)
}


findEuclideanN <- function (a,b,c){
  N = length(a)
  dab = dtr(a,b)
  dbc = dtr(b,c)
  dac = dtr(c,a)
  a1 = c(0,0)
  b1 = c( dab, 0)
  xc = (dac^2-dbc^2+dab^2)/(2*dab)
  if (abs(round(xc, 8)) == abs(round(dac, 8))) yc = 0
  else yc = sqrt(dac^2-xc^2)
  c1 = c( xc, yc)
  P1 <- matrix(c(a1,b1, c1),nrow=length(a1))
  return (P1)
}


curvatureN <- function (P){
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
    distan = round(distancesN (a,b,c), 5)
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
plotTRN <- function (P,cv){
  a = P[,1]
  b = P[,2]
  c = P[,3]
  xl = c(min(a[2], b[2], c[2]),max(a[2], b[2], c[2]) )
  yl = c(min(a[3], b[3], c[3]),max(a[3], b[3], c[3]) )
  trSegment = trSegmentN(a,b)
  par(mfrow=c(2,2))
  plot(trSegment[,2], trSegment[,3], type = 'l', col = 'red', xlim = xl, ylim = yl, main = paste(cv, "N-dim"))
  trSegment = trSegmentN(c,b)
  lines(trSegment[,2], trSegment[,3], type = 'l', col = 'blue' )
  trSegment = trSegmentN(a,c)
  lines(trSegment[,2], trSegment[,3], type = 'l', col = 'green')
  points(b[2], b[3], pch = 16)
  points(c[2], c[3], pch = 16)
  points(a[2], a[3], pch = 16)
  distan = distancesN (a,b,c)
  plot(distan[,2], type = 'l', col = 'blue', main = 'c from a-b')
  lines( distan[,1], type = 'l', col = 'red')
  distan = distancesN (a,c,b)
  plot(distan[,2], type = 'l', col = 'blue', main = 'b from a-c')
  lines( distan[,1], type = 'l', col = 'red')
  distan = distancesN (b,c,a)
  plot(distan[,2], type = 'l', col = 'blue', main = 'a from b-c')
  lines( distan[,1], type = 'l', col = 'red')
}
plotDistanN <- function (P){
  a = P[,1]
  b = P[,2]
  c = P[,3]
  par(mfrow=c(1,3))
  distan = distancesN (a,b,c)
  plot(distan[,2], type = 'l', col = 'blue', main = 'c from a-b')
  lines( distan[,1], type = 'l', col = 'red')
  distan = distancesN (a,c,b)
  plot(distan[,2], type = 'l', col = 'blue', main = 'b from a-c')
  lines( distan[,1], type = 'l', col = 'red')
  distan = distancesN (b,c,a)
  plot(distan[,2], type = 'l', col = 'blue', main = 'a from b-c')
  lines( distan[,1], type = 'l', col = 'red')
}
