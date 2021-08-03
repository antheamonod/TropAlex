# This code returns the combinatorial type of a given tropical triangle with bidimensional coordinates
# Author: Beatrice Matteo


is_1 = function (a,b,c){
  return ((a[1]<=b[1]) & (b[1]<=c[1]) & (a[2] <= c[2]) & (c[2]<= b[2]) & (b[1]-b[2]  <=a[1]-a[2]) & (a[1]-a[2]<= c[1]-c[2]))
}

is_5 = function (a,b,c){
  return ((a[1]<=b[1]) & (b[1]<=c[1]) & (b[2] <= a[2]) & (a[2]<= c[2]) & (c[1]-c[2] <= b[1]-b[2]) & (a[1]-a[2]<= c[1]-c[2]))
}

is_3 = function (a,b,c){
  one = ((a[1]<=b[1]) & (b[1]<=c[1]) & (a[2] <= b[2]) & (b[2]<= c[2]) & (c[1]-c[2] <= b[1]-b[2]) & (b[1]-b[2] <= a[1]-a[2]))
  two = ((a[1]<=b[1]) & (b[1]<=c[1]) & (a[2] >= b[2]) & (b[2]>= c[2]))
  three = ((a[1]<=b[1]) & (b[1]<=c[1]) & (a[2] <= b[2]) & (b[2]<= c[2]) & (c[1]-c[2] >= b[1]-b[2]) & (b[1]-b[2] >= a[1]-a[2]))
  return (one | two | three)
}

is_2 = function (a,b,c){
  one =  ((a[1]<=b[1]) & (b[1]<=c[1]) & (a[2] <= c[2]) & (c[2]<= b[2]) & (a[1]-a[2] <= b[1]-b[2]))
  six = ((a[1]<=b[1]) & (b[1]<=c[1]) & (a[2] <= c[2]) & (c[2]<= b[2]) & (b[2]-b[1] >= a[2]-a[1]) & (c[2]-c[1] >= a[2]-a[1]))
  two = ((a[1]<=b[1]) & (b[1]<=c[1]) & (b[2] >= a[2]) & (a[2]>= c[2])& (b[2]-a[2] >= b[1]-a[1]))
  three = ((a[1]<=b[1]) & (b[1]<=c[1]) & (a[2] >= c[2]) & (c[2]>= b[2])& (c[2]-b[2] <= c[1]-b[1]))
  four = FALSE
  #four = ((a[1]<=b[1]) & (b[1]<=c[1]) & (b[2] <= a[2]) & (a[2] <= c[2])  & (c[2]-b[2] <= c[1]-b[1]))
  five = ((a[1]<=b[1]) & (b[1]<=c[1]) & (a[2] <= b[2]) & (b[2] <= c[2]) & (c[2]-b[2] >= c[1]-b[1]) & (b[2]-a[2] >= b[1]-a[1]))
  seven =  ((a[1]<=b[1]) & (b[1]<=c[1]) & (a[2] <= b[2]) & (b[2] <= c[2]) & (c[2]-b[2] >= c[1]-b[1]) & (b[1]-a[1] >= b[2]-a[2]))
  
  return (one | two | three | four | five | six | seven)
}

is_4 = function (a,b,c){
  one = ((a[1]<=b[1]) & (b[1]<=c[1]) & (b[2] <= a[2]) & (a[2]<= c[2]) & (a[1]-a[2] <= c[1]-c[2]) & (b[1]-b[2] <= c[1]-c[2]))
  six = ((a[1]<=b[1]) & (b[1]<=c[1]) & (b[2] <= a[2]) & (a[2]<= c[2]) & (a[1]-a[2] >= c[1]-c[2]))
  two = ((a[1]<=b[1]) & (b[1]<=c[1]) & (b[2] >= a[2]) & (a[2]>= c[2]) & (b[2]-a[2] <= b[1]-a[1]))
  three = ((a[1]<=b[1]) & (b[1]<=c[1]) & (a[2] >= c[2]) & (c[2]>= b[2])& (c[2]-b[2] >= c[1]-b[1]))
  #four = ((a[1]<=b[1]) & (b[1]<=c[1]) & (b[2] <= a[2]) & (a[2] <= c[2])  & (c[2]-b[2] >= c[1]-b[1]) & ())
  four = FALSE
  five = ((a[1]<=b[1]) & (b[1]<=c[1]) & (a[2] <= b[2]) & (b[2] <= c[2]) & (c[2]-b[2] <= c[1]-b[1]) & (b[2]-a[2] >= b[1]-a[1]))
  seven = ((a[1]<=b[1]) & (b[1]<=c[1]) & (a[2] <= b[2]) & (b[2] <= c[2]) & (c[2]-b[2] >= c[1]-b[1]) & (c[1]-a[1] >= c[2]-a[2]))
  return (one | two | three | four | five | six | seven)
}

which_type = function (a,b,c){
  if (is_1(a,b,c)) return (1)
  if (is_2(a,b,c)) return (2)
  if (is_3(a,b,c)) return (3)
  if (is_4(a,b,c)) return (4)
  if (is_5(a,b,c)) return (5)
  else (return (FALSE))
}

type = function(a,b,c){
  i = 0
  perm = matrix(c(a,b,c, a,c,b, b,a,c, b,c,a, c,a,b, c,b,a), nrow = 6, byrow = TRUE)
  round(perm, 6)
  typ = which_type(a,b,c)
  types = c()
  for (i in 1:6){
    x = perm[i,1:2]
    y = perm[i,3:4]
    z = perm[i,5:6]
    typ = which_type(x,y,z)
    if (typ != FALSE ) types = c(types, typ)
  }
  if (length(types) > 1) {
    print("Multiple types")
    return (types)
  }
  if (length(types) == 0) {
    print("Unknown type")
    return (types)
  }
  return(types)
}

#type (a,b,c)


plotTYPE <- function (x,y,z){
  typ = type(x,y,z)
  xl = c(min(x[1], y[1], z[1]),max(x[1], y[1], z[1]) )
  yl = c(min(x[2], y[2], z[2]),max(x[2], y[2], z[2]) )
  trLine = c()
  t = seq(0,1, length.out = 200)
  for (ti in t){
    trLine = rbind(trLine, tropicalLine(x,y,ti))
  }
  plot(trLine[,1], trLine[,2], type = 'l', col = 'red', xlim = xl, ylim = yl, main = paste("Type", typ))
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
}

l = list()
for (j in 1:10){
  x= runif(2, min = 0, max = 10)
  y= runif(2, min = 0, max = 10)
  z= runif(2, min = 0, max = 10)
  P = matrix(c(x,y,z), nrow = length(x))
  l[[j]] = P
  plotTYPE(x,y,z)
}

types = c()
for (j in 1:1000){
  x= runif(2, min = 0, max = 10)
  y= runif(2, min = 0, max = 10)
  z= runif(2, min = 0, max = 10)
  types = c(types , type(x,y,z))
}
mean(types == 1)
mean(types == 2)
mean(types == 3)
mean(types == 4)
mean(types == 5)

# checking multiple types
for (j in 1:10){
  x = sample(0:10, size = 2, replace = TRUE)
  y  = sample(c(0:10), size = 2, replace = TRUE)
  z  = sample(c(0:10), size = 2, replace = TRUE)
  while ((identical(x,y)) | (identical(x,z)) | (identical(z,y))){ 
    x = c(sample(0:10, size = 2, replace = TRUE))
    y = c(sample(c(0:10), size = 2, replace = TRUE))
    z  = c(sample(c(0:10), size = 2, replace = TRUE))
  }
  plotTYPE(x,y,z)
}


a = c(1.1785227,0.3941443)
b = c(4.979057,3.369097)
c = c(5.713762,4.822034)
is_3(a,b,c)

# type 3
a = c(3,7)
b = c(6,3)
c = c(4,5)

a = c(2.6268049 ,0.4881857)
b= c(4.147312 ,1.490541)
c= c( 6.977384 ,7.465397)


a = c(5.610634, 8.301991)
b= c(8.304375 ,4.946772)
c= c(9.651002, 4.317818)

plotTYPE(x,y,z)
plotTYPE(a,b,c)
type (a,b,c)
P = matrix(c(x,y,z), nrow = length(x))

plotTR2(P, curvature(P))
