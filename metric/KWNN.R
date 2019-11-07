euclideanDistance <- function(u, v)
{
  sqrt(sum(u - v)^2)
}

weightsKWNN = function(i, q)
{
  q^i
}

sortObjectByDist <- function(xl, z, metricFunction = euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  distances <- rep(0, l)
  for (i in 1:l)
  {
    distances[i] <- c(metricFunction(xl[i, 1:n], z))
  }
  orderedXl <- xl[order(distances), ]
  return (orderedXl)
}

kwnn <- function(xl, z, k, q, orderedXl)
{
  
  n <- dim(orderedXl)[2] - 1
  weights = rep(0,3)
  names(weights) <- c("setosa", "versicolor", "virginica")
  classes <- orderedXl[1:k, n+1]
  
  for(i in 1:k)
  {
    weights[classes[i]]<-weightsKWNN(i,q)+weights[classes[i]];
  }
  class <- names(which.max(weights))
  return (class)
}

#LOO = function(xl,class) {
#  n = dim(xl)[1]
#  loo = rep(0, n-1)
#  
#  for(i in 1:(n)){
#    X=xl[-i, 1:3]
#    u=xl[i, 1:2]
#    orderedXl <- sortObjectByDist(X, u)
#    
#    for(k in 1:10){
#      test=kwnn(X,u,k,0.6,orderedXl)
#      if(colors[test] != colors[class[i]]){
#        loo[k] = loo[k]+1;
#      }    
#    } 
#  }
#  loo = loo / n
#  x = 1:(length(loo))
#  y = loo
#  plot(x, y,main ="LOO for KWNN(k) k=7", xlab="k", ylab="LOO", type = "l")
#  
#  min=which.min(loo)
#  points(min, loo[min], pch = 21, col = "red",bg = "red")
#}

colors <- c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")
xl <- iris[, 3:5] 
class <- iris[, 5]
LOO(xl,class)