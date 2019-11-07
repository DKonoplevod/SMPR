euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}

sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  orderedXl <- xl[order(distances[, 2]), ]
  return (orderedXl);
}

looknn <- function(len, data)
{
  result = c(rep(0, length( seq(1, len, 1) ) ) )
  
  for(i in c(1:length(data[,1]) ) ){
    xl <- data[-i,]
    z <- data[i,1:2]
    
    orderedXl <- sortObjectsByDist(xl, z)
    n <- dim(orderedXl)[2] - 1 
    
    for (k in seq(1, len, 1)){
      classes <- orderedXl[1:k, n+1]
      count <- table(classes)
      class <- names(which.max(count))
      
      if(class != data[i,3]){
        result[k] = result[k] + 1
      }
    }   
  }  
  
  return(result)
}

iris30 = iris[3:5]
len <- length(iris30[,1])
loo <- looknn(len, iris30) / len
plot(c(1:len), loo, type ="n", xlab='k', ylab='fails')
lines(c(1:len), loo, type="l", pch=22, lty=1, col="black")
print(which.min(loo))
points(which.min(loo), min(loo) ,pch = 21, bg = "red", col="red", asp = 1)