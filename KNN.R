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

kNN <- function(orderedXL, z, k)
{
  n <- dim(orderedXl)[2] - 1
  classes <- orderedXl[1:k, n + 1]
  counts <- table(classes)
  class <- names(which.max(counts))
  return (class)
}

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1, xlab='', ylab='')

for (x in seq(0.8, 7, by=0.1)){
  for(y in seq(-0.5, 3, by=0.1)){
   z <- c(x,y)
   orderedXl <- sortObjectsByDist(iris[, 3:5], z)
   class <- kNN(orderedXl, k=6)
   points(z[1], z[2], pch = 1, col = colors[class], asp = 1)
  }
}
points(iris[, 3:4], pch = 1, col = colors[iris$Species], asp = 1)
