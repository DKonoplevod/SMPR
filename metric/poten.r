library("plotrix")

dist <- function(a, b) {
  return (sqrt(sum( (a - b) ^ 2)))
}

core3 <- function(r) {
  if (abs(r) > 1) {
    return (0)
  }
  return (1 - abs(r))
}
core4 <- function(r) {
  (2*pi)^0.5 * exp(-0.5 * r*r)
}

potentialsMethod <- function(data, p, core, g, h) 
{
  len <- dim(data)[1]
  classCount <- dim(table(data$Species))
  
  classes <- rep(0, classCount)
  names(classes) <- levels(data$Species)
  
  for (i in seq(len)) {
    e <- data[i,]
    distance <- dist(p, e[1:2])
    
    weight <- g[i] * core(distance / h[i])
    classes[e$Species] <- classes[e$Species] + weight
  }
  
  if (max(classes) == 0) {
    return ("")
  }
  return (names(which.max(classes)))
}

countErrors <- function(data, core, g, h) {
  len <- dim(data)[1]
  err <- 0
  
  for (i in seq(len)) {
    e <- data[i,]
    res <- potentialsMethod(data, e[1:2], core, g, h)
    
    if (res != e$Species)
    {
      err <- err + 1
    }
  }
  
  return(err)
}

findPotentials <- function(data, core, h, maxErr = 5) {
  len <- dim(data)[1]
  
  g <- rep(0, len)
  i <- 1
  while(countErrors(data, core, g, h) > maxErr) {
    e <- data[i,]
    res <- potentialsMethod(data, e[1:2], core, g, h)
    
    if (res != e$Species)
    {
      g[i] <- g[i] + 1
    }
    
    i <- sample(seq(len), 1)
  }
  
  return(g)
}

demo <- function(dat, g, h) 
{
  colors <- c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")
  plot(dat[1:2], pch = 21, col = colors[dat$Species], bg = colors[dat$Species], main="Демонстрация потенциалов")
  
  for (i in seq(length(g))) {
    e <- dat[i,]
    if (g[i] < 1) {
      next
    }
    c <- adjustcolor(colors[e$Species], g[i] / max(g) * 0.2)
    draw.circle(e[,1], e[,2], h[i], col = c, border = c)
  }

  points(dat[1:2], pch = 21, col = "black", bg = colors[dat$Species], main=title)
}

map <- function(dat, g, h) {
  colors <- c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")
  plot(dat[1:2], pch = 21, col = colors[dat$Species], bg = colors[dat$Species], main="Карта классификации")
  
  for (i in seq(1.0, 7.0, 0.1)) {
    for (j in seq(0.1, 2.5, 0.05)) {
      cl <- potentialsMethod(dat, c(i, j), core4, g, h)
      points(i, j, pch = 21, col = colors[cl])
    }
  }
  
  points(dat[1:2], pch = 21, col = "black", bg = colors[dat$Species], main=title)
}

len <- dim(iris)[1]
h <- c(rep(1, len/3), rep(0.5, (len-len/3)))
print(h)
res <- findPotentials(iris[3:5], core3, h, maxErr = 5);

print(res)
demo(iris[3:5], res, h)
#map(iris[3:5], res, h)
