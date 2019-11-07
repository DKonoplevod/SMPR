dist <- function(a, b) {
  return (sqrt(sum( (a - b) ^ 2)))
}

# kwNN algo
kwnn <- function(dat, p, k=c(6), q = c(1)) {
  # calculate distances to each node in data
  dists <- vector("list", length(dat[[1]]))
  for (i in 1:length(dat[[1]])) {
    dists[[i]] <- dist(dat[i,][1:2], p)
  }
  
  # add distance to initial data
  dat <- data.frame(dat, "Distance" = unlist(dists))
  # sort data by distance
  dat <- dat[order(dat$Distance),]
  
  lk <- length(k)
  lq <- length(q)
  # result matrix with k values as rows
  # and q values as columns
  # matrix values are classification result 
  res <- array(0, c(lq, lk))
  for (iq in seq(lq)) {
    w <- q[iq]
    
    for (ik in seq(lk)) {
      kv <- k[ik]
      # how much p is close to each class
      freq <- as.list(rep(0, length(levels(dat$Species))))
      names(freq) = levels(dat$Species)
      
      for (j in seq(kv)) {
        e <- dat[j,]
        freq[[e$Species]] <- freq[[e$Species]] + w ^ j
      }
      
      res[iq, ik] <- names(sort(unlist(freq), decreasing = TRUE))[1]
    }
  }
  
  return (res)
}

# kNN algo
knn <- function(dat, p, k = c(6)) {
  # calculate distances to each node in data
  dists <- vector("list", length(dat[[1]]))
  for (i in 1:length(dat[[1]])) {
    dists[[i]] <- dist(dat[i,][1:2], p)
  }
  
  # add distance to initial data
  dat <- data.frame(dat, "Distance" = unlist(dists))
  # sort data by distance
  dat <- dat[order(dat$Distance),]
  
  res <- list()  
  for (i in seq(length(k))) {
    # take first k values from data
    datK <- head(dat, k[i])
    # count occurances of each group
    occs <- summary(datK$Species)
    # most occuring group  
    res[i] <- names(sort(occs, decreasing = TRUE))[1]
  }
  
  return (unlist(res))
}


proof <- function() {
  a1 <- seq(0, 5, 0.5)
  b1 <- unlist(lapply(a1, function(x) { 2*x - 2 }))
  a2 <- seq(0.5, 5, 0.5)
  b2 <- unlist(lapply(a2, function(x) { 2*x - 3 }))
  a <- c(a1, a2)
  b <- c(b1, b2)
  
  c1 <- seq(0, 5, 1)
  d1 <- unlist(lapply(c1, function(x) { 2*x + 4 }))
  c2 <- seq(0.5, 5, 1)
  d2 <- unlist(lapply(c2, function(x) { 2*x + 3 }))
  a <- c(a, c1, c2)
  b <- c(b, d1, d2)
  
  colors <- factor(c(rep("red", 21), rep("green", 11)))
  cn <- levels(colors)
  df <- data.frame(x=a, y=b, Species=colors)
  plot(df[1:2], pch=22, bg=cn[df$Species], col=cn[df$Species])
  
  for (i in seq(0, 5, 0.1)) {
    for (j in seq(-2, 14, 0.5)) {
      p <- c(i, j)
      res1 <- knn(df, p)
      res2 <- kwnn(df, p, q=c(0.5))
      if (res1 != res2) {
        points(p[1], p[2], pch=21, col=res1, bg=res1)
        points(p[1], p[2], pch=1, col=res2, bg=res2, lwd=2)
      }
      else
      {
        points(p[1], p[2], pch=21, col=res1)
      }
    }
  }
  
  points(df[1:2], pch=21, bg=cn[df$Species], col=cn[df$Species])
}

proof()