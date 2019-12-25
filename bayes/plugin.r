library(shiny)
library(MASS)

ui <- fluidPage(
  titlePanel("Подстановочный (Plug-in) алгоритм"),
  sidebarLayout(
    sidebarPanel(
      actionButton("updateX", "Обновить выборки"),
      wellPanel(
        tags$h2("Класс 1", style = "color: red"),
        numericInput(inputId = "n", label = "Количество элементов", min = 100, max = 500, value = 200, step = 50),
        numericInput(inputId = "pr1", label = "Значимость", min = 1, max = 10, value = 1, step = 0.1),
        numericInput(inputId = "sigma1a", label = "Элемент ковариационной матрицы [1, 1]", min = 0.1, max = 20, value = 1, step = 0.1),
        numericInput(inputId = "sigma1b", label = "Элемент ковариационной матрицы [2, 2]", min = 0.1, max = 20, value = 1, step = 0.1),
        numericInput(inputId = "mu1a", label = "Мат. ожидание X", min = 0, max = 20, value = 8),
        numericInput(inputId = "mu1b", label = "Мат. ожидание Y", min = 0, max = 20, value = 0)
      ),
      wellPanel(
        tags$h2("Класс 2", style = "color: blue"),
        numericInput(inputId = "m", label = "Количество элементов", min = 100, max = 500, value = 200, step = 50),
        numericInput(inputId = "pr2", label = "Значимость", min = 1, max = 10, value = 1, step = 0.1),
        numericInput(inputId = "sigma2a", label = "Элемент ковариационной матрицы [1, 1]", min = 0.1, max = 20, value = 1, step = 0.1),
        numericInput(inputId = "sigma2b", label = "Элемент ковариационной матрицы [2, 2]", min = 0.1, max = 20, value = 5, step = 0.1),
        numericInput(inputId = "mu2a", label = "Мат. ожидание X", min = 0, max = 20, value = 2),
        numericInput(inputId = "mu2b", label = "Мат. ожидание Y", min = 0, max = 20, value = 6)
      )
    ),
    mainPanel(plotOutput(outputId = "plot", height =  "600px"))
  )
)

estimateMu <- function(xs) {
  l <- dim(xs)[2]
  res <- matrix(NA, 1, l)
  for (i in seq(l)) {
    res[1, i] <- mean(xs[,i])
  }
  return(res)
}

estimateSigma <- function(xs, mu) {
  rows <- dim(xs)[1]
  cols <- dim(xs)[2]
  
  res <- matrix(0, cols, cols)
  for (i in seq(rows)) {
    res <- res + t(xs[i,] - mu) %*% (xs[i,] - mu)
  }
  
  return(res/(rows - 1))
}

getFunc <- function(sigma1, mu1, sigma2, mu2) {
  d1 <- det(sigma1)
  d2 <- det(sigma2)
  invs1 <- solve(sigma1)
  invs2 <- solve(sigma2)
  
  a <- invs1 - invs2
  b <- invs1 %*% t(mu1) - invs2 %*% t(mu2)
  
  A <- a[1,1]
  B <- a[2,2]
  C <- 2 * a[1, 2]
  D <- -2 * b[1, 1]
  E <- -2 * b[2, 1]
  G <- c(mu1 %*% invs1 %*% t(mu1) - mu2 %*% invs2 %*% t(mu2)) + log(abs(det(sigma1))) - log(abs(det(sigma2)))
  
  func <- function(x, y) {
    x^2 * A + y^2 * B + x*y*C + x*D + y*E + G
  }
  
  return(func)
}

getConst <- function(x1, x2, l1, l2) {
  n1 <- dim(x1)[1]
  n2 <- dim(x2)[1]
  p1 <- n1 / (n1+n2)
  p2 <- n2 / (n1+n2)
  return( log((l1*p1)^2/(l2*p2)^2) )
}

plugin <- function(x, y, mu, cv, n, prior) {
  res <- log(prior) - n/2*log(2*pi)
  
  x0 <- mu[1]
  y0 <- mu[2]
  
  det <- det(cv)
  cv <- solve(cv)
  
  a <- cv[1, 1]
  b <- cv[1, 2]
  c <- cv[2, 1]
  d <- cv[2, 2]
  
  A <- a
  B <- d
  C <- b+c
  D <- -2*a*x0 - c*y0 - b*y0
  E <- -2*d*y0 - c*x0 - b*x0
  F <- a*x0*x0 + d*y0*y0 + b*x0*y0 + c*x0*y0
  
  res <- res - 0.5 * log(det) - 0.5 * (x^2*A + y^2*B + x*y*C + x*D + y*E + F)
  
  return(res)
}

server <- function(input, output) {
  
  getData <- eventReactive(input$updateX, {
    n <- input$n
    m <- input$m
    
    sigma1i <- matrix(c(input$sigma1a, 0, 0, input$sigma1b), 2, 2)
    sigma2i <- matrix(c(input$sigma2a, 0, 0, input$sigma2b), 2, 2)
    
    mu1i <- c(input$mu1a, input$mu1b)
    mu2i <- c(input$mu2a, input$mu2b)
    
    xc1 <- mvrnorm(n=n, mu = mu1i, Sigma = sigma1i)
    xc2 <- mvrnorm(n=m, mu = mu2i, Sigma = sigma2i)
    
    return (c(n, m, xc1, xc2))
  })
  
  output$plot = renderPlot({
    sigma1i <- matrix(c(input$sigma1a, 0, 0, input$sigma1b), 2, 2)
    sigma2i <- matrix(c(input$sigma2a, 0, 0, input$sigma2b), 2, 2)
    
    mu1i <- c(input$mu1a, input$mu1b)
    mu2i <- c(input$mu2a, input$mu2b)
    
    dataX <- getData()
    xc1 <- matrix(dataX[seq(3, dataX[1]*2+2)], dataX[1], 2)
    xc2 <- matrix(dataX[-seq(1, dataX[1]*2+2)], dataX[2], 2)
    
    prior1 <- input$pr1
    prior2 <- input$pr2
    
    plotxmin <- min(xc1[,1], xc2[,1]) - 1
    plotymin <- min(xc1[,2], xc2[,2]) - 1
    plotxmax <- max(xc1[,1], xc2[,1]) + 1
    plotymax <- max(xc1[,2], xc2[,2]) + 1
    
    mu1 <- estimateMu(xc1)
    mu2 <- estimateMu(xc2)
    sigma1 <- estimateSigma(xc1, mu1)
    sigma2 <- estimateSigma(xc2, mu2)
    
    x <- seq(plotxmin-5, plotxmax+5, len = 100)
    y <- seq(plotymin-5, plotymax+5, len = 100)
  
    plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax))
    
    colors <- c("red", "blue")
    points(xc1, pch=21, col=colors[1], bg=colors[1])
    points(xc2, pch=21, col=colors[2], bg=colors[2])
    
    contour(x, y, outer(x, y, getFunc(sigma1, mu1, sigma2, mu2)), levels = getConst(xc1, xc2, prior1, prior2), add = TRUE, drawlabels = TRUE, lwd = 2.5)
    
    for (i in x) {
      for (j in y) {
        res1 <- plugin(i, j, mu1, sigma1, input$n, prior1)
        res2 <- plugin(i, j, mu2, sigma2, input$m, prior2)
        color <- ifelse(res1 > res2, colors[1], colors[2])
        points(i, j, pch = 21, col = color)
      }
    }
  })
}

shinyApp(ui, server)
