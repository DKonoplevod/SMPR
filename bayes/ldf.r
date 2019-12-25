library(shiny)
library(MASS)

ui <- fluidPage(
  titlePanel("Линейный дискриминант фишера (ЛДФ)"),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        tags$h2("Класс 1", style = "color: red"),
        numericInput(inputId = "n", label = "Количество элементов", min = 100, max = 500, value = 200, step = 50),
        numericInput(inputId = "mu1a", label = "Мат. ожидание X", min = 0, max = 20, value = 8),
        numericInput(inputId = "mu1b", label = "Мат. ожидание Y", min = 0, max = 20, value = 0)
      ),
      wellPanel(
        tags$h2("Класс 2", style = "color: blue"),
        numericInput(inputId = "m", label = "Количество элементов", min = 100, max = 500, value = 200, step = 50),
        numericInput(inputId = "mu2a", label = "Мат. ожидание X", min = 0, max = 20, value = 2),
        numericInput(inputId = "mu2b", label = "Мат. ожидание Y", min = 0, max = 20, value = 6)
      ),
      wellPanel(
        tags$h2("Ковариационная матрица"),
        numericInput(inputId = "sigma1a", label = "Элемент ковариационной матрицы [1, 1]", min = 0.1, max = 20, value = 1, step = 0.1),
        numericInput(inputId = "sigma1b", label = "Элемент ковариационной матрицы [2, 2]", min = 0.1, max = 20, value = 1, step = 0.1),
      )
    ),
    mainPanel(plotOutput(outputId = "plot", height =  "600px"),
    fluidRow(h4("Риск:"),h4(textOutput("risk")))
    )
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

estimateSigma <- function(xs1, mu1, xs2, mu2) {
  rows1 <- dim(xs1)[1]
  cols <- dim(xs1)[2]
  rows2 <- dim(xs2)[1]
  
  res <- matrix(0, cols, cols)
  for (i in seq(rows1)) {
    res <- res + t(xs1[i,] - mu1) %*% (xs1[i,] - mu1)
  }
  for (i in seq(rows2)) {
    res <- res + t(xs2[i,] - mu2) %*% (xs2[i,] - mu2)
  }
  
  return(res/(rows1 + rows2 + 2))
}

getFunc <- function(sigma1, mu1, mu2) {
  d1 <- det(sigma1)
  invs1 <- solve(sigma1)
  
  b <- invs1 %*% t(mu1 - mu2)
  
  D <- b[1, 1]
  E <- b[2, 1]
  mu <- (mu1 + mu2)
  G <- c(mu %*% b) / 2
  
  func <- function(x) {
    -x*D/E + G/E
  }
  
  return(func)
}

getRisk <- function(mu1, mu2, sigma) {
  mah <- -0.5 * (mu1 - mu2) %*% solve(sigma) %*% t(mu1 - mu2)
  res <- gausian(mah, 0, 1)
}
gausian <- function(x, M, D){
  return( (1/(D*sqrt(2*pi))) * exp(-1 * ((x - M)^2)/(2*D^2)) )
}

ldf <- function(x, y, mu, cv) {
  
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
  
  res <- - 0.5 * (x^2*A + y^2*B + x*y*C + x*D + y*E + F)
  
  return(res)
}

server <- function(input, output) {
  output$plot = renderPlot({
    
    n <- input$n
    m <- input$m
    
    sigma1i <- matrix(c(input$sigma1a, 0, 0, input$sigma1b), 2, 2)
    
    mu1i <- c(input$mu1a, input$mu1b)
    mu2i <- c(input$mu2a, input$mu2b)
    
    xc1 <- mvrnorm(n=n, mu = mu1i, Sigma = sigma1i)
    xc2 <- mvrnorm(n=m, mu = mu2i, Sigma = sigma1i)
    
    plotxmin <- min(xc1[,1], xc2[,1]) - 1
    plotymin <- min(xc1[,2], xc2[,2]) - 1
    plotxmax <- max(xc1[,1], xc2[,1]) + 1
    plotymax <- max(xc1[,2], xc2[,2]) + 1
    
    mu1 <- estimateMu(xc1)
    mu2 <- estimateMu(xc2)
    sigma1 <- estimateSigma(xc1, mu1, xc2, mu2)
    
    func <- getFunc(sigma1, mu1, mu2)
    
    plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax))
    
    colors <- c("red", "blue")
    points(xc1, pch=21, col=colors[1], bg=colors[1])
    points(xc2, pch=21, col=colors[2], bg=colors[2])
    
    x <- seq(plotxmin-5, plotxmax+5, len = 100)
    y <- seq(plotymin-5, plotymax+5, len = 100)
    lines(x, func(x), lwd = 2.5, type="l")
    lines(c(mu1[1], mu2[1]), c(mu1[2], mu2[2]), col = 'gray', lwd = 2)
    output$risk = renderText(getRisk(mu1, mu2, sigma1))
    
    for (i in x) {
      for (j in y) {
        res1 <- ldf(i, j, mu1, sigma1)
        res2 <- ldf(i, j, mu2, sigma1)
        color <- ifelse(res1 > res2, colors[1], colors[2])
        points(i, j, pch = 21, col = color)
      }
    }
  })
}

shinyApp(ui, server)