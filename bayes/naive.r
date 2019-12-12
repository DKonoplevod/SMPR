library(shiny)
library(MASS)

ui <- fluidPage(
  titlePanel("Наивный нормальный байесовский классификатор"),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        tags$h2("Класс 1", style = "color: red"),
        numericInput(inputId = "n", label = "Количество элементов", min = 100, max = 500, value = 200, step = 50),
        numericInput(inputId = "sigma1a", label = "Элемент ковариационной матрицы [1, 1]", min = 0.1, max = 20, value = 1, step = 0.1),
        numericInput(inputId = "sigma1b", label = "Элемент ковариационной матрицы [2, 2]", min = 0.1, max = 20, value = 1, step = 0.1),
        numericInput(inputId = "mu1a", label = "Мат. ожидание X", min = 0, max = 20, value = 8),
        numericInput(inputId = "mu1b", label = "Мат. ожидание Y", min = 0, max = 20, value = 0)
      ),
      wellPanel(
        tags$h2("Класс 2", style = "color: blue"),
        numericInput(inputId = "m", label = "Количество элементов", min = 100, max = 500, value = 200, step = 50),
        numericInput(inputId = "sigma2a", label = "Элемент ковариационной матрицы [1, 1]", min = 0.1, max = 20, value = 1, step = 0.1),
        numericInput(inputId = "sigma2b", label = "Элемент ковариационной матрицы [2, 2]", min = 0.1, max = 20, value = 5, step = 0.1),
        numericInput(inputId = "mu2a", label = "Мат. ожидание X", min = 0, max = 20, value = 2),
        numericInput(inputId = "mu2b", label = "Мат. ожидание Y", min = 0, max = 20, value = 6)
      )
    ),
    mainPanel(plotOutput(outputId = "plot", height =  "600px",))
  )
)

getM <- function(xs) {
  l <- dim(xs)[2]
  res <- matrix(NA, 1, l)
  for (i in seq(l)) {
    res[1, i] <- mean(xs[,i])
  }
  return(c(res))
}

getD <- function(xs, mu) {
  rows <- dim(xs)[1]
  cols <- dim(xs)[2]
  res <- matrix(0, 1, cols)
  for (i in seq(rows)) {
    res <- res + (xs[i,] - mu)^2
  }
  return(c(res/(rows-1)))
}

getP <- function(x, M, D){
  return( (1/(D*sqrt(2*pi))) * exp(-1 * ((x - M)^2)/(2*D^2)) )
}

naiveBayes <- function(x, M, D) {
  res <- log(1/2)
  l <- length(x)
  for (i in seq(l)) {
    p <- getP(x[i], M[i], D[i])
    res <- res + log(p)
  }
  return(res)
}

server <- function(input, output) {
  output$plot = renderPlot({
    xc1 <- mvrnorm(
      n=input$n, 
      mu = c(input$mu1a, input$mu1b), 
      Sigma = matrix(c(input$sigma1a, 0, 0, input$sigma1b), 2, 2)
    )
    xc2 <- mvrnorm(
      n=input$m, 
      mu = c(input$mu2a, input$mu1b),
      Sigma = matrix(c(input$sigma2a, 0, 0, input$sigma2b), 2, 2))
    
    plotxmin <- min(xc1[,1], xc2[,1]) - 1
    plotymin <- min(xc1[,2], xc2[,2]) - 1
    plotxmax <- max(xc1[,1], xc2[,1]) + 1
    plotymax <- max(xc1[,2], xc2[,2]) + 1
    
    m1 <- getM(xc1)
    m2 <- getM(xc2)
    d1 <- getD(xc1, m1)
    d2 <- getD(xc2, m2)
    
    x <- seq(plotxmin, plotxmax, 0.2)
    y <- seq(plotymin, plotymax, 0.2)
    
    plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax))
    
    colors <- c("red", "blue")
    points(xc1, pch=21, col=colors[1], bg=colors[1])
    points(xc2, pch=21, col=colors[2], bg=colors[2])
    
    #Карта классификаци
    for (i in x) {
      for (j in y) {
        res1 <- naiveBayes(c(i, j), m1, d1)
        res2 <- naiveBayes(c(i, j), m2, d2)
        color <- ifelse(res1 > res2, colors[1], colors[2])
        points(i, j, pch = 21, col = color)
      }
    }
    
  })
}

shinyApp(ui, server)