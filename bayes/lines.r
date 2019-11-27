library("shiny")

drawLines <- function(center, cv) {
  det <- det(cv)
  cv <- solve(cv)
  
  a <- cv[2, 2]
  b <- -cv[1, 2]
  c <- -cv[2, 1]
  d <- cv[1, 1]
  
  x0 <- center[1]
  y0 <- center[2]
  
  A <- a
  B <- d
  C <- b+c
  D <- -2*a*x0 - c*y0 - b*y0
  E <- -2*d*y0 - c*x0 - b*x0
  F <- a*x0*x0 + d*y0*y0 + b*x0*y0 + c*x0*y0
  
  func <- function(x, y) 
  {
    1 / (2*pi*sqrt(det)) * exp(-0.5 * (x^2*A + y^2*B + x*y*C + x*D + y*E + F))
  }
  
  X <- seq(-8, 8, len=100)
  Y <- seq(-8, 8, len=100)
  Z <- outer(X, Y, func)
  
  contour(X, Y, Z)
}


ui <- fluidPage(
  titlePanel("Линии уровня"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "a", label = "Дисперсия X", min = 0, max = 10, value = 5),
      sliderInput(inputId = "d", label = "Дисперсия Y", min = 0, max = 10, value = 5),
      sliderInput(inputId = "b", label = "Корреляция", min = 0, max = 10, value = 0),
      sliderInput(inputId = "x", label = "Смещение по X", min = -5, max = 5, value = 0),
      sliderInput(inputId = "y", label = "Смещение по Y", min = -5, max = 5, value = 0)
    ),
    mainPanel(plotOutput(outputId = "plot",width = "500px",height = "500px"))
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    m <- matrix(c(input$a, input$b, input$b, input$d), 2, 2)
    drawLines(c(input$x, input$y), m)
  })
}

shinyApp(ui, server)