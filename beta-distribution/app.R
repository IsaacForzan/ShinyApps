library(shiny)
library(ggplot2)

betapdf <- function(alpha, beta) {
  x <- seq(0, 1, length.out = 100)
  y <- dbeta(x, alpha, beta)
  
  plot(x, y, type = "l", col = "blue", lwd = 2,
       main = "Beta Distribution",
       xlab = "Parameter", ylab = "Density")
  polygon(c(x, rev(x)), c(y, rep(0, length(y))), 
          col = rgb(0, 0, 1, 0.3), border = NA)
}

ui <- fluidPage(
  titlePanel("Beta Distribution Visualizer"),
  
  fluidRow(
    column(3, numericInput("alpha", "Alpha", value = 1, min = 0.1, step = 0.1)),
    column(3, numericInput("beta", "Beta", value = 1, min = 0.1, step = 0.1))
  ),
  
  plotOutput("plot")
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    betapdf(input$alpha, input$beta)
  })
}

shinyApp(ui, server)