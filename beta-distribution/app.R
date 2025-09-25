
library(shiny)
library(tidyverse)
library(bslib)

betapdf <- function(alpha, beta) {
  df <- tibble(
    x = seq(-0.2, 1.2, 0.01),
    y = dbeta(x, alpha, beta)
  )
  
  ggplot(df, aes(x, y)) +
    geom_area(fill = "lightblue", alpha = 0.5) +  # alpha = transparency
    geom_line(color = "blue", linewidth = 1) +
    labs(
      title = "Beta distribution",
      x = "Parameter",
      y = "Density"
    ) + theme_bw()
}

betapdf(1,2)

ui1 <- fluidPage(
  column(1,
         numericInput("numalpha", "α", value = 1, min = 0, max = 100, step = 0.1)
         ),
  
  column(1,
         numericInput("numbeta", "β", value = 1, min = 0, max = 100,  step = 0.1)
         ),
  
  plotOutput("posteriorPlot")
)

server1 <- function(input, output, session) {
  output$posteriorPlot <- renderPlot({
  
    alpha <- input$numalpha
    beta  <- input$numbeta
    
    betapdf(alpha, beta)
  }, res = 96)
}

shinyApp(ui1, server1)
