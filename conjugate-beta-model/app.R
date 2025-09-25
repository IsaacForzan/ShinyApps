
library(shiny)

ui <- fluidPage(
  titlePanel("Bayesian Coin Toss Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("alpha", "Prior Alpha", value = 1, min = 0.1, step = 0.1),
      numericInput("beta", "Prior Beta", value = 1, min = 0.1, step = 0.1),
      numericInput("n_toss", "Number of tosses", value = 20, min = 1, step = 1),
      actionButton("start", "Start Simulation")
    ),
    
    mainPanel(
      plotOutput("priorPlot"),
      plotOutput("histPlot"),
      plotOutput("posteriorPlot")
    )
  )
)

server <- function(input, output, session) {
  
  # Plot the prior distribution
  output$priorPlot <- renderPlot({
    x <- seq(0, 1, length.out = 200)
    y <- dbeta(x, input$alpha, input$beta)
    plot(x, y, type = "l", col = "blue", lwd = 2,
         main = "Prior Beta Distribution",
         xlab = "Probability of Heads", ylab = "Density")
    grid()
  })
  
  # Simulation state
  rv <- reactiveValues(
    tosses = NULL,
    heads = 0,
    tails = 0,
    i = 0
  )
  
  # Run simulation when Start is pressed
  observeEvent(input$start, {
    rv$tosses <- rbinom(input$n_toss, 1, 0.5)
    rv$heads <- 0
    rv$tails <- 0
    rv$i <- 0
    
    simulateStep()
  })
  
  # Recursive animation
  simulateStep <- reactive({
    if (rv$i < input$n_toss) {
      rv$i <- rv$i + 1
      toss <- rv$tosses[rv$i]
      if (toss == 1) {
        rv$heads <- rv$heads + 1
      } else {
        rv$tails <- rv$tails + 1
      }
      
      invalidateLater(1000, session)
      simulateStep()
    }
  })
  
  # Histogram of tosses
  output$histPlot <- renderPlot({
    req(rv$i > 0)
    counts <- c(rv$heads, rv$tails)
    names(counts) <- c("Heads", "Tails")
    
    barplot(counts, col = c("skyblue", "orange"),
            main = paste("Coin Tosses (", rv$i, " of ", input$n_toss, ")"),
            ylab = "Count")
  })
  
  # Posterior distribution
  output$posteriorPlot <- renderPlot({
    req(rv$i > 0)
    alpha_post <- input$alpha + rv$heads
    beta_post <- input$beta + rv$tails
    
    x <- seq(0, 1, length.out = 200)
    y <- dbeta(x, alpha_post, beta_post)
    
    plot(x, y, type = "l", col = "red", lwd = 2,
         main = paste("Posterior Beta(", alpha_post, ",", beta_post, ")"),
         xlab = "Probability of Heads", ylab = "Density")
    grid()
  })
}

shinyApp(ui, server)