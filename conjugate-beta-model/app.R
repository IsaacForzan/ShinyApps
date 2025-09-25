
library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Bayesian Coin Toss Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("alpha", "Prior Alpha (α)", value = 1, min = 0.1, step = 0.1),
      numericInput("beta", "Prior Beta (β)", value = 1, min = 0.1, step = 0.1),
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
    ggplot(data.frame(x, y), aes(x, y)) +
      geom_line(color = "blue", linewidth = 1.2) +
      labs(title = "Prior Beta Distribution", y = "Density", x = "Probability of Heads") +
      theme_bw()
  })
  
  # Simulation state
  rv <- reactiveValues(
    tosses = NULL,
    heads = 0,
    tails = 0,
    i = 0, 
    interval = 1000
  )
  
  # Run simulation when Start is pressed
  observeEvent(input$start, {
    rv$tosses <- rbinom(input$n_toss, 1, 0.5)  # fair coin (changed from 0.2)
    rv$heads <- 0
    rv$tails <- 0
    rv$i <- 0
    rv$interval <- round(20000 / input$n_toss) # distribute 20s evenly
    
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
      
      invalidateLater(rv$interval, session)
      simulateStep()
    }
  })
  
  # Histogram of tosses
  output$histPlot <- renderPlot({
    req(rv$i > 0)
    df <- data.frame(
      Outcome = c("Heads", "Tails"),
      Count = c(rv$heads, rv$tails)
    )
    
    ggplot(df, aes(x = Outcome, y = Count, fill = Outcome)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Heads" = "skyblue", "Tails" = "orange")) +
      labs(title = paste("Coin Tosses (", rv$i, " of ", input$n_toss, ")", sep = ""),
           y = "Count", x = "") +
      theme_bw()
  })
  
  # Posterior distribution
  output$posteriorPlot <- renderPlot({
    req(rv$i > 0)
    alpha_post <- input$alpha + rv$heads
    beta_post <- input$beta + rv$tails
    
    x <- seq(0, 1, length.out = 200)
    y <- dbeta(x, alpha_post, beta_post)
    
    ggplot(data.frame(x, y), aes(x, y)) +
      geom_line(color = "red", linewidth = 1.2) +
      labs(title = paste("Posterior Beta(", alpha_post, ",", beta_post, ")", sep = ""),
           y = "Density", x = "Probability of Heads") +
      theme_bw()
  })
}

shinyApp(ui, server)