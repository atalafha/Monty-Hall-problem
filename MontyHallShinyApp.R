library(shiny)
library(DiagrammeR)
library(shiny)

ui <- fluidPage(
  titlePanel("Monty Hall Problem Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("num_sims", "Number of Simulations:", 1000, min = 1, max = 10000),
      actionButton("simulate", "Run Simulation")
    ),
    
    mainPanel(
      plotOutput("barPlot")
    )
  )
)


library(ggplot2)

monty_hall_sim <- function(strategy, n) {
  win_count <- 0
  
  for (i in 1:n) {
    doors <- c("goat", "goat", "car")
    choice <- sample(1:3, 1)
    
    # If a car is chosen, Monty reveals one of the two goats.
    if (doors[choice] == "car") {
      goats <- which(doors == "goat")
      monty_reveals <- sample(goats, 1)
    } else {
      monty_reveals <- which(doors == "goat" & 1:3 != choice)[1]
    }
    
    # Determine new choice based on strategy
    if (strategy == "Switch") {
      choices <- setdiff(1:3, c(choice, monty_reveals))
      new_choice <- choices[[1]]
    } else {
      new_choice <- choice
    }
    
    # Check if the new choice is a car
    if (doors[new_choice] == "car") {
      win_count <- win_count + 1
    }
  }
  
  return(win_count / n)
}

server <- function(input, output) {
  observeEvent(input$simulate, {
    switch_wins <- monty_hall_sim("Switch", input$num_sims)
    stick_wins <- monty_hall_sim("Stick", input$num_sims)
    
    output$barPlot <- renderPlot({
      data <- data.frame(
        Strategy = c("Switch", "Stick"),
        Win_Probability = c(switch_wins, stick_wins)
      )
      ggplot(data, aes(x = Strategy, y = Win_Probability)) +
        geom_bar(stat = "identity", fill = c("blue", "red")) +
        labs(y = "Probability of Winning", title = "Monty Hall Problem Simulation") +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
        geom_text(aes(label=sprintf("%.2f%%", Win_Probability*100)), vjust=-0.5, size=4)
    })
  })
}




# Run the application 
shinyApp(ui = ui, server = server)
