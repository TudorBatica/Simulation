
source("main.R")

library(shiny)

## functie care numara in vectorul a cate ore au partea intreaga data
## in parametrul hour

count <- function(a, hour) {
  x <- 0
  
  for (i in 1 : length(a)) {
    if (trunc(a[[i]]) == hour) {
      x <- x + 1
    }
  }
  
  return(x)
}

## functie care returneaza un vector cu 4 valori, reprezentand pt fiecare
## vector din lista de vectori, numarul de ore care au partea intreaga data
## in parametrul hour

countTheTimestamps <- function(lista, hour) {
  
  a1 <- lista[[1]]
  
  a1Count <- count(a1, hour)
  
  print(a1Count)
  
  a2 <- lista[[2]]
  
  a2Count <- count(a2, hour)
  
  d <- lista[[3]]
  
  dCount <- count(d, hour)
  
  lost_client_timestamps <- lista[[4]]
  
  lost_client_timestamps_count <- count(lost_client_timestamps, hour)
  
  return(c(a1Count, a2Count, dCount, lost_client_timestamps_count))
  
}

ui <- fluidPage(
  sliderInput(inputId = "slider", label = "Choose a number", min = 1, max = 12,
              value = 5),
  plotOutput(outputId = "plot"),
  textInput(inputId = "text", label = "Choose the number of days"),
  actionButton(inputId = "button", label = "See the results"),
  verbatimTextOutput(outputId = "avgClientsServed"),
  verbatimTextOutput(outputId = "avgClientsLost")
)

server <- function(input, output) {
  lista <- simulateOneDay()
  
  output$plot <- renderPlot({
    data <- data.frame(name = c("A1", "A2", "D", "lost_client_timestamps"),
                       value = countTheTimestamps(lista, input$slider))
    
    barplot(height = data$value, names = data$name)
  })
  
  observeEvent(input$button, {
    averageList <- avgClientsStats(as.integer(input$text))
    
    output$avgClientsServed <- renderText({
      averageList[[1]]
    })
  
    output$avgClientsLost <- renderText({
      averageList[[2]]
    })
  })
}

shinyApp(ui = ui, server = server)
