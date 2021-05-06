
source("main.R")

library(shiny)


count <- function(a, hour) {
  x <- 0
  
  for (i in 1 : length(a)) {
    if (trunc(a[[i]]) == hour) {
      x <- x + 1
    }
  }
  
  return(x)
}

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
  plotOutput(outputId = "plot")
)

server <- function(input, output) {
  lista <- simulateOneDay()
  
  output$plot <- renderPlot({
    data <- data.frame(name = c("A1", "A2", "D", "lost_client_timestamps"),
                       value = countTheTimestamps(lista, input$slider))
    
    barplot(height = data$value, names = data$name)
  })
}

shinyApp(ui = ui, server = server)
