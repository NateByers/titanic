library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
source("global.R")

ui <- navbarPage(
  title = "Titanic",
  tabPanel("Data Explorer", column(2, wellPanel(
    radioButtons("survived", "Survival", c("All", "Yes", "No"),
                 "All")
  )),
  
  column(9, 
         plotOutput("age_plot", height = 250))),
  
  tabPanel("Survival"),
  
  tabPanel("Ticket Price")
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dashboard_data <- reactive({
    titanic %>%
      process_dashboard_data(input$survived)
  })
  
  output$age_plot <- renderPlot({
    data <- dashboard_data() %>%
      dplyr::mutate(survived = factor(survived, 0:1, c("No", "Yes")))
    p <- ggplot(data, aes(age, fill = survived)) + 
      geom_histogram() + theme_minimal()
    p
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


