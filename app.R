library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
source("global.R")

ui <- navbarPage(
  title = "Titanic",
  tabPanel("Data Explorer", 
  
  column(6, plotlyOutput("survived_plot", height = 250),
         plotlyOutput("gender_plot", height = 250)),
  
  column(6, 
         plotlyOutput("age_plot", height = 250),
         plotlyOutput("class_plot", height = 250))),
  
  tabPanel("Ticket Price Prediction"),
  
  tabPanel("Survival Prediction")
  
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #dashboard_data <- reactive({
  #  titanic %>%
  #    process_dashboard_data(input$survived)
  #})
  
  output$age_plot <- renderPlotly({
    data <- titanic %>%
      dplyr::mutate(survived = factor(survived, 0:1, c("No", "Yes")))
    p <- ggplot(data, aes(age, fill = survived)) + 
      geom_histogram() + theme_minimal() + ggtitle("Age Distribution")
    ggplotly(p)
  })
  
  output$survived_plot <- renderPlotly({
    data <- titanic %>%
      dplyr::mutate(survived_label = ifelse(survived == 0, "No", "Yes"),
                    survived_pie = 1)
    plot_ly(data, labels =~survived_label, values = ~survived_pie, type = 'pie',
            colors = c("red", "green")) %>%
      layout(title = 'Total Survival',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$gender_plot <- renderPlotly({
    data <- titanic %>%
      dplyr::mutate(survived = factor(survived, 0:1, c("No", "Yes")))
    p <- ggplot(data, aes(sex, fill = survived)) + 
      geom_bar(position="dodge") + 
      theme_minimal()
    ggplotly(p)
  })
  
  output$class_plot <- renderPlotly({
    data <- titanic %>%
      dplyr::mutate(survived = factor(survived, 0:1, c("No", "Yes")))
    p <- ggplot(data, aes(pclass, fill = survived)) + 
      geom_bar(position="dodge") + 
      theme_minimal()
    ggplotly(p)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


