library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)

titanic <- readxl::read_excel("data/titanic3.xls")
load("data/xgboost.rda")


ui <- navbarPage(
  title = "Titanic",
  tabPanel("Dashboard", 
  
  column(6, 
         plotOutput("survived_plot", height = 250),
         plotlyOutput("gender_plot", height = 250)),
  
  column(6, 
         plotlyOutput("age_plot", height = 250),
         plotlyOutput("class_plot", height = 250))),
  
  tabPanel("Raw Data",
           DT::dataTableOutput("raw")),
  
  tabPanel("Prediction",
           sidebarPanel(
             numericInput("age_input", "Age:", 
                          median(titanic$age, na.rm = TRUE)),
             selectInput("sex_input", "Sex:", c("female", "male")),
             radioButtons("class_input", "Passenger Class:",
                          c("1st" = 1, "2nd" = 2, "3rd" = 3))
           ),
           mainPanel(
             column(6,
               plotOutput("survival_probability")
             ),
             column(6,
                    plotOutput("fare_prediction")
                    )
           )
           )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$age_plot <- renderPlotly({
    data <- titanic %>%
      dplyr::mutate(survived = factor(survived, 0:1, c("No", "Yes")))
    p <- ggplot(data, aes(age, fill = survived)) + 
      geom_histogram() + theme_minimal() 
    ggplotly(p)
  })
  
  output$survived_plot <- renderPlot({
    data <- titanic %>%
      dplyr::mutate(survived = factor(survived, 0:1, c("No", "Yes")))
    
    p <- ggplot(data, aes(x=factor(1), fill = survived))+
      geom_bar(width = 1) + coord_polar(theta = "y") + theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold"),
        axis.text.y=element_blank()
      )
    p
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
  
  output$raw <- DT::renderDataTable({
    titanic
  })
  
  output$survival_probability <- renderPlot({
    df <- data.frame(pclass = factor(input$class_input, levels = c("1", "2", "3")),
                     sex = factor(input$sex_input, levels = c("female", "male")),
                     age = input$age_input)
    sparse_matrix <- Matrix::sparse.model.matrix(~.-1, data = df)
    print(colnames(sparse_matrix))
    prediction <- predict(bst, newdata = sparse_matrix)
    plot_df <- data.frame(survival = "", probability = prediction)
    ggplot(plot_df, aes(survival, probability)) + geom_bar(stat = "identity") +
      expand_limits(y = c(0, 1)) + theme_minimal()
  })
  
  output$fare_prediction <- renderPlot({
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


