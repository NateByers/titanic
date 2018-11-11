library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)


titanic <- readxl::read_excel("data/titanic3.xls")
load("data/xgboost.rda")
categoricals <- c("pclass", "sex", "sibsp", "parch", "embarked")

ui <- navbarPage(
  title = "Titanic",
  
  tabPanel("Overview",
           column(9,
                  includeHTML("overview.html"),
                  offset = 1)
           ),

  tabPanel("Data",
           DT::dataTableOutput("raw")),
  
  tabPanel("Data Explorer",
           sidebarPanel(
             selectInput("categorical", "Categorical Plot",
                         categoricals),
             selectInput("numeric", "Numeric Plot",
                         c("age", "fare"))
           ),
           mainPanel(
             column(6,
                    plotOutput("categorical_plot")
             ),
             column(6,
                    plotOutput("numeric_plot")
             )
           )),
  
  tabPanel("Dashboard", 
  
  column(6, 
         plotOutput("survived_plot", height = 250),
         plotlyOutput("gender_plot", height = 250)),
  
  column(6, 
         plotlyOutput("age_plot", height = 250),
         plotlyOutput("class_plot", height = 250))),
  
  
  
  tabPanel("Prediction",
           sidebarPanel(
             numericInput("age_input", "Age:", 
                          median(titanic$age, na.rm = TRUE)),
             selectInput("sex_input", "Sex:", c("female", "male")),
             radioButtons("class_input", "Passenger Class:",
                          c("1st" = 1, "2nd" = 2, "3rd" = 3)),
             radioButtons("sibsp_input", "Sibling or Spouse:",
                          c("Yes" = 1, "No" = 0)),
             sliderInput("fare_input", "Fare:", 
                         min(titanic$fare, na.rm = TRUE),
                         round(max(titanic$fare, na.rm = TRUE)),
                         80, 10)
           ),
           mainPanel(
             column(6,
               plotOutput("fare_boxplot")
             ),
             column(6,
                    plotOutput("survival_probability")
                    )
           )
           )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$categorical_plot <- renderPlot({
    data <- titanic %>%
      dplyr::mutate(survived = factor(survived, 0:1, c("No", "Yes")))

    data[[input$categorical]] <- factor(data[[input$categorical]])
    p <- ggplot(data, aes_string(input$categorical, fill = "survived")) +
      geom_bar(position="dodge") +
      theme_minimal()
    p
  })
  
  output$numeric_plot <- renderPlot({
    df <- titanic %>%
      dplyr::mutate(survived = factor(survived, 0:1, c("No", "Yes")))
    
    ggplot(df, aes_string("survived", input$numeric, fill = "survived")) +
      geom_boxplot() 
  })
  
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
  
  output$raw <- DT::renderDataTable(
    datatable(titanic %>%
                dplyr::mutate(pclass = as.character(pclass),
                              survived = as.character(survived)) %>%
                dplyr::select(-name, -home.dest, -boat, -body),
      filter = 'top')
  )
  
  output$survival_probability <- renderPlot({
    df <- data.frame(pclass = factor(input$class_input, levels = c("1", "2", "3")),
                     sex = factor(input$sex_input, levels = c("female", "male")),
                     age = input$age_input,
                     sibsp = factor(input$sibsp_input, levels = c("0", "1")),
                     fare = input$fare_input)
    print(df)
    sparse_matrix <- Matrix::sparse.model.matrix(~.-1, data = df)
    prediction <- predict(bst, newdata = sparse_matrix)
    plot_df <- data.frame(survival = "", probability = prediction)
    ggplot(plot_df, aes(survival, probability)) + geom_bar(stat = "identity") +
      expand_limits(y = c(0, 1)) + theme_minimal()
  })
  
  output$fare_boxplot <- renderPlot({
    df <- titanic %>%
      dplyr::filter(pclass == input$class_input,
                    sex == input$sex_input) %>%
      dplyr::mutate(x = paste(sex, "\\ Class", pclass))
    
    x <- factor(unique(df$x))
    
    ggplot() +
      geom_boxplot(data = df, aes(x = x, y= fare)) +
      geom_point(data = data.frame(x = x, y = input$fare_input),
                 aes(x=x, y=y), color = 'red', size = 2)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


