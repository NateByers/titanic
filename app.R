library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)


titanic <- readxl::read_excel("data/titanic3.xls")
load("data/xgboost.rda")
categoricals <- c("pclass", "sex", "sibsp", "parch", "embarked")
key <- read.csv("data/key.csv", stringsAsFactors = FALSE)

ui <- navbarPage(
  title = "Titanic",
  
  tabPanel("Overview",
           column(9,
                  includeHTML("RMarkdown/overview.html"),
                  offset = 1)
           ),

  tabPanel("Data",
           tabsetPanel(type = "tabs",
                       tabPanel("DataTable", DT::dataTableOutput("raw")),
                       tabPanel("Key", tableOutput("key_table"))
                       )
           ),
  
  tabPanel("Data Explorer",
           column(5,
                  wellPanel(HTML("<b>Categorical Plot</b>"),
                            HTML("<p>select up to two variables</p>"),
                            selectInput("categorical", "", categoricals,
                                        categoricals[1], multiple = TRUE)),
                  plotOutput("categorical_plot"),
                  offset = 1),
           column(5,
                  wellPanel(HTML("<b>Numeric Plot</b>"),
                            HTML("<p>select one variable</p>"),
                            selectInput("numeric", "", c("age", "fare"))),
                  plotOutput("numeric_plot"),
                  offset = 1)
           
           ),
  
  tabPanel("Dashboard", 
  
  column(6, 
         plotOutput("survived_plot", height = 250),
         plotlyOutput("gender_plot", height = 250)),
  
  column(6, 
         plotlyOutput("age_plot", height = 250),
         plotlyOutput("class_plot", height = 250))),
  
  
  tabPanel("Prediction",
           sidebarPanel(HTML("<h4>Select characteristics of a passenger</h4>"),
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
             
             tabsetPanel(type = "tabs",
                         tabPanel("Plots", 
                                  column(6,
                                         plotOutput("fare_boxplot")
                                  ),
                                  column(6,
                                         plotOutput("survival_probability")
                                  )
                                  ),
                         tabPanel("Model", includeHTML("RMarkdown/model.html"))
             )
             
             
           )
           )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$key_table <- renderTable(key)
  
  output$categorical_plot <- renderPlot({
    data <- titanic %>%
      dplyr::mutate(survived = factor(survived, 0:1, c("No", "Yes")))

    data[[input$categorical[1]]] <- factor(data[[input$categorical[1]]])
    if(length(input$categorical) > 1) {
      data[[input$categorical[2]]] <- factor(data[[input$categorical[2]]])
    }
    
    p <- ggplot(data, aes_string(input$categorical[1], fill = "survived")) +
      geom_bar(position="dodge")
    if(length(input$categorical) > 1) {
      p <- p + facet_wrap(input$categorical[2])
    }
    
    p  + theme_minimal()
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
    # p <- ggplot(data, aes(age, fill = survived)) + 
    #   geom_histogram() + theme_minimal() 
    p <- ggplot(data, aes(age, fill = survived)) + facet_wrap(survived~.) +
      geom_histogram() + theme_minimal()# + ggtitle("Survival by Age")
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
      ) #+ ggtitle("Survival Totals")
    p
  })
  
  # output$survived_plot <- renderPlotly({
  #     data <- titanic %>%
  #       dplyr::mutate(survived = factor(survived, 0:1, c("No", "Yes")))
  #   p <- ggplot(data, aes(survived, fill = survived)) +
  #     geom_bar(stat = "count") + theme_minimal() + ggtitle("Survival Totals")
  #   ggplotly(p)
  # })
  
  output$gender_plot <- renderPlotly({
    data <- titanic %>%
      dplyr::mutate(survived = factor(survived, 0:1, c("No", "Yes")))
    p <- ggplot(data, aes(sex, fill = survived)) + 
      geom_bar(position="dodge") + 
      theme_minimal() #+ ggtitle("Survival by Sex")
    ggplotly(p)
  })
  
  output$class_plot <- renderPlotly({
    data <- titanic %>%
      dplyr::mutate(survived = factor(survived, 0:1, c("No", "Yes")))
    p <- ggplot(data, aes(pclass, fill = survived)) + 
      geom_bar(position="dodge") + 
      theme_minimal() #+ ggtitle("Survival by Class")
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

    sparse_matrix <- Matrix::sparse.model.matrix(~.-1, data = df)
    prediction <- predict(bst, newdata = sparse_matrix)
    plot_df <- data.frame(survival = "", probability = prediction)
    ggplot(plot_df, aes(survival, probability)) + geom_bar(stat = "identity") +
      expand_limits(y = c(0, 1)) + 
      ggtitle("Probability of Survival") +
      theme_minimal(base_size = 18)
  })
  
  output$fare_boxplot <- renderPlot({
    df <- titanic %>%
      dplyr::filter(pclass == input$class_input,
                    sex == input$sex_input) %>%
      dplyr::mutate(group = paste(sex, "\\ Class", pclass))
    
    group <- factor(unique(df$group))
    
    ggplot() +
      geom_boxplot(data = df, aes(x = group, y= fare)) +
      geom_point(data = data.frame(x = group, y = input$fare_input),
                 aes(x=x, y=y), color = 'red', size = 2) + 
      ggtitle("Fare Distribution by Group") +
      theme_minimal(base_size = 18)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


