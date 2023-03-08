library(shiny)
library(tidyverse)
tro <- read.delim("UAH-lower-troposphere-long.csv")
ui <- fluidPage(
  tabsetPanel(
    tabPanel("About",
             "This app uses satellite temperature data from",strong("UAH"),
             br(),
             br(),
             "Temperature", em("temp"),
             "is measured as deviation (deg C) from 1991-2020 baseline",
             br(),
             br(),
             "The dataset contains 14310 observations and 5 variables. Here is a small (random) sample of data:",
             mainPanel(tableOutput("firtab"))),
    tabPanel("Plots",
             sidebarLayout(
               sidebarPanel("You can analyze the global temperature for different regions. Select the regions you are interested in. You will see a monthly scatterplot and the corresponding trend lines.",
                           fluidRow(column(6,
                                           checkboxInput("show_trend", "Display trend(s)", value = FALSE)
                                           ),
                                    column(6,
                                           uiOutput("checkboxregion") )
                                    )
                           ),
                            
               mainPanel(plotOutput("plot"))
             )
    ),
    tabPanel("Table",
             sidebarLayout(
               sidebarPanel (
                 "This panel displays average temperature over different time periods: months , years and decades",
                 radioButtons("time","choose time", choices = c("month","year","decade"))
               ),
               mainPanel(
                 tableOutput("table")
               )
             ))
  )
)

server <- function(input, output) {
  # Create reactive data subset based on selected regions
  sample1 <- reactive({
    tro %>%
      filter(region %in% input$region)
  })
  
  output$checkboxregion <- renderUI({
    checkboxGroupInput("region", "Choose region(s)", choices = unique(tro$region))
  })
  

    

  
  output$firtab <- renderTable({
    tro %>%
      sample_n(5)
  })
  

  output$plot <- renderPlot({
    ggplot(sample1(), aes(year, temp)) +
      geom_point() +
      if (input$show_trend == TRUE) {
        geom_smooth(method = "lm", se = FALSE)
      }
  })
  
  output$table <- renderDataTable({
    
    if (input$average_over == "year") {
      temp_data <- tro %>%
        group_by(year) %>%
        summarize(temp = mean(temp))
    } else if (input$average_over == "decade") {
      temp_data <- tro %>%
        group_by(decade) %>%
        summarize(temp = mean(temp))
    } else {
      temp_data <- tro %>%
        group_by(month) %>%
        summarize(temp = mean(temp))
    }
    
   
    temp_data <- temp_data %>%
      filter(temp >= -0.356153846153846 & temp <= 0.216216216216216)
    
    
    temp_data$decade <- ifelse(
      input$average_over == "decade",
      format(temp_data$decade, scientific = FALSE, nsmall = 2),
     
    )
    
    
  })
}

shinyApp(ui = ui, server = server)