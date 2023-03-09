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
                 radioButtons("average_over","choose time", choices = c("month","year","decade"))
               ),
               mainPanel(
                 dataTableOutput(outputId = "table")
               )
             ))
  )
)

server <- function(input, output) {
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
        summarize(temp = mean(temp))%>%
        arrange(year)
    } else if (input$average_over == "decade") {
      temp_data <- tro %>%
        mutate(decade = 10*floor(year/10)
               )%>%
        group_by(decade) %>%
        summarize(temp = mean(temp))%>%
        arrange(decade)
    } else if (input$average_over == "month"){
      temp_data <- tro %>%
        group_by(month) %>%
        summarize(temp = mean(temp))%>%
        arrange(month)
    
    }
    
   
    
    
    
    
  })
}

shinyApp(ui = ui, server = server)