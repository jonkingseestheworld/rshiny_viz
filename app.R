
## Preamble
#install.packages("shiny")
#install.packages("echarts4r")
#install.packages("dplyr") 

library('shiny')
library('echarts4r')
library('dplyr')
library('DT')

# Sample Data
data <- data.frame(
  category = c("Apples", "Bananas", "Cherries", "Dates", "Elderberries"),
  year_1 = c(10, 23, 15, 8, 12),
  year_3 = c(20, 30, 25, 18, 22),
  year_5 = c(30, 45, 35, 28, 32)
  
)



# UI
ui <- fluidPage(
  titlePanel("Simple RShiny App with echarts4r"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("view_mode", "Choose View:",
                   choices = c("Show Data Table" = "table",
                               "Show Bar Plot" = "plot"),
                   selected = "table"),
      
      # Conditional Year Selection Input
      conditionalPanel(
        br(), 
        condition = "input.view_mode == 'plot'",
        selectInput("year", "Select Year:", 
                    choices = c("Year 1" = "year_1", 
                                "Year 3" = "year_3", 
                                "Year 5" = "year_5"), 
                    selected = "year_1")
      ),
      
      br(), br(),
      actionButton("reset_button", "Reset Selection")
    ) ,
    
    mainPanel(
     actionButton("debug_button", "Enter Debug Mode"), 
     # 
     # h3("Data Table: Fruit Sales Data"),
     # DTOutput("data_table"),  # Output for the data table
     # 
     # br(), br(),
     # 
     # echarts4rOutput("bar_plot")
      conditionalPanel(
        condition = "input.view_mode == 'table'",
        h3("Data Table: Fruit Sales Data"),
        DTOutput("data_table")
      ),
      
      conditionalPanel(
        condition = "input.view_mode == 'plot'",
        h3("Sales Bar Plot"),
        echarts4rOutput("bar_plot")
      )
      
    )
  )
)




# Server
server <- function(input, output, session) {
  
  # Reset Input Selection
  observeEvent(input$reset_button, {
    updateSelectInput(session, "year", selected = "year_1")
    updateSelectInput(session, "view_mode", selected = "table")
  })
  
  # Debug Mode
  observeEvent(input$debug_button, {
    cat("Debug mode activated!\n")
    browser()  # Pauses the app and enters debug mode
  })
  
  # Render Data Table
  output$data_table <- renderDT({
    datatable(data, options = list(pageLength = 5, autoWidth = TRUE))
  })
  

  # Render Bar Plot
  output$bar_plot <- renderEcharts4r({
    
    #req(input$fruit, input$year)  # Ensure input is not NULL
    req(input$view_mode == "plot", input$year) # Ensure input is valid and view is plot
    
    print(input$custom_bar_click)
    
    data %>%
      #filter(category %in% input$fruit) %>%
      e_charts(category) %>%
      e_bar_(input$year, name = paste("Sales in", gsub("year_", "Year ", input$year))) %>%
      e_title("Fruit Sales Bar Chart", paste("Sales Data for", gsub("year_", "Year ", input$year))) %>%
      e_tooltip(trigger = "axis") %>%
      e_color(c("#3498db", "#e74c3c")) %>%
      #e_on(
      #  list(seriesName = "year"),
      #  "function(){alert('Serie clicked')}"
      #)
      e_on(
        query = "series.bar",
        # Set input values
        handler = "function(params){
         Shiny.setInputValue(
          'custom_bar_click',
          {clicked_item: params.name}, {priority: 'event'}
         );
       }",
        event = "click"
      )

  })
}


# Run the app
shinyApp(ui, server)


