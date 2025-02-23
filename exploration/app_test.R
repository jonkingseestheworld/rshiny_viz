
## Preamble
#install.packages("shiny")
#install.packages("echarts4r")
#install.packages("dplyr") 

library('shiny')
library('echarts4r')
library('dplyr')


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
      #selectizeInput("fruit", "Select up to Two Fruits:", 
      #            choices = unique(data$category), 
      #            selected = NULL, #c("Apples", "Bananas"),
      #            multiple = TRUE,
      #            #selectize = TRUE,  # Enable selectize
      #            options = list(maxItems = 2,  placeholder = "Choose up to 2 fruits")  # Restrict to 2 selections
      #            ),
      selectInput("year", "Select Year:", 
                  choices = c("Year 1" = "year_1", 
                              "Year 3" = "year_3", 
                              "Year 5" = "year_5"), 
                  selected = "year_1"),
      actionButton("reset_button", "Reset Selection"),
      br(), 
      actionButton("simulate_click", "Simulate Click on 'Apples'"),  # New button to simulate a click
      
      br(),
      h4("Clicked Bar Information:"),
      #verbatimTextOutput("click_info")  # Output for clicked bar information
      textOutput("click_info"),  # Changed from verbatimTextOutput to textOutput
      
      # JavaScript Handler for Safe Input Update
      tags$script(HTML("
          Shiny.addCustomMessageHandler('updateInput', function(message) {
              console.log('Custom input update:', message);
              Shiny.setInputValue(message.id, message.value, {priority: 'event'});
          });
      "))
      
    ),
    
    mainPanel(
      actionButton("debug_button", "Enter Debug Mode"), 
      
      echarts4rOutput("bar_plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reset Input Selection
  observeEvent(input$reset_button, {
    #updateSelectizeInput(session, "fruit", selected = character(0))
    updateSelectInput(session, "year", selected = "year_1")
    session$sendCustomMessage(type = "updateInput", message = list(id = "bar_clicked"))
    #Shiny.setInputValue('bar_clicked', null, {priority: 'event'})  # Properly reset input
    
  })
  
  # Debug Mode
  observeEvent(input$debug_button, {
    cat("Debug mode activated!\n")
    browser()  # Pauses the app and enters debug mode
  })
  
  
  # Simulate Click on 'Apples'
  observeEvent(input$simulate_click, {
    cat("Simulated click on 'Apples'\n")
    #Shiny.setInputValue('bar_clicked', 'Apples', {priority: 'event'})
    session$sendCustomMessage(type = "updateInput", message = list(id = "bar_clicked", value = "Apples"))
    
  })
  
  
  # Render Bar Plot
  output$bar_plot <- renderEcharts4r({
    
    #req(input$fruit, input$year)  # Ensure input is not NULL
    req(input$year)
    
    data %>%
      #filter(category %in% input$fruit) %>%
      e_charts(category) %>%
      e_bar_(input$year, name = paste("Sales in", gsub("year_", "Year ", input$year))) %>%
      e_title("Fruit Sales Bar Chart", paste("Sales Data for", gsub("year_", "Year ", input$year))) %>%
      e_tooltip(trigger = "axis") %>%
      e_color(c("#3498db", "#e74c3c")) %>%
      e_on("click", "function(params) { 
          console.log('Clicked bar name:', params.name);
          console.log('Clicked data:', params);

          Shiny.setInputValue('bar_clicked', params.name, {priority: 'event'}); 
      }")    
  })
  
  # Observe and Log Click Events
  observe({
    if (!is.null(input$bar_clicked)) {
      cat("RShiny received click event for:", input$bar_clicked, "\n")
    } else {
      cat("No click event received by Shiny yet.\n")
    }
  })
  
  # Display Clicked Bar Information
  output$click_info <- renderText({
    req(input$bar_clicked)
    paste("Bar clicked:", ifelse(is.null(input$bar_clicked), "None", input$bar_clicked))
  })
  
}

# Run the app
shinyApp(ui, server)