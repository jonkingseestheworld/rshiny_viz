
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
  value = c(10, 23, 15, 8, 12)
)





# UI
ui <- fluidPage(
  titlePanel("Simple RShiny App with echarts4r"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("fruit", "Select up to Two Fruits:", 
                  choices = unique(data$category), 
                  selected = NULL, #c("Apples", "Bananas"),
                  multiple = TRUE,
                  #selectize = TRUE,  # Enable selectize
                  options = list(maxItems = 2,  placeholder = "Choose up to 2 fruits")  # Restrict to 2 selections
                  ),
      actionButton("reset_button", "Reset Selection")
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
    updateSelectizeInput(session, "fruit", selected = character(0))
  })
  
  # Debug Mode
  observeEvent(input$debug_button, {
    cat("Debug mode activated!\n")
    browser()  # Pauses the app and enters debug mode
  })
  
  # Render Bar Plot
  output$bar_plot <- renderEcharts4r({
    
    req(input$fruit)  # Ensure input is not NULL
    
    data %>%
      filter(category %in% input$fruit) %>%
      e_charts(category) %>%
      e_bar(value, name = "Fruit Count") %>%
      e_title("Fruit Selection Bar Chart", "Interactive with Shiny Input") %>%
      e_tooltip(trigger = "axis") %>%
      e_color(c("#3498db", "#e74c3c"))
  })
}

# Run the app
shinyApp(ui, server)