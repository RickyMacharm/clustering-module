# Basic Shiny Script :)

# remeber to install this if you haven't already!
library(shiny)

# The UI :)
ui <- fluidPage(
  
  # App title
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    # Sidebar panel 
    sidebarPanel(
      # Input: Slider 
      sliderInput(inputId = "bins",
                  label = "On a scale of 1 to 100, how lame is hello world:",
                  min = 1,
                  max = 100,
                  value = 74)
      
    ),
    mainPanel(
      h3("main panel is here"),
      textOutput("number")
      )
  )
)

# Define server logic 
server <- function(input, output) {

  output$number <- renderText({ 
    input$bins + 10
  })
  
}

# Create Shiny app
shinyApp(ui = ui, server = server)