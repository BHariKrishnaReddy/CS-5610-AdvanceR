library(shiny)
library(shinyjs)

# Define UI
ui <- fluidPage(
  # Use shinyjs to run JavaScript
  useShinyjs(),
  # Title
  titlePanel("Chatbot For Group 7"),
  # Input form
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Enter your name:", ""),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      textOutput("result")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive variable to track redirection
  redirected <- reactiveVal(FALSE)
  observeEvent(input$submit, {
    if (!redirected()) {
      # Redirect to chatbot API URL
      js <- sprintf("window.location.href = 'https://new-interface-bedfc8.zapier.app/chatbot';")
      runjs(js)
      # Update redirection status
      redirected(TRUE)
    } else {
      # Display message if already redirected
      output$result <- renderText("You have already been redirected to the chatbot.")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)