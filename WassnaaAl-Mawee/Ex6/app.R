# Load necessary libraries
library(shiny)
library(httr)

# Define the UI
ui <- fluidPage(
  titlePanel("ChatGPT API Demo"),
  sidebarLayout(
    sidebarPanel(
      textInput("input_text", "Enter your message:"),
      actionButton("submit_button", "Submit")
    ),
    mainPanel(
      verbatimTextOutput("output_text")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Function to make API call to ChatGPT
  call_chatGPT <- function(message) {
    # Define the API endpoint
    endpoint <- "https://api.openai.com/v1/chat/completions"
    
    # Define the request body
    request_body <- list(
      model = "text-davinci-003",  # Specify the ChatGPT model
      prompt = message,
      max_tokens = 50  # Maximum number of tokens in the response
    )
    
    # Make the API request
    response <- POST(endpoint, body = request_body, add_headers("Authorization" = paste("Bearer", api_key)))
    
    # Parse the response
    if (http_type(response) == "application/json") {
      content <- content(response, "parsed")
      return(content$output$text)
    } else {
      return("Error: Unable to fetch response from the API.")
    }
  }
  
  # Event handler for the submit button
  observeEvent(input$submit_button, {
    # Call the ChatGPT API with the input text
    output_text <- call_chatGPT(input$input_text)
    output$output_text <- renderText(output_text)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
