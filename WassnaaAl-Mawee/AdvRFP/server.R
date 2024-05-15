server <- function(input, output, session) {
  
  # Define reactive values to track login status
  login_status <- reactiveValues(logged_in = FALSE)
  
  # Define server logic for login page
  observeEvent(input$login_button, {
    # Perform login validation
    if (input$username == "admin" && input$password == "password") {
      # Successful login
      login_status$logged_in <- TRUE
    } else {
      # Failed login: Show error message
      showModal(modalDialog(
        title = "Error",
        "Invalid username or password. Please try again."
      ))
    }
  })
  
  # Define UI for main page (GPT demo)
  output$main_ui <- renderUI({
    req(login_status$logged_in)  # Require user to be logged in
    fluidPage(
      titlePanel("GPT Demo"),
      sidebarLayout(
        sidebarPanel(),
        mainPanel(
          # Input requests for GPT demo
        )
      )
    )
  })
  
  # Hide the main UI initially
  observe({
    shinyjs::hide("main_ui")
  })
  
  # Show the main UI upon successful login
  observeEvent(login_status$logged_in, {
    if (login_status$logged_in) {
      shinyjs::show("main_ui")
    }
  })
}
