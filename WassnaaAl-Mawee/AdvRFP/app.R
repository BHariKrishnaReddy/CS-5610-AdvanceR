library(shiny)
library(shinyjs)

# Define UI for login page
login_ui <- fluidPage(
  titlePanel("Login"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      textInput("username", "Username"),
      passwordInput("password", "Password"),
      actionButton("login_button", "Login"),
      uiOutput("main_ui_button")  # Output for button to show main UI
    )
  )
)

# Define server logic for login page
login_server <- function(input, output, session) {
  observeEvent(input$login_button, {
    # Perform login validation
    if (input$username == "admin" && input$password == "password") {
      # Successful login: Show main page button
      output$main_ui_button <- renderUI({
        actionButton("show_main_ui", "Continue to GPT Demo")
      })
    } else {
      # Failed login: Show error message
      showModal(modalDialog(
        title = "Error",
        "Invalid username or password. Please try again."
      ))
    }
  })
}

# Define UI for main page (GPT demo)
main_ui <- fluidPage(
  titlePanel("GPT Demo"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      # Input requests for GPT demo
    )
  )
)

# Define server logic for main page (GPT demo)
main_server <- function(input, output, session) {
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

# Combine login and main pages into a single app
ui <- shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  login_ui,
  main_ui
))

server <- function(input, output, session) {
  callModule(login_server, "login")
  callModule(main_server, "main")
  
  observeEvent(input$show_main_ui, {
    # Show the main UI upon button click
    output$main_ui_button <- renderUI({
      main_ui
    })
    # Hide the login UI
    shinyjs::hide("login_ui")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
