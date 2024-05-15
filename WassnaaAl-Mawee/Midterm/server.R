library(shiny)

# Load required data and packages
library(ggplot2)
library(dplyr)

# Define a server function (with appropriate arguments)
server <- function(input, output) {
  
  # Use the diamonds data set provided by ggplot2
  # Use dplyr's sample_n() function to get a random 1000 rows from the data set
  # Store this sample in a variable diamonds_sample
  diamonds_sample <- sample_n(diamonds, 1000)
  
  # For convenience store the range() of values for the price column
  # (of your sample)
  price_range <- range(diamonds_sample$price)
  
  # For convenience, get a vector of column names from the diamonds data set to
  # use as select inputs
  column_names <- colnames(diamonds)
  
  # Assign a reactive renderPlot() function to the outputted 'plot' value
  output$plot <- renderPlot({
    
    # This function should first filter down the diamonds_sample data
    # using the input price range (remember to get both ends)!
    filtered_data <- diamonds_sample %>%
      filter(price >= input$price_choice[1] & price <= input$price_choice[2])
    
    # Use the filtered data set to create a ggplot2 scatter plot with the
    # user-select column on the x-axis, and the price on the y-axis,
    # and encode the "cut" of each diamond using color
    p <- ggplot(filtered_data, aes_string(x = input$feature_choice, y = "price", color = "cut")) +
      geom_point() +
      labs(x = input$feature_choice, y = "Price") +
      theme_minimal()
    
    # Finally, if the "trendline" checkbox is selected, you should add (+)
    # a geom_smooth geometry (with se=FALSE) to your plot
    if (input$show_trendline) {
      p <- p + geom_smooth(se = FALSE)
    }
    
    # Be sure and return the completed plot!
    return(p)
  })
}