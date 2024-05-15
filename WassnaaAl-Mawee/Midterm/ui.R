library(shiny)

# Load required data and packages
library(ggplot2)
library(dplyr)

# Use the `diamonds` data set provided by ggplot2
# Use dplyr's `sample_n()` function to get a random 1000 rows from the data set
# Store this sample in a variable `diamonds_sample`
diamonds_sample <- sample_n(diamonds, 1000)

# For convenience store the `range()` of values for the `price` column
# (of your sample)
price_range <- range(diamonds_sample$price)

# For convenience, get a vector of column names from the `diamonds` data set to
# use as select inputs
column_names <- colnames(diamonds)

# Define a UI using a `fluidPage()` layout with the following content:
ui <- fluidPage(
  
  # A `titlePanel` with the title "Diamond Viewer"
  titlePanel("Diamond Viewer"),
  
  # Your `price_input`
  sliderInput("price_choice", label = "Price (in dollars)", 
              min = price_range[1], max = price_range[2], 
              value = price_range),
  
  # Your `feature_input`
  selectInput("feature_choice", label = "Feature of Interest", 
              choices = column_names, selected = "carat"),
  
  # A `checkboxInput()` labeled "Show Trendline". It's default value is TRUE
  checkboxInput("show_trendline", label = "Show Trendline", value = TRUE),
  
  # A plotOutput showing the 'plot' output (based on the user specifications)
  plotOutput("plot")
)

