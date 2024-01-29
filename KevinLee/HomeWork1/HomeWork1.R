# libraries
library(ggplot2)

# Pareto distribution parameters
a <- 3
b <- 2

# 1. Use the inverse transform method to generate a random sample of size 1000
set.seed(123)
pareto_sample <- b / (runif(1000))^(1/a)

# 2. Plot the density histogram of the sample
hist_plot <- ggplot(data.frame(x = pareto_sample), aes(x)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Density Histogram of Pareto(3,2) Random Sample",
       x = "Value", y = "Frequency")

# 3. Add the theoretical density curve f(x) to the density histogram
density_curve <- function(x) (a * b^a) / x^(a + 1)
hist_plot + stat_function(fun = density_curve, color = "red", size = 1)

# 4. Write your own rpareto function
rpareto <- function(n, shape, scale) {
  scale / (runif(n))^(1/shape)
}

# 5. Use your rpareto function to generate a random sample of size 1000 from Pareto(2,1)
set.seed(456)  # Set seed for repro

