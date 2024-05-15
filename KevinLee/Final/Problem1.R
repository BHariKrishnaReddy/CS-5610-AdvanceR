# Set the seed
set.seed(418)

# Set the parameter values
n <- 16
alpha <- 2
beta <- 4

# Initialize the first sample
x <- 5
y <- 0

# Create vectors to store the generated samples
x_samples <- numeric(10000)
y_samples <- numeric(10000)

# Gibbs sampling
for (i in 1:10000) {
  y <- rbeta(1, x + alpha, n - x + beta)
  x <- rbinom(1, n, y)
  x_samples[i] <- x
  y_samples[i] <- y
}

# Estimating the mean of the marginal distribution f(x) of X
sample_mean_x <- mean(x_samples)
sample_mean_x

# Comparing the sample mean with the theoretical mean
theoretical_mean <- (alpha * n) / (alpha + beta)
theoretical_mean

# Printing the results
cat("Sample mean of X:", sample_mean_x, "\n")
cat("Theoretical mean of X:", theoretical_mean, "\n")