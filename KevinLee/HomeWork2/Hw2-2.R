#problem2
# Set the seed
set.seed(210)
n<- 1000
# Function to calculate the pdf of the given distribution
pdf <- function(x) {
  term1 <- (1 /2)*(1 / sqrt(2 * pi) * exp(-(x - 1)^2 / 2))
  term2 <- (1 /2)*(1 / sqrt(2 * pi) * exp(-(x - 3)^2 / 2))
  return(term1 + term2)
}

# Function to perform importance sampling
importance_sampling <- function(x) {
  dnorm(x, mean=2 ,sd=1)
}
  samples <- rnorm(n, mean = 2 ,sd=1)
  weights <- pdf(samples) / importance_sampling(samples)
importance_sampling_estimate = mean(samples*pdf(samples)/importance_sampling(samples))


# Number of samples for importance sampling


# Generate importance sampling weights
#weights <- importance_sampling(num_samples)

# Estimate Ef(X)
#estimated_mean <- sum(weights) / num_samples

# Draw a plot to show the convergence of the importance sampling approximation
plot(cumsum(samples*pdf(samples)/importance_sampling(samples))/(1:n), type ="l",xlab = "number of samples",ylab="importance sampling estimate")
abline(h=2,col="red")

