set.seed(318)
theta <- 2
eta <- 0
n_iterations <- 50000
cauchy_density <- function(x, theta, eta){
  return(1/((theta*pi)*(1+(x-eta)^2)))
}
metropolis_cauchy <- function(n_iterations, theta, eta) {
  current_sample <- numeric(n_iterations)
  current_sample[1] <- rnorm(1, mean = eta, sd = 10)  
  
  for (i in 2:n_iterations) {
    proposal_sample <- rnorm(1, mean = current_sample[i - 1], sd = 10)
    acceptance_ratio <- cauchy_density(proposal_sample, theta, eta) / 
      cauchy_density(current_sample[i - 1], theta, eta)
    
    if (runif(1) < acceptance_ratio) {
      current_sample[i] <- proposal_sample
    } else {
      current_sample[i] <- current_sample[i - 1]
    }
  }
  
  return (current_sample)
}

sample_cauchy <- metropolis_cauchy(n_iterations, theta, eta)

hist(sample_cauchy, breaks = 100, main = "Histogram of Cauchy Sample", xlab = "Sample", ylab = "Frequency")

sample_percen <- quantile(sample_cauchy, probs = seq(0, 1, by = 0.01))
cauchy_percen <- qcauchy(seq(0, 1, by = 0.01), location = eta, scale = theta)
plot(cauchy_percen, sample_percen, 
     xlab = "Cauchy Distribution percentiles",
     ylab = "Sample percentiles",
     main = "Q-Q Plot: Cauchy Distribution vs Sample")
abline(0, 1, col = "blue")
legend("bottomright", legend = "Fit", col = "blue", lty = 1)