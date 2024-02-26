set.seed(318)

# Function for Rayleigh density
rayleigh_density <- function(x, sigma) {
  (x / sigma^2) * exp(-x^2 / (2 * sigma^2))
}

# Metropolis-Hastings algorithm
metropolis_hastings <- function(n, sigma) {
  samples <- numeric(n)
  samples[1] <- 1
  
  for (i in 2:n) {
    proposal <- rgamma(1, shape = samples[i - 1], rate = 1)
    acceptance_prob <- min(1, rayleigh_density(proposal, sigma) / rayleigh_density(samples[i - 1], sigma))
    if (runif(1) < acceptance_prob) {
      samples[i] <- proposal
    } else {
      samples[i] <- samples[i - 1]
    }
  }
  return(samples)
}

sample_size <- 10000
sigma <- 2
sample <- metropolis_hastings(sample_size, sigma)

#histogram and theoretical density curve
hist(sample, freq = FALSE, col = rgb(0,0,1,0.5), main = "Rayleigh Distribution",
     xlab = "x", ylab = "Density")
curve(rayleigh_density(x, sigma), col = "red", lwd = 2, add = TRUE, 
      yaxt = "n", xaxt = "n")
legend("topright", legend = c("Sample Density", "Theoretical Density"), 
       col = c(rgb(0,0,1,0.5), "red"), lwd = c(10, 2), inset = 0.02)
