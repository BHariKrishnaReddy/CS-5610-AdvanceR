                                        #-----------    Problem 1   -----------#
set.seed(228)
library(ggplot2)
nu <- 4
N <- 10000

# t distribution up to a constant
t_pdf <- function(x, nu) {
  return((1 + (x^2)/nu)^(-(nu + 1)/2))
}

#Cauchy distribution
cauchy_pdf <- function(x) {
  return(dcauchy(x, 0, 1))
}

# sampling algorithm
rejection_sampling <- function(N, nu) {
  samples <- numeric(N)
  accepted <- 0
  
  while (accepted < N) {
    proposal <- rcauchy(1, 0, 1)
    u <- runif(1)
    
    if (u < t_pdf(proposal, nu)/cauchy_pdf(proposal)) {
      samples[accepted + 1] <- proposal
      accepted <- accepted + 1
    }
  }
  
  return(samples)
}

rejection_samples <- rejection_sampling(N, nu)
monte_carlo_estimate <- mean(t_pdf(rejection_samples, nu))

print(paste("Monte Carlo estimate of Ef(X) (Rejection Sampling):", monte_carlo_estimate))

# Metropolis algorithm
metropolis_algorithm <- function(N, nu) {
  samples <- numeric(N)
  current_sample <- 0
  
  for (i in 1:N) {
    proposal <- rnorm(1, current_sample, 1)
    alpha <- min(1, target_pdf(proposal, nu)/target_pdf(current_sample, nu))
    u <- runif(1)
    
    if (u < alpha) {
      current_sample <- proposal
    }
    
    samples[i] <- current_sample
  }
  
  return(samples)
}

metropolis_samples <- metropolis_algorithm(N, nu)

# Calculate percentiles
sample_percentiles <- quantile(metropolis_samples, probs = seq(0, 1, by = 0.1))
t_distribution_percentiles <- qt(seq(0, 1, by = 0.1), nu)

# Compare percentiles
print("Sample percentiles (Metropolis Algorithm):")
print(sample_percentiles)
print("t distribution percentiles:")
print(t_distribution_percentiles)

qqplot(t_distribution_percentiles, sample_percentiles, main = "Q-Q Plot (Metropolis Algorithm)", xlab = "t Distribution Percentiles", ylab = "Sample Percentiles")
abline(0, 1, col = "red")


                                        #-----------    Problem 2   -----------#
#seed is on top !
# pmf for the Sicherman dice
pmf <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)

# Function to perform the Metropolis
metropolis_sicherman <- function(n) {
  outcomes <- c(1, 3, 4, 5, 6, 8)
  sample <- numeric(n)
  counts <- rep(0, length(outcomes))
  current_state <- sample(outcomes, 1, prob = pmf)
  
  for (i in 1:n) {
    proposal <- sample(outcomes, 1, prob = pmf)
    
    # invalid proposals
    if (!(proposal %in% outcomes)) {
      warning("Invalid proposal:", proposal)
      next
    }
    
    alpha <- pmf[proposal] / pmf[current_state]
    
    # Accept or reject based on alpha
    if (is.na(alpha) || runif(1) < alpha) {
      current_state <- proposal
    }
    
    sample[i] <- current_state
    counts[current_state == outcomes] <- counts[current_state == outcomes] + 1
  }
  
  return(list(sample = sample, counts = counts / n))
}

result <- metropolis_sicherman(10000)
print("Relative frequencies:")
print(result$counts)
print("Theoretical probabilities:")
print(pmf)


                                        #-----------    Problem 3   -----------# 

### 2. The inverse transform method can always be used to generate discrete random variable when we know the probability mass function (pmf). TRUE
  # This is a technique used to generate random numbers from any probability distribution, including discrete distributions with known pmfs.
  #It works by inverting the CDF of the distribution.


### 6. When we use a Metropolis algorithm we can only use a symmetric proposal distribution. TRUE
  # In the Metropolis method, the proposal distribution must be symmetric, which means that proposing a move from one state to another has the same probability as proposing the opposite move.
  # This symmetry is necessary for the method to function properly.


### 7. When we use a Metropolis-Hastings algorithm to generate a random sample of 10,000, we need more than 10,000 iterations. FALSE
  # The number of iterations required for the Metropolis-Hastings algorithm is determined by several parameters, including the Markov chain's convergence rate and mixing qualities.
  # However, it is generally believed that the number of iterations necessary will not be much greater than the intended sample size.


### 8. If we wish to obtain independent samples from MCMC, we start to collect samples after the burn-in period. FALSE
  # In Markov Chain Monte Carlo methods, such as Metropolis-Hastings, the burn-in period is employed to allow the Markov chain to attain its equilibrium distribution.
  # Samples acquired during the burn-in period are often discarded because they may not precisely reflect the planned distribution.
  # Independent samples are collected after the burn-in period, when the Markov chain has converged to a stationary distribution.

