set.seed(318)
n <- 4
p <- 0.5
x_values <- 0:n
pmf <- dbinom(x_values, size = n, prob = p)
pmf_table <- data.frame(X = x_values, Probability = pmf)

n_iterations <- 50000

pmf_X <- function(x) {
  if (x >= 0 & x <= 4) {
    return(dbinom(x, size = 4, prob = 0.5))
  } else {
    return(0)
  }
}
current_state <- sample(0:4, 1)  
samples <- numeric(n_iterations)  

for (i in 1:n_iterations) {
  proposed_state <- current_state + sample(-1:1, 1)
  if (proposed_state < 0 | proposed_state > 4) {
    proposed_state <- current_state
  }
  
  acceptance_ratio <- pmf_X(proposed_state) / pmf_X(current_state)
  
  if (runif(1) < acceptance_ratio) {
    current_state <- proposed_state
  }
  
  samples[i] <- current_state
}

table(samples)
mce <- table(samples)/n_iterations
cat("Monte Carlo estimate of E(X):", mce, "\n")

mce_X_squared <- mean(samples^2)
mce_variance <- mce_X_squared - (mce)^2