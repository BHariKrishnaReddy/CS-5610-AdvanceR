# in the given jobsubmission time col was not mentioned so i have edited the main jobsubmission and added "time" col


# Load the data set
jobsubmission <- read.table("/Users/harikrishnareddy/Desktop/R/KevinLee/Final/jobsubmission.txt", header = TRUE)

# Check the structure of the loaded data frame
str(jobsubmission)

# Check the first few rows of the data frame
head(jobsubmission)

# Calculate the MLE of λ
n <- length(jobsubmission$time)
cat("Value of n:", n, "\n")
cat("Sum of times:", sum(jobsubmission$time), "\n")
mle_lambda <- n / sum(jobsubmission$time)

cat("n:", n, "mle_lambda:", mle_lambda, "\n")



# Function to calculate the MLE of λ from a sample
mle_lambda_func <- function(data) {
  n <- length(data)
  return(n / sum(data))
}

# Bootstrap function
bootstrap_se <- function(data, func, B) {
  n <- length(data)
  bootstrap_estimates <- numeric(B)
  
  for (i in 1:B) {
    bootstrap_sample <- sample(data, n, replace = TRUE)
    bootstrap_estimates[i] <- func(bootstrap_sample)
  }
  
  return(sd(bootstrap_estimates))
}

# Estimate the standard error using bootstrap
set.seed(418)  # Set the seed for reproducibility
bootstrap_se_lambda <- bootstrap_se(jobsubmission$time, mle_lambda_func, 1000)
cat("Standard error of the MLE of λ (using bootstrap):", bootstrap_se_lambda, "\n")

# Calculate the bootstrap MLE of λ for each bootstrap sample
bootstrap_mle_lambda <- numeric(1000)
for (i in 1:1000) {
  bootstrap_sample <- sample(jobsubmission$time, n, replace = TRUE)
  bootstrap_mle_lambda[i] <- mle_lambda_func(bootstrap_sample)
}

# Calculate the percentile bootstrap confidence interval
ci_lower <- quantile(bootstrap_mle_lambda, probs = 0.025, na.rm = TRUE)
ci_upper <- quantile(bootstrap_mle_lambda, probs = 0.975, na.rm = TRUE)

cat("95% Confidence Interval for the MLE of λ (percentile bootstrap):\n")
cat("Lower bound:", ci_lower, "\n")
cat("Upper bound:", ci_upper, "\n")
