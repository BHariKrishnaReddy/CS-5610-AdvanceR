# Set the seed for reproducibility
set.seed(418)

# Problem 1: Load the data and draw a scatter plot
hospital_data <- read.table("/Users/harikrishnareddy/Desktop/R/KevinLee/Final/hospital.txt", header = TRUE)
plot(hospital_data$days, hospital_data$prog, xlab = "Days of Hospitalization", ylab = "Prognostic Index", main = "Scatter Plot of Hospital Data")

# Problem 2: Use Newton's method to find the least squares estimators
# Define the objective function Q
Q <- function(params, x, y) {
  b0 <- params[1]
  b1 <- params[2]
  sum((y - b0 * exp(b1 * x))^2)
}

# Define the gradient of Q
grad_Q <- function(params, x, y) {
  b0 <- params[1]
  b1 <- params[2]
  n <- length(y)
  res <- y - b0 * exp(b1 * x)
  c(sum(-2 * res * exp(b1 * x)), sum(-2 * b0 * res * x * exp(b1 * x)))
}

# Define the Hessian of Q
hess_Q <- function(params, x, y) {
  b0 <- params[1]
  b1 <- params[2]
  n <- length(y)
  res <- y - b0 * exp(b1 * x)
  H11 <- sum(-2 * exp(2 * b1 * x))
  H12 <- sum(-2 * x * exp(2 * b1 * x))
  H21 <- H12
  H22 <- sum(-2 * b0 * x^2 * exp(2 * b1 * x))
  matrix(c(H11, H12, H21, H22), nrow = 2)
}

# Newton's method
newton_method <- function(x, y, init_params, tol = 1e-6, max_iter = 100) {
  params <- init_params
  iter <- 0
  while (iter < max_iter) {
    grad <- grad_Q(params, x, y)
    hess <- hess_Q(params, x, y)
    update <- -solve(hess, grad)
    params <- params + update
    
    # Check for convergence
    if (all(!is.na(update)) && all(abs(update) <= tol)) {
      break
    }
    iter <- iter + 1
  }
  params
}


# Apply Newton's method
x <- hospital_data$days
y <- hospital_data$prog
init_params <- c(50, 0)
newton_estimates <- newton_method(x, y, init_params)
cat("Newton's method estimates:\n")
print(newton_estimates)

# Problem 3: Use optim() function
obj_func <- function(params, x, y) {
  b0 <- params[1]
  b1 <- params[2]
  sum((y - b0 * exp(b1 * x))^2)
}

optim_estimates <- optim(par = init_params, fn = obj_func, x = x, y = y)
cat("\noptim() estimates:\n")
print(optim_estimates$par)

# Problem 4: Compare with nls() output
nls_fit <- nls(prog ~ beta0 * exp(beta1 * days), data = hospital_data, start = list(beta0 = 50, beta1 = 0))
cat("\nnls() estimates:\n")
print(coef(nls_fit))