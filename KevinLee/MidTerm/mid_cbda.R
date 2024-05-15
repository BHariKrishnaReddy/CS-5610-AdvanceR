set.seed(228)

#Problem 1
#1)

nu <- 4
n <- 10000

f <- function(x) {
  return((1 + x^2/nu)^(-0.5*(nu+1)))
}

g <- function(x) {
  return(dcauchy(x, location = 0, scale = 1))
}

y <- seq(-5, 5, 0.01)
max(f(y)/g(y))
c <- 3.6

k <- 0
j <- 0
x <- rep(0, n)

while(k < n){
  y <- rcauchy(1, location = 0, scale = 1)
  u <- runif(1)
  j <- j + 1
  if (u < f(y)/(c * g(y))){
    k <- k + 1
    x[k] <- y
  }
}

mc_estimate <- mean((x))

cat("Monte Carlo Estimate of E[(X)]:", mc_estimate, "\n")

#2)

nu <- 4
target_distribution <- function(x) {
  return((1 + x^2/nu)^(-0.5*(nu+1)))
}

q <- function(x) {
  rnorm(1, x, 1)
}

n <- 10000
x <- rep(0, n)

x[1] <- 2

for (i in 1:(n - 1)) {
  xnew <- q(x[i])
  u <- runif(1)
  acceptance_ratio <- target_distribution(xnew) / target_distribution(x[i])
  if (u < acceptance_ratio) {
    x[i + 1] <- xnew
  } else {
    x[i + 1] <- x[i]
  }
}

sample_percentiles <- quantile(x, seq(0, 1, 0.01))

# Compute t distribution percentiles
t_distribution_percentiles <- qt(seq(0, 1, 0.01), df = nu)

# Q-Q plot
plot(t_distribution_percentiles, sample_percentiles, 
     main = "Q-Q Plot",
     xlab = "t Distribution Percentiles",
     ylab = "Sample Percentiles")

# Add a 45-degree reference line
abline(a = 0, b = 1, col = "green")

#Problem 2
#1)

S <- c(1, 3, 4, 5, 6, 8)
pmf <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
n <- 10000
x <- rep(0, n)

Q <- matrix(c(0.4, 0.2, 0.2, 0.2, 0.2 ,0.2, 0.2, 0.4, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.4, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.4, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.4, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.4), nrow = 6, ncol = 6, byrow = TRUE)

x[1] <- sample(S, 1)

for (i in 1:(n-1)) {
  xnew <- sample(S, 1, prob = Q[which(S == x[i]), ])
  u <- runif(1)
  if (u < pmf[which(S == xnew)] / pmf[which(S == x[i])]) {
    x[i + 1] <- xnew
  } else {
    x[i + 1] <- x[i]
  }
}


table_simulation <- table(x)
relative_frequency_simulation <- table_simulation / n

print("Simulation Results:")
print(table_simulation)
print(relative_frequency_simulation)

print("Theoretical Probabilities:")
print(pmf)



#Problem 3
#1   TRUE
#2.	TRUE
#3.	FALSE
#4.	TRUE
#5.	FALSE
#6.	TRUE
#7.	FALSE
#8.	FALSE