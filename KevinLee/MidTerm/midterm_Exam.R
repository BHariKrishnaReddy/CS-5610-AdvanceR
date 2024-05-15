#1.1

#Rejection Sampling
set.seed(228)
v <- 4

f_x <- function(x,v){
  (1+(x^2/v))^(-(v+1)/2)
}

y <- seq(-5, 5, 0.01)
plot(y, f_x(y,v), type ="l", xlab = "", ylab = "") 


y <- seq(-5, 5, 0.01)
plot(y, f_x(y,v), type ="l", xlab = "", ylab = "") 
lines(y, 4 *dcauchy(y,location = 0, scale = 1), col = "red")


y <- seq(-5, 5, 0.01)
plot(y, f_x(y,v)/dcauchy(y,location = 0, scale = 1), type ="l", xlab = "", ylab = "")
max(f_x(y,v)/dcauchy(y)) 
# maximum value came 3.596705 so we are using 4
n <- 10000
k <- 0 
j <- 0 
x <- rep(0, n)

while(k < n){
  y <- rcauchy(1,location = 0, scale = 1) # random variate from the standard cauchy
  u <- runif(1)
  j <- j + 1
  if (u < dt(y,v)/(4*dcauchy(y,location = 0, scale = 1))){
    # we accept y
    k <- k + 1
    x[k] <- y
  }
}

mean(x)
#Answer is -0.0004282286  which is very near to 0.



#1.2

#Metropolis algorithm 
set.seed(228)
f_x <- function(x,v){
  (1+(x^2/v))^(-(v+1)/2)
}

n <- 10000
x <- rep(0, n)
q <- function(x){
  rnorm(1, x, 1)
}

x[1] <- 2

# metropolis algorithm
for(i in 1:(n-1)){
  # pick new sample
  xnew <- q(x[i])
  # acceptance condition
  u <- runif(1)
  if (u < f_x(xnew,4)/f_x(x[i],4)){
    x[i+1] <- xnew
  }
  else{
    x[i+1] <- x[i]
  }
}
# draw the histogram with generated sample x
hist(x, prob = TRUE)
# add theoretical density curve to the histogram to check our sample follows the target distribution
y <- seq(-10, 10, 0.01)
lines(y, f_x(y,4), col = "red")

p <- seq(0.1, 0.9, 0.01)
Qhat <- quantile(x, p) # sample quantiles
Q <- qt(p, v) # theoretical quantiles
plot(Q, Qhat)
abline(a = 0, b = 1, col = "red")

round(rbind(Q, Qhat),2)

# find Monte Carlo estimator of E(X)
mean(x)

# 0.003882399 close to zero 




#2

set.seed(228)
S <- c(1,3,4,5,6,8)
pmf <- c(1/6, 1/6, 1/6, 1/6,1/6,1/6)
n <- 10000
x <- rep(0, n)

n_samples <- 6

# Create a proposal matrix
proposal_matrix <- matrix(0, nrow = n_samples, ncol = n_samples)

# Populate the proposal matrix
for (i in 1:n_samples) {
  proposal_matrix[i, i] <- 0.5
  remaining_prob <- 1 - proposal_matrix[i, i]
  other_prob <- remaining_prob / (n_samples - 1)
  proposal_matrix[i, -i] <- other_prob
}


x[1] <- 3

for(i in 1:(n-1)){
  xnew <- sample(S, 1, prob = proposal_matrix[which(S == x[i]), ])
  # acceptance condition
  u <- runif(1)
  if (u < (pmf[which(S == xnew)]*proposal_matrix[which(S == xnew),which(S == x[i])])/(pmf[which(S == x[i])]*proposal_matrix[which(S == x[i]),which(S == xnew)])){
    x[i+1] <- xnew
  }
  else{
    x[i+1] <- x[i]
  }
}
table(x)
table(x)/n


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