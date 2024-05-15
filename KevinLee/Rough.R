# Set the seed for reproducibility
set.seed(418)

# Install the linprog package if not installed
if (!require("linprog")) {
  install.packages("linprog")
  library("linprog")
}

# Define the objective function coefficients
f.obj <- c(4, 2, 9)

# Define the constraint coefficients
f.con <- rbind(c(2, 1, 1),
               c(-1, 1, -3),
               c(-1, 0, 0),
               c(0, -1, 0),
               c(0, 0, -1))

# Define the constraint direction
f.dir <- c("<=", ">=", "<=", "<=", "<=")

# Define the constraint values
f.rhs <- c(2, -3, 0, 0, 0)

# Solve the Linear Programming problem using solveLP()
lp_result <- solveLP(c(4, 2, 9), rbind(c(2, 1, 1),
                                       c(-1, 1, -3),
                                       c(-1, 0, 0),
                                       c(0, -1, 0),
                                       c(0, 0, -1)), c("<=", ">=", "<=", "<=", "<="), c(2, -3, 0, 0, 0), maximum = TRUE)

# Print the solution
cat("Objective function value (maximum):", lp_result$opt, "\n")
cat("Decision variables:", lp_result$solution, "\n")

# Check if the solution satisfies the constraints
constraints_satisfied <- all(rbind(c(2, 1, 1),
                                   c(-1, 1, -3),
                                   c(-1, 0, 0),
                                   c(0, -1, 0),
                                   c(0, 0, -1)) %*% lp_result$solution <= c(2, -3, 0, 0, 0) * (c("<=", ">=", "<=", "<=", "<=") == "<=")) &
  all(rbind(c(2, 1, 1),
            c(-1, 1, -3),
            c(-1, 0, 0),
            c(0, -1, 0),
            c(0, 0, -1)) %*% lp_result$solution >= c(2, -3, 0, 0, 0) * (c("<=", ">=", "<=", "<=", "<=") == ">="))

cat("Constraints satisfied?", constraints_satisfied, "\n")