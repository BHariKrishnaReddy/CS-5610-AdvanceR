# Setting the seed #
set.seed(418)

# Applying the Simplex Method
library(linprog)

# The Objective function coefficients, c 
c <- c(4, 2, 9)

# The linear constraints coefficients, A and b 
A <- rbind(c(2, 1, 1), c(1, -1, 3))
b <- c(2, 3)

# Solving the linear programming problem
solution <- solveLP(c, b, A, const.dir = rep("<=", length(b)), maximum = TRUE, lpSolve = TRUE)

# Printing the solution
print(solution)