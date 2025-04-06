options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages("invgamma")
library(invgamma)

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

rand_env_custom <- function(L, n) {
  # Initialize an empty data frame to hold lattice points and their values
  env_points <- data.frame(i = numeric(), j = numeric(), value = numeric())
  
  # Loop over all possible sums (s = i+j). We start at 2 since i, j >= 1.
  for (s in 0:n) {
    # Loop over all allowed differences (d = j-i)
    for (d in 0:L) {
      # Compute i and j from s and d
      i_val <- (s - d) / 2
      j_val <- (s + d) / 2
      
      if (is.wholenumber(i_val) && is.wholenumber(j_val)) {
        val <- rinvgamma(1, shape = 1, rate = 1)
        env_points <- rbind(env_points, data.frame(i = i_val, j = j_val, value = val))
      }
    }
  }
  
  return(env_points)
}

# Example usage:
# Create an environment where j - i is between 1 and 3,
# and i + j is between 2 and 10.
set.seed(123)  # for reproducibility
env <- rand_env_custom(L = 50, n = 10)
print(env)