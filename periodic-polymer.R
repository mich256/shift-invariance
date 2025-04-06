# Make sure the invgamma package is available.
if(!require(invgamma)) {
  install.packages("invgamma")
  library(invgamma)
}

# Function to create the random environment
# L: maximum allowed difference (j - i) (allowed values: 1,...,L)
# n: maximum allowed sum (i + j) (allowed values: 1,...,n)
# Note: For positive i, j the smallest possible sum is 2.
rand_env_custom <- function(L, n) {
  # Initialize an empty data frame to hold lattice points and their values
  env_points <- data.frame(i = numeric(), j = numeric(), value = numeric())
  
  # Loop over all possible sums (s = i+j). We start at 2 since i, j >= 1.
  for (s in 2:n) {
    # Loop over all allowed differences (d = j-i)
    for (d in 1:L) {
      # Compute i and j from s and d
      i_val <- (s - d) / 2
      j_val <- (s + d) / 2
      
      # Only include the point if both i and j are positive integers
      if (i_val == floor(i_val) && j_val == floor(j_val) && i_val >= 1 && j_val >= 1) {
        # Draw one inverse gamma random variable with shape = 1 and rate = 1
        val <- rinvgamma(1, shape = 1, rate = 1)
        # Append this point to the data frame
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
env <- rand_env_custom(L = 3, n = 10)
print(env)