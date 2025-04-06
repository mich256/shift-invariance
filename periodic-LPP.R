# 1. Dynamic Programming Approach for Numeric Grids
# -------------------------------------------------
# Computes the last passage percolation (LPP) value on a grid.
# X is a numeric matrix of weights with dimensions (rows x cols)
LPP_grid_value <- function(X) {
  m <- nrow(X)
  n <- ncol(X)
  L <- matrix(0, m, n)
  L[1, 1] <- X[1, 1]
  
  # Fill in the first row (only right moves available)
  for (j in 2:n) {
    L[1, j] <- L[1, j - 1] + X[1, j]
  }
  
  # Fill in the first column (only down moves available)
  for (i in 2:m) {
    L[i, 1] <- L[i - 1, 1] + X[i, 1]
  }
  
  # Fill in the rest using the recurrence:
  # L(i, j) = X[i,j] + max(L(i-1, j), L(i, j-1))
  for (i in 2:m) {
    for (j in 2:n) {
      L[i, j] <- X[i, j] + max(L[i - 1, j], L[i, j - 1])
    }
  }
  
  return(L[m, n])
}

# 2. Symbolic Expression Generation (for small grids)
# ----------------------------------------------------
# These functions generate a string expression for each up/right path
# from the top-left (cell (1,1)) to the bottom-right (cell (rows, cols))
# where each path sum is expressed as a sum of terms like "x_{i,j}".

# (a) all_paths: Generate all sequences of moves.
#     We assume allowed moves are "R" (right) and "D" (down).
all_paths <- function(rows, cols) {
  n <- rows + cols - 2  # total number of moves
  # In any valid path, exactly (cols - 1) moves are to the right.
  combn_indices <- combn(n, cols - 1)
  path_list <- list()
  for (k in 1:ncol(combn_indices)) {
    move_seq <- rep("D", n)
    move_seq[combn_indices[, k]] <- "R"
    path_list[[k]] <- move_seq
  }
  return(path_list)
}

# (b) get_coords: Given a sequence of moves, return the coordinates of each cell.
#     We start at (1,1); "R" increases the column index and "D" increases the row index.
get_coords <- function(moves) {
  n <- length(moves)
  coords <- matrix(NA, nrow = n + 1, ncol = 2)
  coords[1, ] <- c(1, 1)
  i <- 1
  j <- 1
  for (m in 1:n) {
    if (moves[m] == "R") {
      j <- j + 1
    } else if (moves[m] == "D") {
      i <- i + 1
    }
    coords[m + 1, ] <- c(i, j)
  }
  return(coords)
}

# (c) LPP_grid_expressions: Create an expression (as a string) for each path.
#     Each expression is a sum of the symbols x_{i,j} along that path.
#     max_paths is a safeguard to avoid enumerating too many paths.
LPP_grid_expressions <- function(rows, cols, max_paths = 1000) {
  paths <- all_paths(rows, cols)
  if (length(paths) > max_paths) {
    stop("Too many paths to enumerate for these grid dimensions.")
  }
  expr_list <- sapply(paths, function(moves) {
    coords <- get_coords(moves)
    terms <- apply(coords, 1, function(coord) {
      paste0("x_{", coord[1], ",", coord[2], "}")
    })
    paste(terms, collapse = " + ")
  })
  return(expr_list)
}

# (d) LPP_grid_max_expression: Combine all the path expressions into one
#     expression representing the maximum over all path sums.
LPP_grid_max_expression <- function(rows, cols, max_paths = 1000) {
  expr_list <- LPP_grid_expressions(rows, cols, max_paths)
  max_expr <- paste("max(", paste(expr_list, collapse = ", "), ")")
  return(max_expr)
}

# Example usage:
# --------------------
# For a 2x4 grid, the original case, you can see the symbolic max expression:
# cat("Symbolic LPP Expression for a 2x4 Grid:\n")
# cat(LPP_grid_max_expression(2, 4), "\n")

# For a numeric example, generate a random 3x5 grid and compute its LPP value:
# set.seed(123)
# X <- matrix(rexp(15, rate = 1), nrow = 3, ncol = 5)
# cat("\nNumeric Last Passage Time for a 3x5 Grid:", LPP_grid_value(X), "\n")