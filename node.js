// Function to generate an exponential random variable with given rate lambda.
function rexp(lambda) {
  let u = Math.random();
  if (u === 0) u = 1e-10; // Guard against log(0)
  return -Math.log(u) / lambda;
}

// Function to simulate LPP on an m x n grid with exponential weights.
function simulateLPP(m, n, lambda) {
  // Initialize two 2D arrays: one for the grid and one for the dynamic programming table L.
  const grid = Array.from({ length: m }, () => new Array(n));
  const L = Array.from({ length: m }, () => new Array(n));

  // Generate the grid with exponential random weights.
  for (let i = 0; i < m; i++) {
    for (let j = 0; j < n; j++) {
      grid[i][j] = rexp(lambda);
    }
  }

  // Compute the LPP value using dynamic programming.
  // Start at the top-left corner.
  L[0][0] = grid[0][0];

  // Fill in the first row (only right moves available).
  for (let j = 1; j < n; j++) {
    L[0][j] = L[0][j - 1] + grid[0][j];
  }

  // Fill in the first column (only down moves available).
  for (let i = 1; i < m; i++) {
    L[i][0] = L[i - 1][0] + grid[i][0];
  }

  // Fill in the rest of the grid.
  for (let i = 1; i < m; i++) {
    for (let j = 1; j < n; j++) {
      L[i][j] = grid[i][j] + Math.max(L[i - 1][j], L[i][j - 1]);
    }
  }

  // Return the LPP value at the bottom-right corner.
  return L[m - 1][n - 1];
}

// Example usage:
const m = 10;       // Number of rows
const n = 10;       // Number of columns
const lambda = 1;   // Rate for the exponential distribution
const lppValue = simulateLPP(m, n, lambda);

console.log(`LPP value for a ${m}x${n} grid: ${lppValue}`);