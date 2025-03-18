# ============================================================================
# Wrapper to the covariance model functions
#
# Details:
# Calculates the distance matrix between two sets of locations
#
# Inputs:
# - c1 is a two-column matrix of coordinates (longitude/latitude) for the
#   locations in the first set.
# - c2 is a two-column matrix of coordinates (longitude/latitude) for the
#   locations in the second set.
#
# Outputs:
# - A distance matrix between two sets of locations
# ============================================================================
distant <- function(c1, c2) {
  if (length(c1) == 2) {
    c1 <- matrix(c1, ncol = 2)
  } else {
    c1 <- c1
  }

  if (length(c2) == 2) {
    c2 <- matrix(c2, ncol = 2)
  } else {
    c2 <- c2
  }

  n1 <- nrow(c1) # Number of rows in c1
  n2 <- nrow(c2) # Number of rows in c2

  unit1 <- matrix(1, n1, 1) # Column vector of ones with n1 rows
  unit2 <- matrix(1, n2, 1) # Column vector of ones with n2 rows

  # Calculate the Kronecker product
  d <- kronecker(unit2, c1) - kronecker(c2, unit1)

  # Conditional to handle the size of D
  if (ncol(d) == 1) {
    d <- abs(d)
  } else {
    d <- sqrt(rowSums(d^2)) # Equivalent to sqrt(sum((D.^2)')')
  }

  d <- matrix(d, nrow = n1, ncol = n2) # Reshape D to n1 x n2

  return(round(d, 4))
}
