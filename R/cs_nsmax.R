
# ============================================================================
# Wrapper to prob_zk function
#
# Details:
# Compute the nsmax closest locations of the cs to the estimation location, x
#
# Inputs:
# - x vector of estimation location
# - cs matrix of soft data locations
# - nsmax number of soft data locations closer to the estimation location
#
# Outputs:
# - A two column matrix of locations closest to the estimation location
# ============================================================================
cs_nsmax <- function(x, cs, nsmax) {

  if (nsmax > nrow(cs)) {
    stop("Error: The nsmax must not exceed the number of soft-data locations.")
  }

  n <- nrow(cs)  # Number of rows in cs
  # Pre-allocate distance vector
  dist <- numeric(n)

  x <- matrix(c(x), ncol = 2)

  # Compute squared Euclidean distances directly
  for (i in 1:n) {
    dist[i] <- sqrt(sum((x - cs[i, ])^2))
  }

  # Sort the distances and get the indices of the smallest `nsmax`
  index <- order(dist)[1:nsmax]
  cs_new <- cs[index, , drop = FALSE]

  return(list(cs_new, index))
}
