# ============================================================================
# Wrapper to prob_zk function
#
# Details:
# Compute the nsmax closest locations of the cs to the estimation location, x
#
# Inputs:
# - x vector of estimation location
# - ch matrix of hard data locations
# - nhmax number of hard data locations closer to the estimation location
#
# Outputs:
# - A two column matrix of locations closest to the estimation location
# ============================================================================
ch_nhmax <- function(x, ch, nhmax) {

  if (nhmax > nrow(ch)) {
    stop("Error: The nhmax must not exceed the number of hard-data locations.")
  }

  n <- nrow(ch)  # Number of rows in cs
  # Pre-allocate distance vector
  dist <- numeric(n)

  x <- matrix(c(x), ncol = 2)

  # Compute squared Euclidean distances directly
  for (i in 1:n) {
    dist[i] <- sum((x - ch[i, ])^2)
  }

  # Sort the distances and get the indices of the smallest `nsmax`
  index <- order(dist)[1:nhmax]
  ch_new <- ch[index, , drop = FALSE]

  return(list(ch_new, index))
}
