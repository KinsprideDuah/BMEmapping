# ============================================================================
# Wrapper to the prob_zk function
#
# Details:
# Compute the covariance between two sets of coordinates
#
# Inputs:
# -  c1 n1 by d matrix of coordinates for the locations in the first set. A line
#    corresponds to the vector of coordinates at a location, so the number of
#    columns is equal to the dimension of the space. There is no restriction on
#    the dimension of the space.
# -  c2 n2 by d matrix of coordinates for the locations in the second set,
#    using the same conventions as for c1.
# -  model string name of covariance or variogram model
# -  nugget a non-negative value
# -  sill a non-negative value
# -  range a non-negative value
# -  dmax a non-negative value
#
# Outputs:
# - A covariance matrix with same size as D
# ============================================================================
covmat <- function(c1, c2, model, nugget, sill, range) {
  if (nugget < 0) {
    stop("Error: nugget cannot be negative. Execution stopped.")
  }

  if (sill <= 0 || range <= 0) {
    stop("Error: Neither the sill nor range can be negative. Execution stopped.")
  }

  d <- distant(c1, c2)

  if (model == "sph") {
    k <- spherical(d, nugget, sill, range)
  } else if (model == "exp") {
    k <- exponential(d, nugget, sill, range)
  } else if (model == "gau") {
    k <- gausian(d, nugget, sill, range)
  } else {
    k <- c("Error: The variogram/covariance model is incorrect. Execution stopped.")
  }

  #k[d > dmax] <- 0

  return(round(k, 6))
}
