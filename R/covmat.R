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

  # Input checks
  if (nugget < 0) {
    stop("`nugget` must be non-negative.")
  }
  if (sill <= 0) {
    stop("`sill` must be positive.")
  }
  if (range <= 0) {
    stop("`range` must be positive.")
  }

  # Compute pairwise distances
  d <- distant(c1, c2)

  # Select model
  model <- tolower(model)
  k <- switch(model,
              sph = spherical(d, nugget, sill, range),
              exp = exponential(d, nugget, sill, range),
              gau = gausian(d, nugget, sill, range),
              stop("Invalid model type. Choose from 'sph', 'exp', or 'gau'."))

  return(round(k, 6))
}
