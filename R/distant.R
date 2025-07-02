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
  d_x <- outer(c1[, 1], c2[, 1], "-")
  d_y <- outer(c1[, 2], c2[, 2], "-")
  d <- sqrt(d_x^2 + d_y^2)

  return(round(d, 4))
}
