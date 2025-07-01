# ============================================================================
# Wrapper to the covmat functions
#
# Details:
# Calculates the Spherical covariance matrix
#
# Inputs:
# - dmatrix a distance matrix, having real non-negative
# - nugget a non-negative value
# - sill a non-negative value
# - range a non-negative value
#
# Outputs:
# - A matrix with same size as dmatrix
# ============================================================================
spherical <- function(dmatrix, nugget, sill, range) {
  # Calculate the spherical covariance
  cov <- sill * (1 - ((3 / 2) * (dmatrix / range) - (1 / 2) *
                        (dmatrix / range)^3))
  #diag(cov) <- diag(cov) + nugget

  return(cov)
}
