# ============================================================================
# Wrapper to the covmat functions
#
# Details:
# Calculates the Exponential covariance matrix
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
exponential <- function(dmatrix, nugget, sill, range) {
  # Calculate the exponential covariance
  cov <- sill * exp(-3 * dmatrix / range)
  diag(cov) <- diag(cov) + nugget

  return(cov)
}
