# ============================================================================
# Wrapper to the covmat functions
#
# Details:
# Calculates the Gaussian covariance matrix
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
gausian <- function(dmatrix, nugget, sill, range) {
  # Calculate the Gaussian covariance
  cov <- sill * exp(-(dmatrix / range)^2)
  #diag(cov) <- diag(cov) + nugget

  # Set values to 0 where distances exceed the range
  cov[dmatrix > range] <- 0

  return(cov)
}
