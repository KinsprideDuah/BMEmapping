# ============================================================================
# Wrapper to bme_cv functions
#
# Details:
# Cross validation function to compute the BME mode , mean and variance of
# estimation location(s).
#
# Inputs:
# -  x matrix of estimation locations
# -  ch matrix of hard data locations
# -  cs matrix of soft data locations
# -  zh vector of hard data
# -  a vector of lower bounds of soft data
# -  b vector of lower bounds of soft data
# -  model string name of covariance or variogram model
# -  nugget a non-negative value
# -  sill a non-negative value
# -  range a non-negative value
# -  nsmax number of soft data locations closer to the estimation location
# -  nhmax number of hard data locations closer to the estimation location
#
# Outputs:
# - A data frame of estimation locations with their corresponding bme mode
#   estimates
# ============================================================================
bme_mean_loocv <- function(ch, cs, zh, a, b, model, nugget, sill, range,
                           nsmax, nhmax) {
  n <- nrow(ch)
  d <- matrix(NA, nrow = n, ncol = 2)

  for (i in 1:n) {
    d[i, ] <- bme_estimate(x = ch[i, ], ch = ch[-i,], cs, zh = zh[-i], a, b,
                           model, nugget, sill, range,
                           nsmax, nhmax)[, c(2, 3)]
  }

  df <- cbind.data.frame(ch, zh, d, zh - d[ ,1], 1:n)
  names(df) <- c("coord.1", "coord.2", "observed", "mean", "variance",
                 "residual", "fold")

  return(df)
}
