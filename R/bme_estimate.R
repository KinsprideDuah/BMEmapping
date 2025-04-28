# ============================================================================
# Wrapper to bme_mean and bme_mode functions
#
# Details:
# Compute the bme mean, variance and mode of an estimation location
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
# - A data frame of estimation locations with their corresponding bme mean,
#   variance and mode estimates
# ============================================================================
bme_estimate <- function(x, ch, cs, zh, a, b, model, nugget, sill, range,
                         nsmax, nhmax) {

  x <- matrix(c(x), ncol = 2)
  nk <- nrow(x)

  # set up container for estimates: mean, variance, mode
  df <- matrix(NA, ncol = 3, nrow = nk)

  for (i in 1:nk) {
    d <- prob_zk(x[i, ], ch, cs, zh, a, b, model, nugget, sill, range,
                 nsmax, nhmax)

    delta <- d[2, 1] - d[1, 1]

    # compute mean
    zk_mean <- sum(d[, 1] * d[, 2] * delta)

    # compute variance
    zk_var <- sum((d[, 1] - zk_mean)^2 * d[, 2] * delta)

    # compute mode
    zk_mode <- d[which.max(d[, 2]), 1]

    # gather estimates
    df[i, ] <- c(zk_mode, zk_mean, zk_var)
  }

  return(df)
}
