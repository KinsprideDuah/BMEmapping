# ============================================================================
# Wrapper to bme_predict function
#
# Details:
# Computes posterior summaries at estimation locations, including the posterior
# mean and its associated variance, posterior mode, and posterior median.
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
# - A data frame containing the estimation locations and their corresponding
#   BME posterior summaries, including the mean, variance, mode, and median
#   estimates.
#
# Example:
# - data("utsnowload")
# - x <- utsnowload[1:3, c("latitude", "longitude")]
# - ch <- utsnowload[6:67, c("latitude", "longitude")]
# - cs <- utsnowload[68:232, c("latitude", "longitude")]
# - zh <- utsnowload[6:67, c("hard")]
# - a <- utsnowload[68:232, c("lower")]
# - b <- utsnowload[68:232, c("upper")]
# - data_object <- bme_map(ch, cs, zh, a, b)
# - bme_predict(x, data_object, model = "exp", nugget = 0.0953,
#               sill = 0.3639, range = 1.0787, type = "mean")
# ============================================================================
bme_estimate <- function(x, data_object, model, nugget, sill, range,
                         nsmax = 5, nhmax = 5, n = 50,
                         zk_range = extended_range(data_object)) {
  # x <- matrix(c(x), ncol = 2)
  nk <- nrow(x)

  # set up container for estimates: mean, variance, mode
  df <- matrix(NA, ncol = 4, nrow = nk)

  for (i in 1:nk) {
    d <- prob_zk(
      x = x[i, ], data_object = data_object,
      model = model, nugget = nugget, sill = sill, range = range,
      nsmax = nsmax, nhmax = nhmax, n = n, zk_range = zk_range
    )

    delta <- d[2, 1] - d[1, 1]

    #--------------------------------------------------------------------------#
    # compute mean
    #--------------------------------------------------------------------------#
    zk_mean <- sum(d[, 1] * d[, 2] * delta)

    #--------------------------------------------------------------------------#
    # compute variance
    #--------------------------------------------------------------------------#
    zk_var <- sum((d[, 1] - zk_mean)^2 * d[, 2] * delta)

    #--------------------------------------------------------------------------#
    # compute mode
    #--------------------------------------------------------------------------#
    zk_mode <- d[which.max(d[, 2]), 1]

    #--------------------------------------------------------------------------#
    # compute median
    #--------------------------------------------------------------------------#
    # Normalize probabilities
    d$prob <- d$prob_zk_i / sum(d$prob_zk_i)

    # Compute Cd
    d$cd <- cumsum(d$prob)

    # Median via step function
    zk_median <- stats::approx(x = d$cd, y = d$zk_i, xout = 0.5,
                               ties = "ordered")$y

    #--------------------------------------------------------------------------#
    # gather estimates
    df[i, ] <- c(zk_mode, zk_mean, zk_median, zk_var)
    #--------------------------------------------------------------------------#
  }

  return(round(df, 4))
}




