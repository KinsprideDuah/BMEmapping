# ============================================================================
# Wrapper to q_bme_cv function
#
# Details:
# Performs leave-one-out cross validation (LOOCV) at hard data locations.
#
# Inputs:
# -  data object (zh, cs, zh, a, b)
# -  a vector of lower bounds of soft data
# -  b vector of lower bounds of soft data
# -  nq a positive integer indicating the number of quantile levels
# -  nsmax number of soft data locations closer to the estimation location
# -  nhmax number of hard data locations closer to the estimation location
# -  zk_range a numeric vector specifying the range over which to evaluate the
#    unobserved value at the estimation location
# -  type a character string specifying the type of BME prediction to compute.
# -  k an integer specifying the number of folds (or partitions) into which the
#    hard data are divided (here, k = number of hard data locations).
#
# Outputs:
# -  A data frame containing the coordinates of the hard data locations, the
#    observed values, the corresponding BME predictions, prediction residuals,
#    and the cross-validation fold indices.
#
# Example:
# - data("utsnowload")
# - ch <- utsnowload[2:10, c("latitude", "longitude")]
# - cs <- utsnowload[68:232, c("latitude", "longitude")]
# - zh <- utsnowload[2:10, c("hard")]
# - a <- utsnowload[68:232, c("lower")]
# - b <- utsnowload[68:232, c("upper")]
# - data_object <- bme_map(ch, cs, zh, a, b)
# - q_bme_loocv(x, data_object, type = "mean", k = 9)
# ============================================================================
q_bme_loocv <- function(data_object, nsmax = 5, nhmax = 5, n = 50, nq = 3,
                        zk_range = extended_range(data_object),
                        type) {

  type <- match.arg(type, choices = c("mean", "mode", "median"))

  col_idx <- if (type == "mode") {
    1
  } else if (type == "mean") {
    c(2, 4)
  } else {
    3
  }

  col_names <- if (type == "mode") {
    "mode"
  } else if (type == "mean") {
    c("mean", "variance")
  } else {
    "median"
  }

  ch <- as.data.frame(data_object$ch)
  cs <- as.data.frame(data_object$cs)
  zh <- data_object$zh
  a <- data_object$a
  b <- data_object$b

  nh <- nrow(ch)
  est <- matrix(NA, nrow = nh, ncol = length(col_idx))

  for (i in seq_len(nh)) {

    data_obj <- bme_map(
      ch = ch[-i, ],
      cs = cs,
      zh = zh[-i],
      a = a,
      b = b
    )

    est[i, ] <- q_bme_estimate(
      x = ch[i, ],
      data_object = data_obj,
      nq = nq,
      nsmax = nsmax,
      nhmax = nhmax,
      n = n,
      zk_range = zk_range
    )[, col_idx]
  }

  ch_names <- if (is.null(colnames(ch))) c("coord.1", "coord.2") else colnames(ch)

  result <- cbind.data.frame(
    ch,
    observed = zh,
    est,
    residual = round(zh - est[, 1], 4),
    fold = seq_len(nh)
  )

  names(result) <- c(ch_names, "observed", col_names, "residual", "fold")

  structure(result, class = c("BMEmapping", "data.frame"))
}
