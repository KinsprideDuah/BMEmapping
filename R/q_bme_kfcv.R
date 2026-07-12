# ============================================================================
# Wrapper to bme_cv function
#
# Details:
# Performs K-fold cross validation (KFCV) at hard data locations.
#
# Inputs:
# -  data object (zh, cs, zh, a, b)
# -  a vector of lower bounds of soft data
# -  b vector of lower bounds of soft data
# -  model string name of covariance or variogram model
# -  nugget a non-negative value
# -  sill a non-negative value
# -  range a non-negative value
# -  nsmax number of soft data locations closer to the estimation location
# -  nhmax number of hard data locations closer to the estimation location
# -  zk_range a numeric vector specifying the range over which to evaluate the
#    unobserved value at the estimation location
# -  type a character string specifying the type of BME prediction to compute.
# -  k an integer specifying the number of folds (or partitions) into which the
#    hard data are divided.
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
# - q_bme_kfcv(x, data_object, model = "exp", nugget = 0.0953,
#            sill = 0.3639, range = 1.0787, type = "mean", k = 4)
# ============================================================================
q_bme_kfcv <- function(data_object, nsmax = 5, nhmax = 5, n = 50, nq =3,
                       zk_range = extended_range(data_object),
                       type = c("mean", "mode", "median"),
                       k) {

  type <- match.arg(type)

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


  if (k > nh) {
    stop("Number of folds cannot exceed the number of hard data points.")
  }

  set.seed(123)
  fold_id <- sample(rep(1:k, length.out = nh))
  folds <- split(seq_len(nh), fold_id)

  est <- matrix(NA, nrow = nh, ncol = length(col_idx))
  fold_results <- numeric(nh)

  # Cross-validation loop
  for (i in seq_along(folds)) {

    test_idx <- folds[[i]]
    train_idx <- setdiff(seq_len(nh), test_idx)

    data_obj <- bme_map(
      ch = ch[train_idx, ],
      cs = cs,
      zh = zh[train_idx],
      a = a,
      b = b
    )

    pred <- q_bme_estimate(
      x = ch[test_idx, ],
      data_object = data_obj,
      nq = nq,
      nsmax = nsmax,
      nhmax = nhmax,
      n = n,
      zk_range = zk_range
    )[, col_idx, drop = FALSE]

    est[test_idx, ] <- pred
    fold_results[test_idx] <- i
  }

  ch_names <- if (is.null(colnames(ch))) {
    c("coord.1", "coord.2")
  } else {
    colnames(ch)
  }

  result <- cbind.data.frame(
    ch,
    observed = zh,
    est,
    residual = round(zh - est[, 1], 4),
    fold = fold_results
  )

  names(result) <- c(
    ch_names,
    "observed",
    col_names,
    "residual",
    "fold"
  )

  structure(result, class = c("BMEmapping", "data.frame"))
}
