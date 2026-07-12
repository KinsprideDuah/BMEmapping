#' @title Leave-one-out cross validation (LOOCV) at hard data locations.
#'
#' @usage bme_cv(data_object, model, nugget, sill, range,
#'               nsmax = 5, nhmax = 5, n = 50,
#'               zk_range = extended_range(data_object), type,
#'               k = 5)
#'
#' @param data_object A list containing the hard and soft data.
#' @param model A string specifying the variogram or covariance model to use
#'        (e.g., \code{"exp"}, \code{"sph"}, etc.).
#' @param nugget A non-negative numeric value for the nugget effect in the
#'        variogram model.
#' @param sill A numeric value representing the sill (total variance) in the
#'        variogram model.
#' @param range A positive numeric value for the range (or effective range)
#'        parameter of the variogram model.
#' @param nsmax A positive numeric value specifying the maximum number of nearby
#'        soft data points to include for estimation (default is 5).
#' @param nhmax A positive numeric value specifying the maximum number of nearby
#'        hard data points to include for estimation (default is 5).
#' @param n A positive numeric value indicating the number of points at which to
#'        evaluate the posterior density over \code{zk_range} (default is 50).
#' @param zk_range A numeric vector specifying the range over which to evaluate
#'        the unobserved value at the estimation location (\code{zk}). Although
#'        \code{zk} is unknown,  it is assumed to lie within a range similar to
#'        the observed data (\code{zh}, \code{a}, and \code{b}). It is advisable
#'        to explore the posterior distribution at a few locations using
#'        \code{prob_zk()} before finalizing this range.
#' @param type A string indicating the type of BME prediction to compute: either
#'        \code{"mean"} for the posterior mean, \code{"mode"} for the
#'        posterior mode or \code{"median"} for the posterior median.
#' @param k A positive numeric value specifying the number of folds
#'        (or partitions) into which the hard data are divided (default is 5).
#'
#' @returns A data frame containing the coordinates of the hard data locations,
#'          the observed values, the corresponding BME predictions (posterior
#'          \code{mean}, \code{mode}, or \code{median}, depending on
#'          \code{type}), the posterior variance (when
#'          \code{type = "mean"}), the prediction residuals, and the
#'          cross-validation fold indices.
#'
#' @description
#' \code{bme_cv} performs cross-validation to evaluate the predictive
#' performance of the Bayesian Maximum Entropy (BME) spatial interpolation
#' method using both hard and soft (interval) data. The function supports both
#' leave-one-out cross-validation (LOOCV) and K-fold cross-validation,
#' depending on the value of \code{k}. If \code{k} equals the number of hard
#' data locations, LOOCV is performed by removing one hard observation at a
#' time and predicting it using the remaining hard and all soft data. If
#' \code{k} is less than the number of hard data locations, the hard data are
#' randomly partitioned into \code{k} folds, with each fold used once as the
#' validation set while the remaining folds are used for prediction. Depending
#' on the \code{type} argument, predictions are returned as posterior means,
#' posterior modes, or posterior medians.
#'
#' This function is useful for validating the BME interpolation method and
#' tuning variogram parameters.
#'
#' @examples
#' data("utsnowload")
#' ch <- utsnowload[2:10, c("latitude", "longitude")]
#' cs <- utsnowload[68:232, c("latitude", "longitude")]
#' zh <- utsnowload[2:10, c("hard")]
#' a <- utsnowload[68:232, c("lower")]
#' b <- utsnowload[68:232, c("upper")]
#' data_object <- bme_map(ch, cs, zh, a, b)
#' bme_cv(data_object,
#'   model = "exp", nugget = 0.0953, sill = 0.3639,
#'   range = 1.0787, type = "mean", k = 5
#' )
#'
#' @export
bme_cv <- function(data_object, model, nugget, sill, range,
                   nsmax = 5, nhmax = 5, n = 50,
                   zk_range = extended_range(data_object),
                   type, k = 5) {

  # Number of hard data locations
  nk <- nrow(as.data.frame(data_object$ch))

  # Use LOOCV when k equals the number of hard data points
  if (k == nk) {

    result <- bme_loocv(
      data_object = data_object,
      model = model,
      nugget = nugget,
      sill = sill,
      range = range,
      nsmax = nsmax,
      nhmax = nhmax,
      n = n,
      zk_range = zk_range,
      type = type
    )

  } else if (k < nk) {

    result <- bme_kfcv(
      data_object = data_object,
      model = model,
      nugget = nugget,
      sill = sill,
      range = range,
      nsmax = nsmax,
      nhmax = nhmax,
      n = n,
      zk_range = zk_range,
      type = type,
      k = k
    )

  } else {

    stop("k cannot be greater than the number of hard data locations.")
  }

  return(result)
}
