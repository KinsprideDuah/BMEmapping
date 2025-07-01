#' @title Leave-one-out cross validation (LOOCV) at hard data locations.
#'
#' @description
#' \code{bme_cv} performs LOOCV to evaluate the prediction performance
#' of the Bayesian Maximum Entropy (BME) spatial interpolation method using both
#' hard and soft (interval) data.
#'
#' For each hard data location, the function removes the observed value and
#' predicts it using all remaining hard and soft data points. This is repeated
#' for every hard data location. The predictions are either posterior means or
#' posterior modes, depending on the \code{type} argument.
#'
#' The function returns prediction results at each location, including the
#' residuals (differences between observed and predicted values), and
#' computes three performance metrics:
#' \itemize{
#'   \item \strong{ME (Mean Error)} – measures prediction bias.
#'   \item \strong{MAE (Mean Absolute Error)} – measures average magnitude of
#'   prediction error.
#'   \item \strong{RMSE (Root Mean Squared Error)} – emphasizes larger errors
#'   and reflects prediction accuracy.
#' }
#'
#' This function is useful for validating the BME interpolation method and
#' tuning variogram parameters.
#'
#' @returns A list with two elements:
#' \describe{
#'   \item{\code{results}}{A data frame containing the coordinates, observed
#'   values, BME predictions (posterior \code{mean} or \code{mode}), posterior
#'   variance (if \code{type = "mean"}), residuals, and fold indices.}
#'   \item{\code{metrics}}{A one-row data frame reporting the mean error (ME),
#'   mean absolute error (MAE), and root mean squared error (RMSE) from the
#'   cross-validation.}
#' }
#'
#' @usage bme_cv(ch, cs, zh, a, b,
#'        model, nugget, sill, range, nsmax = 5,
#'        nhmax = 5, n = 50, zk_range = range(zh, a, b, -2, 2),
#'        type)
#'
#' @param ch A matrix of spatial coordinates for hard data locations
#'        (each row is a location).
#' @param cs A matrix of spatial coordinates for soft (interval) data locations.
#' @param zh A numeric vector of observed values at the hard data locations.
#' @param a A numeric vector of lower bounds for the soft interval data.
#' @param b A numeric vector of upper bounds for the soft interval data.
#' @param model A string specifying the variogram or covariance model to use
#'        (e.g., \code{"exp"}, \code{"sph"}, etc.).
#' @param nugget A non-negative numeric value for the nugget effect in the
#'        variogram model.
#' @param sill A numeric value representing the sill (total variance) in the
#'        variogram model.
#' @param range A positive numeric value for the range (or effective range)
#'        parameter of the variogram model.
#' @param nsmax An integer specifying the maximum number of nearby soft data
#'        points to include for estimation (default is 5).
#' @param nhmax An integer specifying the maximum number of nearby hard data
#'        points to include for estimation (default is 5).
#' @param n An integer indicating the number of points at which to evaluate the
#'        posterior density over \code{zk_range} (default is 50).
#' @param zk_range A numeric vector specifying the range over which to evaluate
#'        the unobserved value at the estimation location (\code{zk}). Although
#'        \code{zk} is unknown,  it is assumed to lie within a range similar to
#'        the observed data (\code{zh}, \code{a}, and \code{b}). It is advisable
#'        to explore the posterior distribution at a few locations using
#'        \code{prob_zk()} before finalizing this range. The default is
#'        \code{c(min(zh, a, -2), max(zh, b, 2))}.
#' @param type A string indicating the type of BME prediction to compute: either
#'        \code{"mean"} for the posterior mean or \code{"mode"} for the
#'        posterior mode.
#'
#' @examples
#' data("utsnowload")
#' ch <- data.matrix(utsnowload[2:10, c("latitude", "longitude")])
#' cs <- data.matrix(utsnowload[68:232, c("latitude", "longitude")])
#' zh <- c(utsnowload[2:10, c("hard")])
#' a <- c(utsnowload[68:232, c("lower")])
#' b <- c(utsnowload[68:232, c("upper")])
#' bme_cv(ch, cs, zh, a, b, model = "exp", nugget = 0.0953, sill = 0.3639,
#'        range = 1.0787, type = "mean")
#'
#' @export
bme_cv <- function(ch, cs, zh, a, b, model, nugget, sill, range, nsmax = 5,
                   nhmax = 5, n = 50, zk_range = extended_range(zh, a, b),
                   type) {
  type <- match.arg(type, choices = c("mean", "mode"))
  col_idx <- if (type == "mode") 1 else c(2, 3)
  col_names <- if (type == "mode") "mode" else c("mean", "variance")

  nh <- nrow(ch)
  est <- matrix(NA, nrow = nh, ncol = length(col_idx))

  for (i in seq_len(nh)) {
    est[i, ] <- bme_estimate(
      x = ch[i, ], ch = ch[-i, ], cs = cs, zh = zh[-i], a = a, b = b,
      model = model, nugget = nugget, sill = sill, range = range,
      nsmax = nsmax, nhmax = nhmax, n = n,
      zk_range = zk_range
    )[, col_idx]
  }

  ch_names <- if (is.null(colnames(ch))) c("coord.1", "coord.2") else colnames(ch)
  result <- cbind.data.frame(ch, zh, est, round(zh - est[, 1], 4), seq_len(nh))
  names(result) <- c(ch_names, "observed", col_names, "residual", "fold")

  metrics <- with(result, {
    c(
      ME = round(mean(residual), 4),
      MAE = round(mean(abs(residual)), 4),
      RMSE = round(sqrt(mean(residual^2)), 4)
    )
  })

  return(list(results = result, metrics = as.data.frame(t(metrics))))
}

