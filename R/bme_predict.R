#' @title Bayesian Maximum Entropy (BME) Spatial Interpolation
#'
#' @description
#' \code{bme_predict} performs BME spatial interpolation at user-specified
#' estimation locations. It uses both hard data (precise measurements) and soft
#' data (interval or uncertain measurements), along with a specified variogram
#' model, to compute either the posterior mean or mode and associated variance
#' for each location. This function enables spatial prediction in settings where
#' uncertainty in data must be explicitly accounted for, improving estimation
#' accuracy when soft data is available.
#'
#' @usage bme_predict(x, ch, cs, zh, a, b,
#'             model, nugget, sill, range, nsmax = 5,
#'             nhmax = 5, n = 50, zk_range = range(zh, a, b, -2, 2),
#'             type)
#'
#' @param x A two-column matrix of spatial coordinates for the estimation
#'        locations.
#' @param ch A two-column matrix of spatial coordinates for hard data locations.
#' @param cs A two-column matrix of spatial coordinates for soft (interval) data
#'        locations.
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
#'        \code{c(min(zh, a, -2), max(zh, b, 2)}.
#' @param type A string indicating the type of BME prediction to compute: either
#'        \code{"mean"} for the posterior mean or \code{"mode"} for the
#'        posterior mode.
#'
#' @returns A data frame with four columns: the first two contain the geographic
#'          coordinates, the third provides the BME prediction (posterior mean
#'          or mode), and the fourth gives the associated posterior variance.
#'
#' @import mvtnorm
#'
#' @examples
#' data("utsnowload")
#' x <- data.matrix(utsnowload[1, c("latitude", "longitude")])
#' ch <- data.matrix(utsnowload[2:67, c("latitude", "longitude")])
#' cs <- data.matrix(utsnowload[68:232, c("latitude", "longitude")])
#' zh <- c(utsnowload[2:67, c("hard")])
#' a <- c(utsnowload[68:232, c("lower")])
#' b <- c(utsnowload[68:232, c("upper")])
#' bme_predict(x, ch, cs, zh, a, b, model = "exp", nugget = 0.0953,
#'             sill = 0.3639, range = 1.0787, type = "mean")
#'
#' @export
bme_predict <- function(x, ch, cs, zh, a, b, model, nugget, sill, range,
                        nsmax = 5, nhmax = 5, n = 50,
                        zk_range = range(zh, a, b, -2, 2), type) {

  type <- match.arg(type, choices = c("mean", "mode"))

  cols <- c(if (type == "mode") 1 else 2, 3)
  names <- c(if (type == "mode") "mode" else "mean", "variance")

  est <- bme_estimate(x, ch, cs, zh, a, b, model, nugget, sill, range,
                      nsmax, nhmax, n, zk_range)[, cols]
  est <- matrix(est, ncol = 2)

  result <- data.frame(coord.1 = x[, 1],
                       coord.2 = x[, 2],
                       est1 = est[, 1],
                       est1 = est[, 2])
  names(result)[3:4] <- names

  return(result)
}
