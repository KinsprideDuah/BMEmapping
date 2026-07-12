#' @title BME prediction using the random quantile approach
#'
#' @usage q_bme_predict(x, data_object, nsmax = 5, nhmax = 5, n = 50, nq = 3,
#'                    zk_range = extended_range(data_object), type)
#'
#' @param x A two-column matrix of spatial coordinates for the estimation
#'        locations.
#' @param data_object A list containing the hard and soft data.
#' @param nsmax A positive numeric value specifying the maximum number of nearby
#'        soft data points to include for estimation (default is 5).
#' @param nhmax A positive numeric value specifying the maximum number of nearby
#'        hard data points to include for estimation (default is 5).
#' @param n A positive numeric value indicating the number of points at which to
#'        evaluate the posterior density over \code{zk_range} (default is 50).
#' @param nq A positive numeric value for the number of quantile levels
#'        (default is 3).
#' @param zk_range A numeric vector specifying the range over which to evaluate
#'        the unobserved value at the estimation location (\code{zk}). Although
#'        \code{zk} is unknown,  it is assumed to lie within a range similar to
#'        the observed data (\code{zh}, \code{a}, and \code{b}). It is advisable
#'        to explore the posterior distribution at a few locations using
#'        \code{prob_zk()} before finalizing this range.
#' @param type A string indicating the type of BME prediction to compute: either
#'        \code{"mean"} for the posterior mean or \code{"mode"} for the
#'        posterior mode.
#'
#' @returns A data frame with either 3 or 4 columns, depending on the prediction
#'          type. The first two columns contain the geographic coordinates. If
#'          \code{type = "mean"}, the third and fourth columns represent the
#'          posterior mean and its associated variance, respectively. If
#'          \code{type = "mode" or "median"}, only a third column is returned for
#'          the posterior mode or median.
#'
#' @description
#' \code{bme_predict} performs BME spatial interpolation at user-specified
#' estimation locations. It uses both hard data (precise measurements) and soft
#' data (interval or uncertain measurements), along with a specified variogram
#' model, to compute either the posterior mean (and its associated variance),
#' mode or median for each location. This function enables spatial prediction in
#' settings where uncertainty in data must be explicitly accounted for,
#' improving estimation accuracy when soft data is available.
#'
#' @examples
#' data("utsnowload")
#' x <- utsnowload[1:3, c("latitude", "longitude")]
#' ch <- utsnowload[6:67, c("latitude", "longitude")]
#' cs <- utsnowload[68:232, c("latitude", "longitude")]
#' zh <- utsnowload[6:67, c("hard")]
#' a <- utsnowload[68:232, c("lower")]
#' b <- utsnowload[68:232, c("upper")]
#' data_object <- bme_map(ch, cs, zh, a, b)
#' q_bme_predict(x, data_object, type = "mean")
#' @importFrom stats approx
#' @export
q_bme_predict <- function(x, data_object, nsmax = 5, nhmax = 5, n = 50,
                            nq = 3, zk_range = extended_range(data_object),
                            type) {

  type <- match.arg(type, choices = c("mean", "mode", "median"))

  cols <- if (type == "mode") {
    1
  } else if (type == "mean") {
    c(2, 4)
  } else {
    3
  }

  est_names <- if (type == "mode") {
    "mode"
  } else if (type == "mean") {
    c("mean", "variance")
  } else {
    "median"
  }

  est <- q_bme_estimate(
    x = x,
    data_object = data_object,
    nsmax = nsmax,
    nhmax = nhmax,
    n = n,
    zk_range = zk_range
  )[, cols, drop = FALSE]

  x_names <- if (is.null(colnames(x))) c("coord.1", "coord.2") else colnames(x)

  result <- cbind.data.frame(x, est)
  names(result) <- c(x_names, est_names)

  structure(result, class = c("BMEmapping", "data.frame"))
}
