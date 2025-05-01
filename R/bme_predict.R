#' @title bme_predict function
#'
#' @details Compute the BME predictions for estimation locations.
#'
#' @returns Data frame of estimation locations with their corresponding BME
#'          predictions
#'
#' @param x matrix of estimation locations
#' @param ch matrix of hard data locations
#' @param cs matrix of soft data locations
#' @param zh vector of hard data
#' @param a vector of lower bounds of soft data
#' @param b vector of lower bounds of soft data
#' @param model string name of covariance or variogram model
#' @param nugget a non-negative value
#' @param sill a non-negative value
#' @param range a non-negative value
#' @param nsmax number of soft data locations closer to the estimation location
#' @param nhmax number of hard data locations closer to the estimation location
#' @param type string name for the type of prediction preferred. Type of
#'        prediction can either be "mean" (posterior mean) or "mode" (posterior
#'        mode)
#'
#' @import mvtnorm
#'
#' @examples
#' data("utah")
#' x <- data.matrix(utah[1, c("x", "y")])
#' ch <- data.matrix(utah[2:67, c("x", "y")])
#' cs <- data.matrix(utah[68:232, c("x", "y")])
#' zh <- c(utah[2:67, c("center")])
#' a <- c(utah[68:232, c("lower")])
#' b <- c(utah[68:232, c("upper")])
#' model <- "sph"
#' nugget <- 0.1184
#' sill <- 0.3474
#' range <- 119197
#' nsmax <- 5
#' nhmax <- 10
#' type <- "mean"
#' bme_predict(x, ch, cs, zh, a, b, model, nugget, sill, range, nsmax, nhmax,
#'             type)
#'
#' @export
bme_predict <- function(x, ch, cs, zh, a, b, model, nugget, sill, range,
                        nsmax, nhmax, type) {

  if (!(type %in% c("mean", "mode"))) {
    stop("Error: The type must be either 'mean' or 'mode'. Execution stopped.")
  }

  if (type == "mode") {
    d = bme_estimate(x, ch, cs, zh, a, b, model, nugget, sill, range,
                     nsmax, nhmax)[, c(1, 3)]
    d = data.frame(matrix(c(d), ncol = 2))
    names(d) = c("mode", "variance")
  } else if (type == "mean") {
    d = bme_estimate(x, ch, cs, zh, a, b, model, nugget, sill, range,
                     nsmax, nhmax)[, c(2, 3)]
    d = data.frame(matrix(c(d), ncol = 2))
    names(d) = c("mean", "variance")
  }

  y <- data.frame(matrix(c(x), ncol = 2))
  names(y) <- c("coord.1", "coord.2")
  df <- cbind.data.frame(y, d)

  return(df)
}
