#' @title bme_cv function
#'
#' @details Leave-one-out cross validation (LOOCV) function to compute
#'          predictions for ONLY hard data locations.
#'
#' @returns Data frame of estimation locations with their corresponding
#'          predictions, residuals and folds.
#'
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
#' @examples
#' data("utah")
#' ch <- data.matrix(utah[2:67, c("lat", "lon")])
#' cs <- data.matrix(utah[68:232, c("lat", "lon")])
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
#' bme_cv(ch, cs, zh, a, b, model, nugget, sill, range, nsmax, nhmax, type)
#'
#' @export
bme_cv <- function(ch, cs, zh, a, b, model, nugget, sill, range, nsmax,
                      nhmax, type) {
  if (type == "mean") {
    d <- bme_mean_loocv(ch, cs, zh, a, b, model, nugget, sill, range,
                        nsmax, nhmax)
  } else if (type == "mode") {
    d <- bme_mode_loocv(ch, cs, zh, a, b, model, nugget, sill, range,
                        nsmax, nhmax)
  } else {
    d <- "The type can only be mean or mode."
  }

  return(d)
}
