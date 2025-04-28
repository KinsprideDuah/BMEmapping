#' @title Posterior density plot function
#'
#' @details Plots the posterior density of an estimation location
#'
#' @returns graph
#'
#' @param x matrix of estimation locations. Cannot exceed 10 locations.
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
#'
#' @examples
#' data("utah")
#' x <- data.matrix(utah[1, c("lat", "lon")])
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
#' posterior_plot(x, ch, cs, zh, a, b, model, nugget, sill, range, nsmax, nhmax)
#'
#' @export
posterior_plot <- function(x, ch, cs, zh, a, b, model, nugget, sill, range,
                           nsmax, nhmax) {
  nk <- nrow(x)

  if (nk > 10) {
    stop("Error: Can only plot posterior density for not more than 4 locations.")
  }

  d_list <- list()

  for (i in 1:nk) {
    d_list[[i]] <- prob_zk(x[i, ], ch, cs, zh, a, b, model, nugget, sill, range,
                           nsmax, nhmax)
  }

  for (i in 1:nk) {
    post_plot(d_list[[i]])
    Sys.sleep(1)
  }

}
