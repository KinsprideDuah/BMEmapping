#' @title sim_cholesky function
#'
#' @details This method implements the traditional non-conditional simulation
#'          using Cholesky decomposition of the covariance matrix. This method
#'          implements the traditional non-conditional simulation. Cholesky
#'          decomposition is a matrix factorization technique used primarily for
#'          symmetric, positive-definite matrices. It expresses a matrix as the
#'          product of a lower triangular matrix and its transpose. It is ideal
#'          for generating multiple independent sets of zero-mean Gaussian
#'          distributed values, typically for a small number of hard data
#'          points.
#'
#' @returns A vector of simulated Gaussian values.
#'
#' @param x matrix of coordinates representing the locations where hard data
#'          will be simulated. Each row corresponds to the coordinate vector
#'          of a simulation location, with the number of columns indicating the
#'          spatial dimension. There are no restrictions on the dimensionality
#'          of the space.
#' @param model string name of covariance or variogram model
#' @param nugget a non-negative value
#' @param sill a non-negative value
#' @param range a non-negative value
#'
#' @examples
#' x <- matrix(runif(10), ncol = 2)
#' model <- "sph"
#' nugget <- 0
#' sill <- 1
#' range <- 1
#' sim_cholesky(x, model, nugget, sill, range)
#'
#' @export
#'
sim_cholesky <- function(x, model, nugget, sill, range) {

  K <- covmat(x, x, model, nugget, sill, range)
  L <- chol(K, pivot = TRUE)
  Zh <- as.vector(t(L) %*% stats::rnorm(nrow(x)))

  return(round(Zh, 4))
}


