#' @title Posterior Density Estimation at a Single Location
#'
#' @description
#' Computes the posterior and plots probability density function (PDF) at a
#' single unobserved spatial location using the Bayesian Maximum Entropy (BME)
#' framework. This function integrates both hard data (precise measurements) and
#' soft data (interval or uncertain observations), together with a specified
#' variogram model, to numerically estimate the posterior density across a
#' range of possible values. Optionally displays a plot of the posterior density
#' function for the specified location.
#'
#' @return Two elements:
#' \describe{
#'   \item{\emph{data frame}}{A data frame with two columns: \code{zk_i}
#'   (assumed zk values) and \code{prob_zk_i} (corresponding posterior
#'   densities).}
#'   \item{\emph{plot}}{An optional plot of posterior density of the estimation
#'   location.}
#' }
#'
#' @usage prob_zk(x, ch, cs, zh, a, b,
#'         model, nugget, sill, range, nsmax = 5,
#'         nhmax = 5, n = 50, zk_range = extended_range(zh, a, b),
#'         plot = FALSE)
#'
#' @param x A two-column matrix of spatial coordinates for a single estimation
#'        location.
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
#'        \code{extended_range(zh, a, b)}.
#' @param n An integer indicating the number of points at which to evaluate the
#'        posterior density over \code{zk_range}.
#' @param plot Logical; if \code{TRUE}, plots the posterior density curve.
#'
#' @examples
#' data("utsnowload")
#' x <- utsnowload[1, c("latitude", "longitude")]
#' ch <- utsnowload[2:67, c("latitude", "longitude")]
#' cs <- utsnowload[68:232, c("latitude", "longitude")]
#' zh <- utsnowload[2:67, "hard"]
#' a <- utsnowload[68:232, "lower"]
#' b <- utsnowload[68:232, "upper"]
#' prob_zk(x, ch, cs, zh, a, b, model = "exp", nugget = 0.0953, sill = 0.3639,
#'         range = 1.0787, plot = TRUE)
#'
#' @export
prob_zk <- function(x, ch, cs, zh, a, b, model, nugget, sill, range, nsmax = 5,
                    nhmax = 5, n = 50, zk_range = extended_range(zh, a, b),
                    plot = FALSE) {

  check_x(x, cs, ch)
  check_matrix_or_dataframe(ch, "ch")
  check_matrix_or_dataframe(cs, "cs")
  check_vectors(zh, a, b)
  check_lengths(ch, zh, cs, a, b)

  x  <- clean_input(x)
  ch <- clean_input(ch)
  cs <- clean_input(cs)
  zh <- clean_input(zh)
  a  <- clean_input(a)
  b  <- clean_input(b)

  if (nrow(x) != 1) {
    stop("Can only compute the mapping set for a single location")
  }

  # set up zk vector
  zk_vec <- seq(from = zk_range[1], to = zk_range[2], length.out = n)


  # sorting nhmax hard data locations closest to the estimation location
  new_ch <- ch_nhmax(x, ch, nhmax)
  ch <- new_ch[[1]]
  index_h <- new_ch[[2]]
  zh <- zh[index_h]

  # sorting nsmax hard data locations closest to the estimation location
  new_cs <- cs_nsmax(x, cs, nsmax)
  cs <- new_cs[[1]]
  index_s <- new_cs[[2]]
  a <- a[index_s]
  b <- b[index_s]

  # additional variance of soft data
  vs <- (b - a)^2 / 12

  # Build the covariance matrix
  cov_h_h <- covmat(ch, ch, model, nugget, sill, range)
  cov_h_k <- covmat(ch, x, model, nugget, sill, range)
  cov_h_s <- covmat(ch, cs, model, nugget, sill, range)

  cov_k_h <- covmat(x, ch, model, nugget, sill, range)
  cov_k_k <- covmat(x, x, model, nugget, sill, range)

  cov_s_h <- covmat(cs, ch, model, nugget, sill, range)
  cov_s_s <- covmat(cs, cs, model, nugget, sill, range)
  diag(cov_s_s) <- diag(cov_s_s) + vs

  # Composite covariances
  cov_kh_kh <- rbind(cbind(cov_k_k, cov_k_h), cbind(cov_h_k, cov_h_h))
  cov_s_kh <- covmat(cs, rbind(x, ch), model, nugget, sill, range)
  cov_kh_s <- covmat(rbind(x, ch), cs, model, nugget, sill, range)

  ###########################################################################
  #   Part a: compute normalization constant
  ###########################################################################

  # lower and upper limits
  lower_a <- a
  upper_a <- b

  inv_cov_hs_hs <- solve(cov_h_h)

  # covariance matrix
  cov_a <- cov_s_s - cov_s_h %*% inv_cov_hs_hs %*% cov_h_s
  #if (det(cov_a) <= 0) cov_a <- cov_s_s

  # mean vector
  mu_a <- c(cov_s_h %*% inv_cov_hs_hs %*% zh)

  aa <- mvtnorm::pmvnorm(
    lower = lower_a, upper = upper_a, mean = mu_a,
    sigma = cov_a
  )[1]

  # set up container
  pk <- numeric()

  # Part B: conditional mean and covariance of zk
  m_k <- c(cov_k_h %*% solve(cov_h_h, zh)) # mean
  cov_k <- cov_k_k - cov_k_h %*% inv_cov_hs_hs %*% cov_h_k # covariance

  # Part C: compute integral of soft data
  # conditional variance
  inv_cov_kh_kh <- solve(cov_kh_kh)
  cov_soft <- cov_s_s - cov_s_kh %*% inv_cov_kh_kh %*% cov_kh_s
  #if (det(cov_soft) <= 0) cov_soft <- cov_s_s

  for (i in 1:n) {

    ###########################################################################
    #      zk, zkh values
    ###########################################################################

    zk <- zk_vec[i]
    zk_h <- c(zk, zh)

    ###########################################################################
    #   Part B: compute density of zk
    ###########################################################################

    # density
    f_zk <- mvtnorm::dmvnorm(x = zk, mean = m_k, sigma = cov_k)[1]

    ###########################################################################
    #   Part C: compute integral of soft data
    ###########################################################################

    # lower and upper limits
    lower_soft <- c(a - cov_s_kh %*% inv_cov_kh_kh %*% zk_h)
    upper_soft <- c(b - cov_s_kh %*% inv_cov_kh_kh %*% zk_h)

    # conditional mean
    m_soft <- rep(0, nsmax)

    # Compute multidimensional integral
    f_soft <- mvtnorm::pmvnorm(
      lower = lower_soft, upper = upper_soft, mean = m_soft,
      sigma = cov_soft
    )[1]

    #if (f_soft == 0) f_soft <- 1e-4
    #if (aa == 0) aa <- 1e-4

    pk[i] <- round(((1 / aa) * f_zk * f_soft), 5)
  }

  d <- data.frame("zk_i" = zk_vec, "prob_zk_i" = pk)
  df <- d[!rowSums(is.na(d)), ]

  # Plot if requested
  if (plot) {
    plot(df$zk_i, df$prob_zk_i, type = "l", xlab = "z", ylab = "f(z)",
         main = "posterior density")
  }

  return(df)
}
