#' @title Posterior Density Estimation at a Single Location
#'
#' @usage q_prob_zk(x, data_object, nsmax = 5, nhmax = 5, n = 50, nq = 3,
#'                  zk_range = extended_range(data_object))
#'
#' @param x A two-column matrix of spatial coordinates for a single estimation
#'        location.
#' @param data_object A list containing the hard and soft data.
#' @param nq A positive numeric value for the number of quantile levels
#'        (default is 3).
#' @param nsmax A positive numeric value specifying the maximum number of nearby
#'        soft data points to include for estimation (default is 5).
#' @param nhmax A positive numeric value specifying the maximum number of nearby
#'        hard data points to include for estimation (default is 5).
#' @param n An integer indicating the number of points at which to evaluate the
#'        posterior density over \code{zk_range} (default is 50).
#' @param nq An integer indicating the number of quantile levels (default is 3).
#' @param zk_range A numeric vector specifying the range over which to evaluate
#'        the unobserved value at the estimation location (\code{zk}). Although
#'        \code{zk} is unknown,  it is assumed to lie within a range similar to
#'        the observed data (\code{zh}, \code{a}, and \code{b}). It is advisable
#'        to explore the posterior distribution at a few locations using
#'        \code{prob_zk()} before finalizing this range
#'
#' @return A data frame with two columns: \code{zk_i} (assumed zk values) and
#'         \code{prob_zk_i} (corresponding posterior densities).
#'
#' @description
#' Computes the posterior and plots probability density function (PDF) at a
#' single unobserved spatial location using the Bayesian Maximum Entropy (BME)
#' framework. This function integrates both hard data (precise measurements) and
#' soft data (interval or uncertain observations), together with a specified
#' variogram model, to numerically estimate the posterior density across a
#' range of possible values.
#'
#' @examples
#' data("utsnowload")
#' x <- utsnowload[1, c("latitude", "longitude")]
#' ch <- utsnowload[2:67, c("latitude", "longitude")]
#' cs <- utsnowload[68:232, c("latitude", "longitude")]
#' zh <- utsnowload[2:67, "hard"]
#' a <- utsnowload[68:232, "lower"]
#' b <- utsnowload[68:232, "upper"]
#' data_object <- bme_map(ch, cs, zh, a, b)
#' q_prob_zk(x, data_object)
#'
#' @export

q_prob_zk <- function(x, data_object, nsmax = 5, nhmax = 5, n = 50, nq = 3,
                          zk_range = extended_range(data_object)) {

  x <- clean_input(x)
  check_xx(x)

  ch <- data_object$ch
  cs <- data_object$cs
  zh <- data_object$zh
  a <- data_object$a
  b <- data_object$b

  if (nrow(x) != 1) {
    stop("Can only compute the mapping set for a single location")
  }

  # set up zk vector
  zk_vec <- seq(from = zk_range[1], to = zk_range[2], length.out = n)

  # Generate matrix of quantile values and combine with hard data values
  d <- combine_data(zh = zh, a = a, b = b, nq = nq)

  # Fit a variogram to each column of the data and extract the variogram
  # information (model and parameters)
  vg_info <- vg_results(ch = ch, cs = cs, d = d)

  # sorting nhmax hard data locations closest to the estimation location
  new_ch <- ch_nhmax(x = x, ch = ch, nhmax = nhmax)
  ch <- new_ch[[1]]
  index_h <- new_ch[[2]]
  zh <- zh[index_h]

  # sorting nsmax hard data locations closest to the estimation location
  new_cs <- cs_nsmax(x = x, cs = cs, nsmax = nsmax)
  cs <- new_cs[[1]]
  index_s <- new_cs[[2]]
  a <- a[index_s]
  b <- b[index_s]

  # Use the variogram information to compute the covariance matrices for the
  # hard, soft and estimations and average them
  average_matrix <- covmat_avg(ch = ch, cs = cs, x = x, vg_info = vg_info)


  # Build the covariance matrix
  cov_h_h <- average_matrix$cov_h_h
  cov_h_k <- average_matrix$cov_h_k
  cov_h_s <- average_matrix$cov_h_s
  cov_k_k <- average_matrix$cov_k_k
  cov_s_k <- average_matrix$cov_s_k
  cov_s_s <- average_matrix$cov_s_s

  cov_s_h <- t(cov_h_s)
  cov_k_h <- t(cov_h_k)

  # Composite covariances
  cov_kh_kh <- rbind(cbind(cov_k_k, cov_k_h), cbind(cov_h_k, cov_h_h))
  cov_s_kh <- cbind(cov_s_k, cov_s_h)
  cov_kh_s <- t(cov_s_kh)

  ###########################################################################
  #   Part a: compute normalization constant
  ###########################################################################

  # lower and upper limits
  lower_a <- a
  upper_a <- b

  inv_cov_hs_hs <- solve(cov_h_h)

  # covariance matrix
  cov_a <- cov_s_s - cov_s_h %*% inv_cov_hs_hs %*% cov_h_s
  if (det(cov_a) <= 0) cov_a <- cov_s_s

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
  if (det(cov_soft) <= 0) cov_soft <- cov_s_s

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

    if (f_soft == 0) f_soft <- 1e-4
    if (aa == 0) aa <- 1e-4

    pk[i] <- round(((1 / aa) * f_zk * f_soft), 5)
  }

  d <- data.frame("zk_i" = zk_vec, "prob_zk_i" = pk)
  df <- d[!rowSums(is.na(d)), ]

  return(structure(df, class = c("BMEmapping", "data.frame")))
}
