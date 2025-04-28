#' @title Posterior density function
#'
#' @details Compute the posterior densities of assumed zki values
#'
#' @returns A two column matrix of zk and associated probabilities
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
#' prob_zk(x, ch, cs, zh, a, b, model, nugget, sill, range, nsmax, nhmax)
#'
#' @export
prob_zk <- function(x, ch, cs, zh, a, b, model, nugget, sill, range,
                    nsmax = 5, nhmax = 10) {
  set.seed(123)

  check_x(x, cs, ch)
  check_matrix_or_dataframe(ch, "ch")
  check_matrix_or_dataframe(cs, "cs")
  check_vectors(zh, a, b)
  check_lengths(ch, zh, cs, a, b)

  x <- matrix(x, ncol = 2)

  if (nrow(x) != 1) {
    stop("Can only compute the mapping set for a single location")
  }


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

  # additional mean and variance of soft data
  ms <- 1 / 2 * (b + a)
  #ms <- rep(0, nsmax)
  vs <- 1 / 12 * (b - a)^2

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


  # range of zk values
  zk_min <- min(c(zh, a))
  zk_max <- max(c(zh, b))

  n <- 30
  zk_vec <- seq(from = zk_min, to = zk_max, length.out = n)


  ###########################################################################
  #   Part a: compute normalization constant
  ###########################################################################

  # lower and upper limits
  lower_a <- a # c(a - cov_s_h %*% solve(cov_h_h, zh))
  upper_a <- b # c(b - cov_s_h %*% solve(cov_h_h, zh))

  inv_cov_hs_hs <- solve(cov_h_h)

  # covariance matrix
  cov_a <- cov_s_s - cov_s_h %*% inv_cov_hs_hs %*% cov_h_s

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

    if (f_soft == 0) {f_soft <- 1e-4}
    if (aa == 0) {aa <- 1e-4}

    pk[i] <- round(((1 / aa) * f_zk * f_soft), 5)
  }

  d <- data.frame(zki = zk_vec, f_zki = pk)
  df <- d[!rowSums(is.na(d)), ]

  return(df)
}
