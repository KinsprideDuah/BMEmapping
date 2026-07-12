#' @title BME credible interval
#'
#' @usage q_bme_predict_ci(x, data_object, nsmax = 5, nhmax = 5,n = 50, nq = 3,
#'                         zk_range = extended_range(data_object), level)
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
#' @param nq An integer indicating the number of quantile levels (default is 3).
#' @param zk_range A numeric vector specifying the range over which to evaluate
#'        the unobserved value at the estimation location (\code{zk}). Although
#'        \code{zk} is unknown,  it is assumed to lie within a range similar to
#'        the observed data (\code{zh}, \code{a}, and \code{b}). It is advisable
#'        to explore the posterior distribution at a few locations using
#'        \code{prob_zk()} before finalizing this range.
#' @param level A numeric value between 0 and 1 specifying the credible interval
#'        level for uncertainty quantification. For example, \code{level = 0.90}
#'        computes a 90% credible interval using the posterior distribution.
#'
#' @returns A data frame whose first two columns contain the geographic
#'          coordinates of the estimation locations. The remaining two columns
#'          contain the lower and upper bounds of the posterior credible
#'          interval corresponding to the specified \code{level}. For example,
#'          if \code{level = 0.90}, the returned bounds represent the 90%
#'          posterior credible interval.
#'
#' @description
#' \code{bme_predict_ci} computes posterior credible intervals for each
#' estimation location at a user-specified credibility level using the random
#' quantile approach. The function explicitly incorporates uncertainty in the
#' available data, providing interval estimates that quantify the uncertainty
#' associated with BME spatial predictions.
#'
#' @examples
#' data("utsnowload")
#' x <- utsnowload[1:3, c("latitude", "longitude")]
#' ch <- utsnowload[5:67, c("latitude", "longitude")]
#' cs <- utsnowload[68:232, c("latitude", "longitude")]
#' zh <- utsnowload[5:67, c("hard")]
#' a <- utsnowload[68:232, c("lower")]
#' b <- utsnowload[68:232, c("upper")]
#' data_object <- bme_map(ch, cs, zh, a, b)
#' q_bme_predict_ci(x, data_object, level = 0.90)
#' @importFrom stats approx
#' @export
q_bme_predict_ci <- function(x, data_object, nsmax = 5, nhmax = 5,
                             n = 50, nq = 3,
                             zk_range = extended_range(data_object), level) {

  nk <- nrow(x)

  # set up container for estimates: lower and upper
  df <- matrix(NA, ncol = 2, nrow = nk)

  alpha <- 1 - level
  colnames(df) <- c(
    paste0("Lower_", level * 100),
    paste0("Upper_", level * 100)
  )

  for (i in 1:nk) {

    # Posterior density for i-th location
    d <- q_prob_zk(
      x = x[i, ], data_object = data_object, nsmax = nsmax, nhmax = nhmax,
      n = n, nq = nq, zk_range = zk_range
    )

    # Normalize PDF and calculate the cumulative distribution
    d$norm_prob <- d$prob_zk_i / sum(d$prob_zk_i)
    d$cdf <- cumsum(d$norm_prob)
    lower <- stats::approx(x = d$cdf, y = d$zk_i, xout = alpha / 2,
                    ties = "ordered")$y
    upper <- stats::approx(x = d$cdf, y = d$zk_i, xout = 1 - (alpha / 2),
                    ties = "ordered")$y
    df[i,] <- c(lower, upper)
  }

  return(cbind(x, round(df, 4)))
}
