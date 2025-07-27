#' @title Create BMEmapping object
#'
#' @usage bme_map(ch, cs, zh, a, b)
#'
#' @param ch A two-column matrix of coordinates for hard data locations.
#' @param cs A two-column matrix of coordinates for soft data locations.
#' @param zh A numeric vector of observed values at the hard data locations.
#' @param a A numeric vector of lower bounds for soft interval data.
#' @param b A numeric vector of upper bounds for soft interval data.
#'
#' @returns A list containing the input arguments \code{ch}, \code{cs},
#'          \code{zh}, \code{a}, and \code{b}, with class \code{"BMEmapping"}.
#'
#' @description
#' Function that creates BMEmapping objects; objects that hold all the data
#' information necessary for BME interpolation.
#'
#' @examples
#' ch <- matrix(c(1, 2, 3, 4), ncol = 2)
#' cs <- matrix(c(5, 6, 7, 8), ncol = 2)
#' zh <- c(10, 20)
#' a <- c(8, 9)
#' b <- c(12, 14)
#' obj <- bme_map(ch, cs, zh, a, b)
#' class(obj)  # "BMEmapping"
#'
#' @export
bme_map <- function(ch, cs, zh, a, b) {

  ch <- clean_input(ch)
  cs <- clean_input(cs)
  zh <- clean_input(zh)
  a  <- clean_input(a)
  b  <- clean_input(b)

  check_matrix_or_dataframe(ch, "ch")
  check_matrix_or_dataframe(cs, "cs")
  check_vectors(zh, a, b)
  check_lengths(ch, zh, cs, a, b)

  result <- list(
    ch = ch,
    cs = cs,
    zh = zh,
    a = a,
    b = b
  )

  return(structure(result, class = c("BMEmapping", "list")))
}
