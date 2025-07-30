# ============================================================================
# Wrapper to prob_zk function
#
# Details:
#' Computes an extended numeric range that includes all elements from three
#' numeric vectors: x, y, z. The range is extended by 10\% of the original
#' range on both sides
#
# Inputs:
# - x, y, z are numeric vectors of hard, lower and upper bounds of interval data
#
# Outputs:
# - A two-length vector containing the extended minimum and maximum value
# ============================================================================
#' @keywords internal
extended_range <- function(x) {
  # Combine vectors
  zh <- x$zh
  a  <- x$a
  b  <- x$b
  combined <- c(zh, a, b)

  # Compute min and max
  min_val <- min(combined, na.rm = TRUE)
  max_val <- max(combined, na.rm = TRUE)

  # Compute delta
  d <- (max_val - min_val) / 5

  # Return the extended range
  extended_range <- c(min_val - d, max_val + d)
  return(extended_range)
}
