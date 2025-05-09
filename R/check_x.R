# ============================================================================
# Wrapper to prob-zk functions
#
# Details:
# Checks if x is appropriate
# ============================================================================
check_x <- function(x, ch, cs) {
  if (!(is.vector(x) && length(x) == 2) && !(is.matrix(x) && ncol(x) == 2) &&
      !(is.data.frame(x) && ncol(x) == 2)) {
    stop("Error: x must be a vector of length 2, a 2-column matrix, or a 2-column data frame. Execution stopped.")
  }

  y <- rbind(ch, cs)
  if (any(apply(y, 1, function(row) all(row == x))) == TRUE) {
    stop("The prediction location cannot be part of the data locations: ch and cs")
  }
}

