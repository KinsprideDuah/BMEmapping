# ============================================================================
# Wrapper to prob-zk functions
#
# Details:
# Checks if x is appropriate
# ============================================================================
check_x <- function(x) {
  if (!(is.vector(x) && length(x) == 2) && !(is.matrix(x) && ncol(x) == 2) &&
      !(is.data.frame(x) && ncol(x) == 2)) {
    stop("Error: x must be a vector of length 2, a 2-column matrix,
         \n or a 2-column data frame. Execution stopped.")
  }
}
