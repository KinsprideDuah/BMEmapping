# ============================================================================
# Wrapper to prob-zk functions
#
# Details:
# Checks if ch and cs are either matrices or data frames
# ============================================================================
check_matrix_or_dataframe <- function(x, name) {
  if (!(is.data.frame(x) || is.matrix(x))) {
    stop(paste("Error:", name, "must be either a data frame or a matrix."))
  }
}
