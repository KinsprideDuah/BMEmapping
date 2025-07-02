# -----------------------------------------------------------------------------
# Utility function to convert single-column data frames to vectors,
# and all other data frames to numeric matrices
# -----------------------------------------------------------------------------
clean_input <- function(obj) {
  if (is.data.frame(obj)) {
    if (ncol(obj) == 1) {
      return(obj[[1]])
    } else {
      return(as.matrix(obj))
    }
  }
  return(obj)
}
