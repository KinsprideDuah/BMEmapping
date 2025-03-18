# ============================================================================
# Wrapper to prob-zk functions
#
# Details:
# # Check if zh, a, and b are vectors
# ============================================================================
check_vectors <- function(zh, a, b) {
  if (!(is.vector(zh) && is.vector(a) && is.vector(b))) {
    stop("Error: zh, a and b must be vectors.")
  }
}
