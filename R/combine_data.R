# ============================================================================
# Wrapper to new prob_zk function
#
# Details:
# Generate quantile values in the interval for each pair from the bounds of the
# intervals for each soft data location and combines the hard data and each
# quatile values
#
# Inputs:
# -  a vector of hard data values
# -  a vector of lower bounds of soft data
# -  b vector of lower bounds of soft data
# -  nq number of partitions between the bounds of the inetrvals
#
# Outputs:
# - A (nh + ns) by (qn + 2) matrix of hard data values and qunatiles:
#   [[zh,a) (zh,q1) (zh,q2) ... (zh,qn) (zh,b)]
# ============================================================================

combine_data <- function(zh, a, b, nq) {
  mat <- t(mapply(function(start, end) {
    seq(start, end, length.out = nq + 2)
  }, a, b))

  df <- sapply(1:ncol(mat), function(j) {
    c(zh, mat[, j])
  })

  return(df)
}
