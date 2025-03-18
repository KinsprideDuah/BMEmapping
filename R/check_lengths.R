# ============================================================================
# Wrapper to prob-zk functions
#
# Details:
# Checks if ch and cs are either matrices or data frames
# ============================================================================
check_lengths <- function(ch, zh, cs, a, b) {
  if (nrow(ch) != length(zh)) {
    stop("Error: Number of hard data (zh) and their corresponding, \n
          locations (ch) are different.")
  }

  if (length(a) != length(b)) {
    stop("Error: Number of lower bounds must equal number of upper bounds.")
  }

  if (nrow(cs) != length(a)) {
    stop("Error: Number of soft data (a or b) and their corresponding,
         \n locations (cs) are different.")
  }
}
