# ============================================================================
# Wrapper to posterior_plot function
#
# Details:
# Plots the posterior density of an estimation location
#
# Inputs:
# -  data frame of zk.i and prob
#
# Outputs:
# - posterior density plot
# ============================================================================
post_plot <- function(x) {
  plot(x = x[, 1], y = x[, 2], type = "l", xlab = "z", ylab = "f(z)",
       main = "posterior density")
}
