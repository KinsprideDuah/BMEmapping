# Suppress R CMD check NOTE about NSE variables used in ggplot2
utils::globalVariables(c(
  "zk_i", "prob_zk_i", "predicted", "residual", "data_object",
  "qq_theoretical", "std_resid", "metrics", "observed", "y", "type", "z"
))
