# ============================================================================
# Wrapper to new prob_zk function
#
# Details:
# Uses the extracted variogram informatioj from vg_results to compute the
# covariance matrices for each quantile data
#
# Inputs:
# -  x estimation location
# -  ch hard data locations
# -  cs soft data locations
# -  d dataframe of variogram information for the combine data (zh and quantile)
#
# Outputs:
# - A list of average matrices for hard, soft and estimation locations
# ============================================================================

covmat_avg <- function(ch, cs, x, vg_info) {
  # Initialize lists to store covariance matrices for each model row
  cov_h_h <- list()
  cov_h_k <- list()
  cov_h_s <- list()
  cov_k_k <- list()
  cov_s_k <- list()
  cov_s_s <- list()

  # Loop through each row of variogram parameters
  for (i in seq_len(nrow(vg_info))) {
    model <- vg_info$model[i]
    nugget <- vg_info$nugget[i]
    sill <- vg_info$sill[i]
    range <- vg_info$range[i]

    cov_h_h[[i]] <- covmat(ch, ch, model, nugget, sill, range)
    cov_h_k[[i]] <- covmat(ch, x, model, nugget, sill, range)
    cov_h_s[[i]] <- covmat(ch, cs, model, nugget, sill, range)
    cov_k_k[[i]] <- covmat(x, x, model, nugget, sill, range)
    cov_s_k[[i]] <- covmat(cs, x, model, nugget, sill, range)
    cov_s_s[[i]] <- covmat(cs, cs, model, nugget, sill, range)
  }

  # Compute element-wise averages of each list of matrices
  avg_matrix <- function(mat_list) {
    Reduce("+", mat_list) / length(mat_list)
  }

  # Return average matrices
  average_matrix <- list(
    cov_h_h = avg_matrix(cov_h_h),
    cov_h_k = avg_matrix(cov_h_k),
    cov_h_s = avg_matrix(cov_h_s),
    cov_k_k = avg_matrix(cov_k_k),
    cov_s_k = avg_matrix(cov_s_k),
    cov_s_s = avg_matrix(cov_s_s)
  )

  return(average_matrix)
}
