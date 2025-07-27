#' @title Summary Method for BME Cross-Validation Results
#'
#' @description
#' Provides a concise summary of LOOCV performance for BME predictions at hard
#' data locations. Computes standard residual-based accuracy metrics, including
#' Mean Error (ME), Mean Absolute Error (MAE), and Root Mean Squared Error (RMSE).
#'
#' @param object An object of class \code{"bme_cv"}, typically returned by a
#'        BME cross-validation function. Must contain a numeric vector named
#'        \code{residual}.
#' @param ... Additional arguments passed to \code{summary()}.
#'
#' @return A data frame containing three columns:
#' \describe{
#'   \item{\code{ME}}{Mean Error (average residual).}
#'   \item{\code{MAE}}{Mean Absolute Error.}
#'   \item{\code{RMSE}}{Root Mean Squared Error.}
#' }
#' @export
summary.BMEmapping <- function(object, ...) {
  if (!inherits(object, "BMEmapping")) {
    stop("Object must be of class 'BMEmapping'.")
  }

  if (is.null(object$residual)) {
    stop("No residuals found in the object.")
  }

  observed <- object$observed
  residual <- object$residual
  n <- length(residual)


  # metrics
  metric_list <- list(

    # Mean Error (Bias)
    ME = mean(residual),

    # Mean Absolute Error
    MAE = mean(abs(residual)),

    # Root Mean Square Error
    RMSE = sqrt(mean(residual^2)),

    # Coefficient of Determination (R²)
    Rsquared = 1 - (sum(residual^2) / sum((observed - mean(observed))^2))
  )

  # Convert to two-column data frame
  metrics_df <- data.frame(
    Metric = names(metric_list),
    Value = unlist(metric_list),
    row.names = NULL
  )

  cat("Summary of BME Cross-Validation\n")
  cat("   for hard data locations\n")
  cat("--------------------------------\n")
  cat("Number of predictions:", n, "\n\n")

  for (i in seq_len(nrow(metrics_df))) {
    cat(sprintf("%-10s %8.4f\n", metrics_df$Metric[i], metrics_df$Value[i]))
  }

    metrics_df <- data.frame(
      Metric = names(metric_list),
      Value = unlist(metric_list),
      row.names = NULL
    )

  invisible(metrics_df)
}




