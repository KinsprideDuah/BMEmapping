#' @title Plot Method for BMEmapping Objects (ggplot2)
#'
#' @description
#' Uses ggplot2 to generate diagnostic or posterior density plots for objects
#' of class \code{"BMEmapping"} returned by \code{bme_cv()} or \code{prob_zk()}.
#'
#' @param x An object of class \code{"BMEmapping"}.
#' @param ... Additional arguments (currently unused).
#'
#' @importFrom ggplot2 aes after_stat geom_abline geom_density geom_histogram
#'     geom_hline geom_line geom_point ggplot labs theme_minimal
#' @importFrom stats density IQR ppoints qnorm sd
#' @importFrom utils globalVariables
#' @export
plot.BMEmapping <- function(x, ...) {
  if (!inherits(x, "BMEmapping")) {
    stop("Object must be of class 'BMEmapping'.")
  }

  if (is.null(ncol(x))) {
   # Create unified coordinate names
     coord_names <- if (is.null(colnames(x$ch))) c("x", "y") else colnames(x$ch)

   # Bind coordinates and label types
   df <- data.frame(
     rbind(x$ch, x$cs),
     type = factor(c(
       rep("Hard", nrow(x$ch)),
       rep("Soft", nrow(x$cs))
     ), levels = c("Hard", "Soft"))
   )

   df$z <- c(rep(0, length(x$zh)), x$b - x$a)

   # Split into two groups: r == 0 and r != 0
   df_hard <- df[df$z == 0, ]
   df_soft <- df[df$z != 0, ]

   # Assign column names
   colnames(df_hard)[1:2] <- c("x", "y")
   colnames(df_soft)[1:2] <- c("x", "y")

   p <- ggplot() +
     ggplot2::geom_point(data = df_hard, aes(x = x, y = y, shape = type),
                color = "gray40", size = 2) +
     ggplot2::geom_point(data = df_soft, aes(x = x, y = y, shape = type,
                                             color = z),
                size = 2) +
     ggplot2::scale_color_gradient(low = "blue", high = "red") +
     ggplot2::labs(
       title = "Spatial Data",
       x = coord_names[1],
       y = coord_names[2],
       color = "Intervl width",
       shape = "Data type"
     ) +
     ggplot2::theme_minimal(base_size = 10) +
     ggplot2::theme(
       panel.border = ggplot2::element_rect(color = "black", fill = NA),
       plot.title = ggplot2::element_text(hjust = 0.5, size = 15),
       legend.position = "right",
       legend.direction = "vertical"
      )
    print(p)
    return(invisible(p))
  }

  if (ncol(x) == 2) {
    # Posterior density case
    colnames(x) <- c("zk_i", "prob_zk_i")
    p <- ggplot2::ggplot(x, aes(x = zk_i, y = prob_zk_i)) +
      ggplot2::geom_line(color = "blue", linewidth = 0.5) +
      ggplot2::labs(x = "z", y = "f(z)") +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white",color = "black")
      )
    print(p)
    return(invisible(p))
  }

  # Diagnostic case: Requires predicted + residuals
  pred_col <- names(x)[names(x) %in% c("mean", "mode")]
  if (length(pred_col) == 0 || !"residual" %in% names(x)) {
    stop("BMEmapping object must contain 'mean' or 'mode' and 'residual' columns.")
  }

  x$predicted <- x[[pred_col[1]]]
  x$std_resid <- x$residual / sd(x$residual, na.rm = TRUE)
  x$qq_theoretical <- stats::qnorm(ppoints(nrow(x)))


  # --- Observed vs Predicted ---
  p1 <- ggplot2::ggplot(x, aes(x = predicted, y = observed)) +
    ggplot2::geom_point(color = "darkgreen", alpha = 0.7) +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                         color = "red", linetype = "dashed") +
    ggplot2::labs(title = "Observed vs Predicted",
                  x = "Predicted Values", y = "Observed Values") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.background = ggplot2::element_rect(fill = "white", color = "black"),
      panel.grid.minor = ggplot2::element_blank()
    )


  # --- Histogram of Residuals ---
  # Freedman–Diaconis
  IQR_val <- stats::IQR(x$residual)
  n <- length(x$residual)
  bin_width <- 2 * IQR_val / n^(1/3)

  p2 <- ggplot2::ggplot(x, ggplot2::aes(x = residual)) +
    ggplot2::geom_histogram(aes(y = after_stat(density)),
                            binwidth = bin_width,
                            fill = "grey80",
                            color = "white") +
    ggplot2::geom_density(color = "red", linewidth = 0.8) +
    ggplot2::labs(title = "Histogram of Residuals",
                  x = "Residual",
                  y = "Density") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5 ),
      panel.background = ggplot2::element_rect(fill = "white", color = "black"),
      panel.grid.minor = ggplot2::element_blank()
    )


  # --- Residuals vs. Predicted ---
  p3 <- ggplot2::ggplot(x, aes(x = predicted, y = residual)) +
    ggplot2::geom_point(color = "steelblue",
                        alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0,
                        linetype = "dashed",
                        color = "red") +
    ggplot2::labs(title = "Residuals vs Predicted",
                  x = "Predicted Values",
                  y = "Residuals") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.background = ggplot2::element_rect(fill = "white", color = "black"),
      panel.grid.minor = ggplot2::element_blank()
    )


  # --- Q-Q Plot of Residuals ---
  p4 <- ggplot2::ggplot(x, aes(x = qq_theoretical, y = sort(std_resid))) +
    ggplot2::geom_point(color = "darkgreen",
                        alpha = 0.7) +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                         color = "red", linetype = "dashed") +
    ggplot2::labs(title = "Q-Q Plot of Residuals",
                  x = "Theoretical Quantiles",
                  y = "Standardized Residuals") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.background = ggplot2::element_rect(fill = "white",color = "black"),
      panel.grid.minor = ggplot2::element_blank()
    )


  # Arrange in 2x2 grid
  gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)

}


