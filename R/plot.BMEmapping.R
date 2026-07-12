#' @title Plot Method for BMEmapping Objects
#'
#' @description
#' Generates graphical displays for objects of class \code{"BMEmapping"}.
#' The output depends on the object type:
#' \itemize{
#'   \item Objects returned by \code{bme_map()} produce a spatial plot showing
#'         hard and soft data locations.
#'   \item Objects returned by \code{prob_zk()} produce posterior density plots.
#'   \item Objects returned by \code{bme_predict()} or \code{bme_interval()}
#'         produce spatial prediction maps.
#'   \item Objects returned by \code{bme_cv()} produce diagnostic plots for
#'         evaluating prediction performance.
#' }
#'
#' @param x An object of class \code{"BMEmapping"}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A \pkg{ggplot2} object or an arranged collection of plots,
#' returned invisibly.
#'
#' @importFrom ggplot2 aes after_stat coord_equal element_blank element_rect
#' element_text geom_abline geom_density geom_histogram geom_hline
#' geom_line geom_point ggplot labs scale_colour_gradient theme theme_minimal
#' @importFrom gridExtra grid.arrange
#' @importFrom stats approx density
#' @importFrom stats IQR ppoints qnorm sd
#' @export
plot.BMEmapping <- function(x, ...) {

  if (!inherits(x, "BMEmapping")) {
    stop("Object must be of class 'BMEmapping'.")
  }


  ## Common theme
  bme_theme <- function() {
    ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        panel.background = ggplot2::element_rect(
          fill = "white",
          color = "black"
        ),
        panel.grid.minor = ggplot2::element_blank()
      )
  }



  ## ===============================================================
  ## bme_map object
  ## ===============================================================

  if (is.null(ncol(x))) {

    coord_names <- if (is.null(colnames(x$ch))) {
      c("x", "y")
    } else {
      colnames(x$ch)
    }


    df <- data.frame(
      rbind(x$ch, x$cs),
      type = factor(
        c(
          rep("Hard", nrow(x$ch)),
          rep("Soft", nrow(x$cs))
        ),
        levels = c("Hard", "Soft")
      )
    )

    df$interval_width <- c(
      rep(0, length(x$zh)),
      x$b - x$a
    )


    names(df)[1:2] <- c("x", "y")

    df_hard <- df[df$interval_width == 0, ]
    df_soft <- df[df$interval_width > 0, ]


    p <- ggplot2::ggplot() +

      ggplot2::geom_point(
        data = df_hard,
        ggplot2::aes(
          x = x,
          y = y,
          shape = type
        ),
        colour = "blue",
        size = 2
      ) +

      ggplot2::geom_point(
        data = df_soft,
        ggplot2::aes(
          x = x,
          y = y,
          shape = type,
          colour = interval_width
        ),
        size = 2
      ) +

      ggplot2::scale_colour_gradient(
        low = "yellow",
        high = "red"
      ) +

      ggplot2::labs(
        x = coord_names[1],
        y = coord_names[2],
        colour = "Interval width",
        shape = "Data type"
      ) +

      bme_theme()


    print(p)
    return(invisible(p))
  }



  ## ===============================================================
  ## Posterior density
  ## ===============================================================

  if (ncol(x) == 2) {

    df <- data.frame(
      zk_i = x[[1]],
      prob_zk_i = x[[2]]
    )


    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = zk_i,
        y = prob_zk_i
      )
    ) +

      ggplot2::geom_line(
        colour = "blue"
      ) +

      ggplot2::labs(
        x = "z",
        y = "Posterior density"
      ) +

      bme_theme()


    print(p)
    return(invisible(p))
  }



  ## ===============================================================
  ## Spatial prediction plots
  ## ===============================================================

  if (ncol(x) %in% c(3, 4)) {

    coord_x <- names(x)[1]
    coord_y <- names(x)[2]


    make_plot <- function(z) {

      ggplot2::ggplot(
        x,
        ggplot2::aes(
          x = .data[[coord_x]],
          y = .data[[coord_y]],
          colour = .data[[z]]
        )
      ) +

        ggplot2::geom_point(size = 2) +

        ggplot2::coord_equal() +

        ggplot2::scale_colour_gradient(
          low = "blue",
          high = "red"
        ) +

        ggplot2::labs(
          title = z,
          x = coord_x,
          y = coord_y,
          colour = z
        ) +

        bme_theme()
    }


    if (ncol(x) == 3) {

      p <- make_plot(names(x)[3])

      print(p)
      return(invisible(p))

    } else {

      p <- gridExtra::grid.arrange(
        make_plot(names(x)[3]),
        make_plot(names(x)[4]),
        ncol = 2
      )

      return(invisible(p))
    }
  }



  ## ===============================================================
  ## Cross-validation diagnostics
  ## ===============================================================


  pred_col <- intersect(
    names(x),
    c("mean", "mode", "median")
  )


  if (length(pred_col) == 0 ||
      !"residual" %in% names(x)) {

    stop(
      "Cross-validation object must contain a prediction column ",
      "('mean', 'mode', or 'median') and 'residual'."
    )
  }


  x$predicted <- x[[pred_col[1]]]

  x$std_resid <- x$residual /
    stats::sd(x$residual, na.rm = TRUE)

  x$qq <- stats::qnorm(
    stats::ppoints(nrow(x))
  )


  p1 <- ggplot2::ggplot(
    x,
    ggplot2::aes(predicted, observed)
  ) +
    ggplot2::geom_point(colour = "darkgreen") +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      colour = "red",
      linetype = "dashed"
    ) +
    ggplot2::labs(
      title = "Observed vs Predicted"
    ) +
    bme_theme()



  bw <- 2 * stats::IQR(x$residual) /
    length(x$residual)^(1/3)


  p2 <- ggplot2::ggplot(
    x,
    ggplot2::aes(residual)
  ) +

    ggplot2::geom_histogram(
      ggplot2::aes(
        y = ggplot2::after_stat(density)
      ),
      binwidth = bw,
      fill = "grey80",
      colour = "white"
    ) +

    ggplot2::geom_density(
      colour = "red"
    ) +

    ggplot2::labs(
      title = "Residual Distribution"
    ) +

    bme_theme()



  p3 <- ggplot2::ggplot(
    x,
    ggplot2::aes(
      predicted,
      residual
    )
  ) +

    ggplot2::geom_point(
      colour = "steelblue"
    ) +

    ggplot2::geom_hline(
      yintercept = 0,
      colour = "red",
      linetype = "dashed"
    ) +

    ggplot2::labs(
      title = "Residuals vs Predicted"
    ) +

    bme_theme()



  p4 <- ggplot2::ggplot(
    x,
    ggplot2::aes(
      qq,
      sort(std_resid)
    )
  ) +

    ggplot2::geom_point(
      colour = "darkgreen"
    ) +

    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      colour = "red",
      linetype = "dashed"
    ) +

    ggplot2::labs(
      title = "Normal Q-Q Plot"
    ) +

    bme_theme()



  gridExtra::grid.arrange(
    p1, p2, p3, p4,
    ncol = 2
  )
}
