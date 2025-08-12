#' Plot Regime-Specific Time Series Paths with Enhancements
#'
#' This function visualizes the actual vs fitted values of a selected variable from a TVAR model
#' over time, with shaded regime areas, threshold annotations, and optional structural residuals.
#'
#' @param model A fitted TVAR model object.
#' @param variable The name of the endogenous variable to plot.
#' @param time_index Optional vector of dates (same length as observations).
#' @param show_fitted Logical. If TRUE, overlay fitted values.
#' @param show_residuals Logical. If TRUE, plot regime residual bands.
#' @param save_path Optional file path to save the figure.
#' @param title Optional plot title.
#' @param subtitle Optional subtitle.
#' @param verbose Logical. If TRUE, prints extra info.
#'
#' @return A ggplot2 object.
#' @export
plot_tvar_regime_paths <- function(
    model,
    variable,
    time_index = NULL,
    show_fitted = TRUE,
    show_residuals = FALSE,
    save_path = NULL,
    title = NULL,
    subtitle = NULL,
    verbose = FALSE
) {
  stopifnot("regimes" %in% names(model))
  stopifnot(variable %in% colnames(model$regimes$low$Y))
  
  # Extract series
  actual_low <- model$regimes$low$Y[[variable]]
  actual_high <- model$regimes$high$Y[[variable]]
  fitted_low <- as_tibble(model$regimes$low$fitted)[[variable]]
  fitted_high <- as_tibble(model$regimes$high$fitted)[[variable]]
  
  n_low <- length(actual_low)
  n_high <- length(actual_high)
  total_n <- n_low + n_high
  
  # Time index setup
  if (is.null(time_index)) {
    time_index <- seq_len(total_n)
  }
  
  stopifnot(length(time_index) == total_n)
  
  df <- dplyr::tibble(
    time = time_index,
    actual = c(actual_low, actual_high),
    fitted = c(fitted_low, fitted_high),
    regime = factor(
      c(rep("low", n_low), rep("high", n_high)),
      levels = c("low", "high")
    )
  )
  
  # Residuals 
  if (show_residuals) {
    resid_low <- as_tibble(model$regimes$low$residuals)[[variable]]
    resid_high <- as_tibble(model$regimes$high$residuals)[[variable]]
    df <- dplyr::mutate(df, residual = c(resid_low, resid_high))
  }
  
  # Plot core
  p <- ggplot2::ggplot(df, ggplot2::aes(x = time)) +
    ggplot2::geom_line(ggplot2::aes(y = actual, color = "Actual"), linewidth = 1.1) +
    {
      if (show_fitted)
        ggplot2::geom_line(ggplot2::aes(y = fitted, color = "Fitted"), linewidth = 0.9, linetype = "dashed")
    } +
    ggplot2::geom_rect(
      data = df,
      ggplot2::aes(
        xmin = time - 0.5,
        xmax = time + 0.5,
        ymin = -Inf,
        ymax = Inf,
        fill = regime
      ),
      alpha = 0.06,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_color_manual(values = c("Actual" = "black", "Fitted" = "darkgreen")) +
    ggplot2::scale_fill_manual(values = c("low" = "steelblue", "high" = "tomato")) +
    ggplot2::labs(
      title = title %||% glue::glue("TVAR Regime Paths for '{variable}'"),
      subtitle = subtitle %||% "Shaded by Regime (Low/High)",
      x = "Time",
      y = variable,
      color = "Series"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  
  if (verbose) cat("\n[plot_tvar_regime_paths] Plotting complete.\n")
  
  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, p, width = 9, height = 5.5)
    message(glue::glue("Saved regime plot to: {save_path}"))
  } else {
    print(p)
  }
  
  invisible(p)
}