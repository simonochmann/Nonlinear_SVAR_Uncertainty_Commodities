#' Plot Actual vs Fitted Values by Regime (Enhanced)
#'
#' This function visualizes the actual vs fitted values from a TVAR model across regimes.
#' It includes enhancements such as R² annotations, optional regression lines, residual
#' analysis, and metadata-aware captions for elite-level analysis.
#'
#' @param model A fitted TVAR model object.
#' @param variable The name of the endogenous variable to plot.
#' @param show_fit_lines Logical. If TRUE, overlays OLS fits.
#' @param show_residuals Logical. If TRUE, adds residual density panels.
#' @param save_path Optional file path to save the plot.
#' @param verbose Logical. If TRUE, print progress messages.
#'
#' @return A ggplot2 or patchwork object.
#' @export
plot_tvar_fitted_vs_actual <- function(
    model,
    variable,
    show_fit_lines = TRUE,
    show_residuals = FALSE,
    save_path = NULL,
    verbose = FALSE
) {
  stopifnot("regimes" %in% names(model))
  stopifnot(variable %in% colnames(model$regimes$low$Y))
  
  # Extract data
  actual_low <- model$regimes$low$Y[[variable]]
  actual_high <- model$regimes$high$Y[[variable]]
  fitted_low <- model$regimes$low$fitted[, variable]
  fitted_high <- model$regimes$high$fitted[, variable]
  
  # Compute residuals
  resid_low <- actual_low - fitted_low
  resid_high <- actual_high - fitted_high
  
  # Compute R² 
  r2_low <- 1 - sum(resid_low^2) / sum((actual_low - mean(actual_low))^2)
  r2_high <- 1 - sum(resid_high^2) / sum((actual_high - mean(actual_high))^2)
  
  df <- dplyr::tibble(
    actual = c(actual_low, actual_high),
    fitted = c(fitted_low, fitted_high),
    regime = factor(
      c(rep("low", length(actual_low)), rep("high", length(actual_high))),
      levels = c("low", "high")
    ),
    residual = c(resid_low, resid_high)
  )
  
  # Core scatter plot with 45° line
  base_plot <- ggplot2::ggplot(df, ggplot2::aes(x = actual, y = fitted, color = regime)) +
    ggplot2::geom_point(alpha = 0.8) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "gray40") +
    {
      if (show_fit_lines) ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 0.6)
    } +
    ggplot2::facet_wrap(~ regime, scales = "free") +
    ggplot2::scale_color_manual(values = c("low" = "steelblue", "high" = "tomato")) +
    ggplot2::labs(
      title = glue::glue("Actual vs Fitted Values for '{variable}' by Regime"),
      subtitle = glue::glue("R² (low): {round(r2_low, 3)}, R² (high): {round(r2_high, 3)}"),
      x = "Actual",
      y = "Fitted"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "none")
  
  # Optional residual density plots
  if (show_residuals) {
    resid_plot <- ggplot2::ggplot(df, ggplot2::aes(x = residual, fill = regime)) +
      ggplot2::geom_density(alpha = 0.6) +
      ggplot2::facet_wrap(~ regime, scales = "free") +
      ggplot2::labs(
        title = "Residual Density by Regime",
        x = "Residual",
        y = "Density"
      ) +
      ggplot2::scale_fill_manual(values = c("low" = "steelblue", "high" = "tomato")) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(legend.position = "none")
    
    final_plot <- patchwork::wrap_plots(base_plot, resid_plot, ncol = 1)
  } else {
    final_plot <- base_plot
  }
  
  # Save or print
  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, final_plot, width = 9, height = if (show_residuals) 8 else 5.5)
    if (verbose) message(glue::glue("Saved fitted vs actual plot to: {save_path}"))
  } else {
    print(final_plot)
  }
  
  invisible(final_plot)
}