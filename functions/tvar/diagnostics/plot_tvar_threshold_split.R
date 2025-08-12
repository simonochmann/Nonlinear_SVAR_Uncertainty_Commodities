#' Dual-View Threshold Distribution Diagnostics for TVAR
#'
#' Generates an elite diagnostic plot for the threshold variable: split histogram/density
#' and regime-wise violin/boxplot with skewness/kurtosis annotations.
#'
#' @param df A data.frame or tibble containing the threshold variable.
#' @param regime_var Character. Name of the variable used as threshold.
#' @param threshold Numeric. Threshold value used to split regimes.
#' @param threshold_percentile Optional. Percentile label for the threshold (e.g., "50%").
#' @param bins Histogram bins. Default is "auto".
#' @param log_stats Logical. If TRUE, prints skewness and kurtosis by regime.
#' @param show_density Logical. If TRUE, overlays density curves.
#' @param show_rug Logical. If TRUE, adds rug plot.
#' @param save_path Optional path to save the plot (PDF/PNG).
#'
#' @return Invisibly returns a list with ggplot object and regime stats.
#' @export
plot_tvar_threshold_split <- function(
    df,
    regime_var,
    threshold,
    threshold_percentile = NULL,
    bins = "auto",
    log_stats = TRUE,
    show_density = TRUE,
    show_rug = TRUE,
    save_path = NULL
) {
  stopifnot("data.frame" %in% class(df))
  stopifnot(regime_var %in% colnames(df))
  stopifnot(is.numeric(threshold))
  
  # ---- Preprocess ----
  df <- df |>
    dplyr::mutate(
      regime = ifelse(.data[[regime_var]] <= threshold, "low", "high") |>
        factor(levels = c("low", "high"))
    )
  
  # ---- Skewness & Kurtosis ----
  regime_stats <- df |>
    dplyr::group_by(regime) |>
    dplyr::summarise(
      skewness = round(moments::skewness(.data[[regime_var]]), 3),
      kurtosis = round(moments::kurtosis(.data[[regime_var]]), 3),
      .groups = "drop"
    )
  
  if (log_stats) {
    cat("\n[Threshold Variable Diagnostics]\n")
    print(regime_stats)
  }
  
  # ---- Panel A: Histogram & Density ----
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[regime_var]], fill = regime)) +
    {
      if (bins == "auto") ggplot2::geom_histogram(alpha = 0.5, position = "identity")
      else ggplot2::geom_histogram(alpha = 0.5, position = "identity", bins = bins)
    } +
    {
      if (show_density) ggplot2::geom_density(alpha = 0.3, color = "black")
    } +
    {
      if (show_rug) ggplot2::geom_rug(alpha = 0.15)
    } +
    ggplot2::geom_vline(xintercept = threshold, linetype = "dashed", linewidth = 1.1) +
    ggplot2::annotate(
      "text",
      x = threshold,
      y = Inf,
      label = glue::glue("Threshold = {round(threshold, 4)}\n({threshold_percentile} percentile)"),
      vjust = 2,
      hjust = -0.05,
      size = 3.5,
      fontface = "italic"
    ) +
    ggplot2::scale_fill_manual(values = c("low" = "steelblue", "high" = "tomato")) +
    ggplot2::labs(
      title = glue::glue("Distribution of Threshold Variable: {regime_var}"),
      subtitle = glue::glue("Split on {regime_var} at {round(threshold, 4)}"),
      x = regime_var,
      y = "Frequency"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank())
  
  # ---- Panel B: Violin + Boxplot with Annotations
  p2 <- ggplot2::ggplot(df, ggplot2::aes(x = regime, y = .data[[regime_var]], fill = regime)) +
    ggplot2::geom_violin(trim = FALSE, alpha = 0.5) +
    ggplot2::geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.6) +
    ggplot2::scale_fill_manual(values = c("low" = "steelblue", "high" = "tomato")) +
    ggplot2::labs(
      title = "Regime Diagnostics: Skewness & Kurtosis",
      x = "Regime", y = regime_var
    ) +
    ggplot2::annotate(
      "text",
      x = c(1, 2),
      y = max(df[[regime_var]], na.rm = TRUE),
      label = paste0("Skew = ", regime_stats$skewness, "\nKurt = ", regime_stats$kurtosis),
      vjust = -0.5,
      size = 3.5
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "none")
  
  # ---- Combine Plots ----
  combined_plot <- patchwork::wrap_plots(p1, p2, ncol = 2)
  
  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, combined_plot, width = 11.5, height = 5.5)
    message(glue::glue("Saved diagnostic plot to: {save_path}"))
  } else {
    print(combined_plot)
  }
  
  return(invisible(list(
    plot = combined_plot,
    skew_kurtosis = regime_stats
  )))
}
