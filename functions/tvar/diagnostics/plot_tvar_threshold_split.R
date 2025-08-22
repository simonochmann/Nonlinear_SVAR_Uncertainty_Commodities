#' Dual-View Threshold Distribution Diagnostics for TVAR
#'
#' Generates a diagnostic plot for the threshold variable: (A) split histogram/density
#' with a vertical threshold line and (B) regime-wise violin/boxplot with skewness/kurtosis
#' annotations. Any "overall" statistics are printed but excluded from plot annotations
#' to avoid length mismatches.
#'
#' @param df A data.frame or tibble containing the threshold variable.
#' @param regime_var Character. Name of the variable used as threshold (column in `df`).
#' @param threshold Numeric. Threshold value used to split regimes.
#' @param threshold_percentile Optional. Percentile label for the threshold (e.g., "50%").
#' @param bins Histogram bins. Either "auto" (default) or a single integer.
#' @param log_stats Logical. If TRUE, prints skewness and kurtosis by regime (+ overall).
#' @param show_density Logical. If TRUE, overlays density curves.
#' @param show_rug Logical. If TRUE, adds a rug plot.
#' @param save_path Optional path to save the combined plot (PNG/PDF/etc.).
#'
#' @return Invisibly returns a list with the ggplot object and a list of stats:
#'   - $by_regime: tibble with regime-wise skewness/kurtosis (low/high)
#'   - $overall: tibble with overall skewness/kurtosis
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
  # ---- Preconditions ----
  stopifnot("data.frame" %in% class(df))
  stopifnot(is.character(regime_var), length(regime_var) == 1L)
  stopifnot(regime_var %in% colnames(df))
  stopifnot(is.numeric(threshold), length(threshold) == 1L, is.finite(threshold))
  
  # Lazy requires (for non-package context)
  requireNamespace("dplyr")
  requireNamespace("ggplot2")
  requireNamespace("moments")
  requireNamespace("glue")
  requireNamespace("patchwork")
  
  # ---- Preprocess & regime tagging ----
  df <- df |>
    dplyr::mutate(
      regime = ifelse(.data[[regime_var]] <= threshold, "low", "high"),
      regime = factor(regime, levels = c("low", "high"))
    )
  
  # ---- Skewness & Kurtosis ----
  # Regime-wise (used for BOTH printing and annotations)
  regime_stats <- df |>
    dplyr::group_by(regime) |>
    dplyr::summarise(
      skewness = suppressWarnings(round(moments::skewness(.data[[regime_var]], na.rm = TRUE), 3)),
      kurtosis = suppressWarnings(round(moments::kurtosis(.data[[regime_var]], na.rm = TRUE), 3)),
      .groups = "drop"
    )
  
  # Overall (printed only; NOT used for plot annotations)
  overall_stats <- df |>
    dplyr::summarise(
      skewness = suppressWarnings(round(moments::skewness(.data[[regime_var]], na.rm = TRUE), 3)),
      kurtosis = suppressWarnings(round(moments::kurtosis(.data[[regime_var]], na.rm = TRUE), 3))
    ) |>
    dplyr::mutate(regime = factor(NA_character_, levels = c("low", "high"))) |>
    dplyr::relocate(regime)
  
  if (isTRUE(log_stats)) {
    cat("\n[Threshold Variable Diagnostics]\n")
    # Bind rows for console display only (regimes + overall NA row)
    print(dplyr::bind_rows(regime_stats, overall_stats))
  }
  
  # ---- Panel A: Histogram & Density ----
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[regime_var]], fill = regime))
  
  if (identical(bins, "auto")) {
    p1 <- p1 + ggplot2::geom_histogram(alpha = 0.5, position = "identity")
  } else {
    stopifnot(is.numeric(bins), length(bins) == 1L, is.finite(bins), bins > 0)
    p1 <- p1 + ggplot2::geom_histogram(alpha = 0.5, position = "identity", bins = bins)
  }
  
  if (isTRUE(show_density)) {
    p1 <- p1 + ggplot2::geom_density(alpha = 0.3, color = "black")
  }
  if (isTRUE(show_rug)) {
    p1 <- p1 + ggplot2::geom_rug(alpha = 0.15)
  }
  
  # Threshold label
  thr_lab <- if (!is.null(threshold_percentile) && nzchar(threshold_percentile)) {
    glue::glue("Threshold = {round(threshold, 4)}\n({threshold_percentile} percentile)")
  } else {
    glue::glue("Threshold = {round(threshold, 4)}")
  }
  
  p1 <- p1 +
    ggplot2::geom_vline(xintercept = threshold, linetype = "dashed", linewidth = 1.1) +
    ggplot2::annotate("text",
                      x = threshold, y = Inf, label = thr_lab,
                      vjust = 1.5, hjust = -0.05, size = 3.5, fontface = "italic") +
    ggplot2::scale_fill_manual(values = c("low" = "steelblue", "high" = "tomato")) +
    ggplot2::labs(
      title = glue::glue("Distribution of Threshold Variable: {regime_var}"),
      subtitle = glue::glue("Split on {regime_var} at {round(threshold, 4)}"),
      x = regime_var,
      y = "Frequency"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank())
  
  # ---- Panel B: Violin + Boxplot with Annotations ----
  # Only use *present* regimes for annotation to keep x/label lengths equal
  reg_stats_plot <- regime_stats |>
    dplyr::filter(!is.na(regime))
  
  # If, for some reason, only one regime is present (extreme threshold), we still annotate it.
  ann_x <- as.numeric(reg_stats_plot$regime)   # 1 for "low", 2 for "high" (matching x = regime)
  ann_lab <- paste0("Skew = ", reg_stats_plot$skewness,
                    "\nKurt = ", reg_stats_plot$kurtosis)
  
  # Vertical position near the top
  y_top <- max(df[[regime_var]], na.rm = TRUE)
  
  p2 <- ggplot2::ggplot(df, ggplot2::aes(x = regime, y = .data[[regime_var]], fill = regime)) +
    ggplot2::geom_violin(trim = FALSE, alpha = 0.5) +
    ggplot2::geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.6) +
    ggplot2::scale_fill_manual(values = c("low" = "steelblue", "high" = "tomato")) +
    ggplot2::labs(
      title = "Regime Diagnostics: Skewness & Kurtosis",
      x = "Regime", y = regime_var
    ) +
    {
      # Only add annotation if we actually have labels (guards against empty df)
      if (length(ann_x) > 0L) {
        ggplot2::annotate(
          "text",
          x = ann_x,
          y = rep(y_top, length(ann_x)),
          label = ann_lab,
          vjust = -0.5,
          size = 3.5
        )
      }
    } +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "none")
  
  # ---- Combine Plots ----
  combined_plot <- patchwork::wrap_plots(p1, p2, ncol = 2)
  
  if (!is.null(save_path) && nzchar(save_path)) {
    ggplot2::ggsave(save_path, combined_plot, width = 11.5, height = 5.5)
    message(glue::glue("Saved diagnostic plot to: {save_path}"))
  } else {
    print(combined_plot)
  }
  
  invisible(list(
    plot = combined_plot,
    stats = list(
      by_regime = regime_stats,
      overall   = overall_stats
    )
  ))
}
