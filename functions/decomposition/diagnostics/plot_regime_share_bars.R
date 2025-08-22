#' Bar chart: low vs high regime shares of Δ (per scenario × variable)
#'
#' @param delta_tbl tibble: scenario, regime, variable, t, delta
#' @param metric "abs" (L1 share; default) or "signed" (signed sums)
#' @param scenario character or NULL
#' @param variable character or NULL
#' @param save_path optional PNG path
#' @param width,height,dpi numeric
#'
#' @return ggplot
plot_regime_share_bars <- function(
    delta_tbl,
    metric = c("abs","signed"),
    scenario = NULL,
    variable = NULL,
    save_path = NULL,
    width = 8, height = 5, dpi = 220
) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("fs", quietly = TRUE)
  
  metric <- match.arg(metric)
  df <- delta_tbl
  if (!is.null(scenario)) df <- dplyr::filter(df, .data$scenario %in% scenario)
  if (!is.null(variable)) df <- dplyr::filter(df, .data$variable %in% variable)
  
  # compute shares using your module
  shares <- decompose_by_regime(df, metric = metric)
  
  p <- ggplot2::ggplot(shares, ggplot2::aes(x = regime, y = share, fill = regime)) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::facet_grid(variable ~ scenario) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      title = paste0("Regime Shares of Δ (", metric, ")"),
      x = "Regime",
      y = "Share of Δ"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
  
  if (!is.null(save_path)) {
    fs::dir_create(dirname(save_path))
    ggplot2::ggsave(save_path, p, width = width, height = height, dpi = dpi)
    message("[diag] wrote regime-shares: ", fs::path_rel(save_path))
    return(invisible(p))
  }
  p
}
