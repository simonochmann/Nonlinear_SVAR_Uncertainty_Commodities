#' Bar chart: low vs high regime shares of Δ (per scenario × variable)
#'
#' @param delta_tbl tibble: scenario, regime, variable, t, delta
#' @param metric "abs" (L1 share; default) or "signed" (signed sums)
#' @param scenario,variable optional filters
#' @param units_y either "percent" (default) or "raw" if you compute non-normalised values
#' @return ggplot
plot_regime_share_bars <- function(
    delta_tbl,
    metric = c("abs","signed"),
    scenario = NULL,
    variable = NULL,
    save_path = NULL,
    width = 8, height = 5, dpi = 400
) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("fs", quietly = TRUE)
  
  metric <- match.arg(metric)
  df <- delta_tbl
  if (!is.null(scenario)) df <- dplyr::filter(df, .data$scenario %in% scenario)
  if (!is.null(variable)) df <- dplyr::filter(df, .data$variable %in% variable)
  
  # if only one regime, skip (not meaningful)
  if (dplyr::n_distinct(df$regime) < 2) {
    message("[diag] skipped regime-shares: single regime present.")
    if (!is.null(save_path)) {
      # create an empty placeholder with a note (optional)
      gr <- ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = 0,
                                                  label = "Regime shares skipped: only 'combined' regime present.",
                                                  size = 4) + ggplot2::theme_void()
      save_png(gr, save_path, width = width, height = height, dpi = dpi)
    }
    return(invisible(NULL))
  }
  
  # compute shares (uses repo’s decompose_by_regime if present)
  if (!exists("decompose_by_regime", mode = "function")) {
    decompose_by_regime <- function(x, metric) {
      g <- x %>% dplyr::group_by(.data$scenario, .data$variable, .data$regime)
      vals <- if (metric == "abs") g %>% dplyr::summarise(v = sum(abs(.data$delta), na.rm = TRUE), .groups = "drop")
      else                  g %>% dplyr::summarise(v = sum(.data$delta,     na.rm = TRUE), .groups = "drop")
      vals %>%
        dplyr::group_by(.data$scenario, .data$variable) %>%
        dplyr::mutate(share = dplyr::if_else(sum(.data$v, na.rm = TRUE) > 0, .data$v/sum(.data$v, na.rm = TRUE), 0)) %>%
        dplyr::ungroup()
    }
  }
  shares <- decompose_by_regime(df, metric = metric)
  
  p <- ggplot2::ggplot(shares, ggplot2::aes(x = .data$regime, y = .data$share, fill = .data$regime)) +
    ggplot2::geom_col(width = 0.75, colour = "grey30", linewidth = 0.2) +
    ggplot2::facet_grid(variable ~ scenario) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
    ggplot2::labs(
      title = paste0("Regime shares of Δ (", metric, ")"),
      x = NULL, y = "Share of Δ"
    ) +
    theme_nlsvar() + ggplot2::theme(legend.position = "none") +
    ggplot2::coord_flip()
  
  if (!is.null(save_path)) {
    save_png(p, save_path, width = width, height = height, dpi = dpi)
    message("[diag] wrote regime-shares: ", fs::path_rel(save_path))
    return(invisible(p))
  }
  p
}