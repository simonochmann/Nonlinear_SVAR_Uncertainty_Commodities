#' Waterfall: contributions at a selected horizon (per scenario × variable)
#'
#' @param contrib_tbl tibble: scenario, shock_id, regime, variable, t, contribution
#' @param horizon integer (t) to visualize. If NULL, uses max(t).
#' @param scenario character or NULL: filter to a specific scenario (NULL = all)
#' @param variable character or NULL: filter to specific variable(s)
#' @param top_n integer: keep top_n shocks by absolute contribution (per facet). NULL = all
#' @param include_residual logical: keep "residual" bar if present
#' @param save_path optional file path to write PNG
#' @param width,height,dpi numeric: image params for ggsave
#'
#' @return ggplot object (invisibly if saved)
plot_decomp_waterfall <- function(
    contrib_tbl,
    horizon       = NULL,
    scenario      = NULL,
    variable      = NULL,
    top_n         = 8,
    include_residual = TRUE,
    save_path     = NULL,
    width = 9, height = 6, dpi = 220
) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("forcats", quietly = TRUE)
  
  df <- contrib_tbl
  
  # filters
  if (!is.null(scenario)) df <- dplyr::filter(df, .data$scenario %in% scenario)
  if (!is.null(variable)) df <- dplyr::filter(df, .data$variable %in% variable)
  
  if (is.null(horizon)) horizon <- suppressWarnings(max(df$t, na.rm = TRUE))
  df <- dplyr::filter(df, .data$t == horizon)
  
  if (!include_residual) df <- dplyr::filter(df, .data$shock_id != "residual")
  
  # aggregate over regimes at the chosen horizon
  agg <- df %>%
    dplyr::group_by(.data$scenario, .data$variable, .data$shock_id) %>%
    dplyr::summarise(contribution = sum(.data$contribution, na.rm = TRUE), .groups = "drop")
  
  # select top_n by |contribution| per (scenario, variable)
  if (!is.null(top_n)) {
    agg <- agg %>%
      dplyr::group_by(.data$scenario, .data$variable) %>%
      dplyr::slice_max(order_by = abs(.data$contribution), n = top_n, with_ties = FALSE) %>%
      dplyr::ungroup()
  }
  
  # order shocks by contribution sign/size within facet
  agg <- agg %>%
    dplyr::group_by(.data$scenario, .data$variable) %>%
    dplyr::mutate(shock_id = forcats::fct_reorder(.data$shock_id, .data$contribution)) %>%
    dplyr::ungroup()
  
  p <- ggplot2::ggplot(agg, ggplot2::aes(x = .data$shock_id, y = .data$contribution, fill = .data$shock_id)) +
    ggplot2::geom_col() +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
    ggplot2::facet_grid(variable ~ scenario, scales = "free_y") +
    ggplot2::labs(
      title = paste0("Forecast Decomposition – Waterfall at t = ", horizon),
      x = "Shock / Component",
      y = "Contribution"
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
    message("[diag] wrote waterfall: ", fs::path_rel(save_path))
    return(invisible(p))
  }
  p
}
