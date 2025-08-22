#' Stacked area: contributions over horizon (per scenario × variable)
#'
#' @param contrib_tbl tibble: scenario, shock_id, regime, variable, t, contribution
#' @param scenario character or NULL
#' @param variable character or NULL
#' @param include_residual logical
#' @param smooth logical: apply light geom_line over the stack for readability
#' @param save_path optional PNG path
#' @param width,height,dpi numeric
#'
#' @return ggplot
plot_decomp_stacked_area <- function(
    contrib_tbl,
    scenario = NULL,
    variable = NULL,
    include_residual = TRUE,
    smooth = TRUE,
    save_path = NULL,
    width = 10, height = 6, dpi = 220
) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("fs", quietly = TRUE)
  
  df <- contrib_tbl
  if (!is.null(scenario)) df <- dplyr::filter(df, .data$scenario %in% scenario)
  if (!is.null(variable)) df <- dplyr::filter(df, .data$variable %in% variable)
  if (!include_residual)   df <- dplyr::filter(df, .data$shock_id != "residual")
  
  # aggregate over regimes to keep plot readable
  agg <- df %>%
    dplyr::group_by(.data$scenario, .data$variable, .data$shock_id, .data$t) %>%
    dplyr::summarise(contribution = sum(.data$contribution, na.rm = TRUE), .groups = "drop")
  
  p <- ggplot2::ggplot(agg, ggplot2::aes(x = .data$t, y = .data$contribution, fill = .data$shock_id)) +
    ggplot2::geom_area(position = "stack", alpha = 0.85) +
    ggplot2::facet_grid(variable ~ scenario, scales = "free_y") +
    ggplot2::labs(
      title = "Forecast Decomposition – Stacked Contributions over Horizon",
      x = "Horizon (t)",
      y = "Contribution"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
  
  if (smooth) {
    p <- p + ggplot2::geom_line(ggplot2::aes(group = 1), linewidth = 0.25, color = "black", alpha = 0.6)
  }
  
  if (!is.null(save_path)) {
    fs::dir_create(dirname(save_path))
    ggplot2::ggsave(save_path, p, width = width, height = height, dpi = dpi)
    message("[diag] wrote stacked-area: ", fs::path_rel(save_path))
    return(invisible(p))
  }
  p
}
