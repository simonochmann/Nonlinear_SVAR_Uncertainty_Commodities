# functions/decomposition/diagnostics/plot_decomp-stacked_area.R
plot_decomp_stacked_area <- function(
    contrib_tbl,
    scenario = NULL,
    variable = NULL,
    include_residual = TRUE,
    smooth = TRUE,
    top_k = 8,
    variable_top_k = 12,
    units = c("auto","raw","percent","bps","ppm"),
    save_path = NULL,
    width = 11, height = 7, dpi = 400
) {
  units <- match.arg(units)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("fs", quietly = TRUE)
  
  df <- contrib_tbl
  if (!is.null(scenario)) df <- dplyr::filter(df, .data$scenario %in% scenario)
  if (!is.null(variable)) df <- dplyr::filter(df, .data$variable %in% variable)
  if (!include_residual)  df <- dplyr::filter(df, .data$shock_id != "residual")
  
  agg <- df %>%
    dplyr::group_by(.data$scenario, .data$variable, .data$shock_id, .data$t) %>%
    dplyr::summarise(contribution = sum(.data$contribution, na.rm = TRUE), .groups = "drop")
  
  # top response variables per scenario (by L1 over t × shocks)
  if (is.null(variable)) {
    top_vars <- agg %>%
      dplyr::group_by(.data$scenario, .data$variable) %>%
      dplyr::summarise(imp = sum(abs(.data$contribution), na.rm = TRUE), .groups = "drop") %>%
      dplyr::group_by(.data$scenario) %>%
      dplyr::slice_max(order_by = .data$imp, n = variable_top_k, with_ties = FALSE) %>%
      dplyr::ungroup()
    # order variables by impact per scenario so the most important appear first
    agg <- agg %>%
      dplyr::inner_join(top_vars, by = c("scenario","variable")) %>%
      dplyr::group_by(.data$scenario) %>%
      dplyr::mutate(variable = forcats::fct_reorder(.data$variable, .data$imp, .desc = TRUE)) %>%
      dplyr::ungroup()
  }
  
  # Top‑K shocks per facet (tail -> "Other")
  top_ids <- agg %>%
    dplyr::group_by(.data$scenario, .data$variable, .data$shock_id) %>%
    dplyr::summarise(imp_s = sum(abs(.data$contribution), na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(.data$scenario, .data$variable) %>%
    dplyr::slice_max(order_by = .data$imp_s, n = top_k, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(in_top = TRUE) %>%
    dplyr::select(.data$scenario, .data$variable, .data$shock_id, .data$in_top)
  
  agg <- agg %>%
    dplyr::left_join(top_ids, by = c("scenario","variable","shock_id")) %>%
    dplyr::mutate(shock_group = dplyr::if_else(!is.na(.data$in_top), .data$shock_id, "Other")) %>%
    dplyr::select(-.data$in_top)
  
  sc <- auto_units(agg$contribution, mode = units)
  agg <- dplyr::mutate(agg, value_scaled = .data$contribution * sc$scale)
  
  net <- agg %>%
    dplyr::group_by(.data$scenario, .data$variable, .data$t) %>%
    dplyr::summarise(net = sum(.data$value_scaled, na.rm = TRUE), .groups = "drop")
  
  p <- ggplot2::ggplot(agg, ggplot2::aes(x = .data$t, y = .data$value_scaled, fill = .data$shock_group)) +
    ggplot2::geom_area(position = "stack", alpha = 0.95, na.rm = TRUE) +
    ggplot2::geom_line(data = net, ggplot2::aes(x = .data$t, y = .data$net, group = 1),
                       inherit.aes = FALSE, linewidth = 0.55, colour = "black", alpha = 0.7) +
    ggplot2::facet_grid(variable ~ scenario, scales = "free_y") +
    ggplot2::scale_y_continuous(labels = sc$lab, breaks = scales::breaks_pretty(n = 3)) +
    ggplot2::labs(
      title = "Forecast decomposition — stacked contributions over horizon",
      x = "Horizon (t)",
      y = if (sc$suffix == "") "Contribution" else paste0("Contribution (", sc$suffix, ")"),
      fill = "Shock"
    ) +
    theme_nlsvar() +
    ggplot2::theme(legend.position = "bottom")
  
  if (!is.null(save_path)) {
    save_png(p, save_path, width = width, height = height, dpi = dpi)
    message("[diag] wrote stacked-area: ", fs::path_rel(save_path))
    return(invisible(p))
  }
  p
}