plot_regime_overlay_timeseries <- function(y, dates, regime_index, title = NULL, save_path = NULL) {
  if (is.null(y) || is.null(regime_index)) return(invisible(NULL))
  if (is.null(dates)) dates <- seq_along(y)
  df <- data.frame(t = dates, y = as.numeric(y),
                   regime = factor(regime_index, levels = c(1,2), labels = c("low","high")))
  p <- ggplot2::ggplot(df, ggplot2::aes(t, y)) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_rect(data = df[df$regime == "high", ],
                       ggplot2::aes(xmin = t, xmax = dplyr::lead(t, default = max(t)), ymin = -Inf, ymax = Inf),
                       inherit.aes = FALSE, alpha = 0.08) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 12)
  if (!is.null(save_path)) { fs::dir_create(dirname(save_path)); ggplot2::ggsave(save_path, p, width = 10, height = 3.5, dpi = 300) }
  invisible(p)
}
