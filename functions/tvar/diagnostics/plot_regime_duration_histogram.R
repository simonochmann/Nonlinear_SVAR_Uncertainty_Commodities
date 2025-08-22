#' Duration histograms + overlaid geometric mean (theoretical) per regime
#' @export
plot_regime_duration_histogram <- function(spells, save_path = NULL) {
  if (is.null(spells) || !nrow(spells)) return(invisible(NULL))
  df <- spells
  df$regime <- factor(df$regime, levels = c("low","high"))
  
  p <- ggplot2::ggplot(df, ggplot2::aes(duration)) +
    ggplot2::geom_histogram(bins = 20, alpha = 0.6) +
    ggplot2::facet_wrap(~ regime, nrow = 1, scales = "free_y") +
    ggplot2::labs(title = "Regime spell durations", x = "Periods", y = "Count") +
    ggplot2::theme_minimal(base_size = 12)
  
  if (!is.null(save_path)) {
    fs::dir_create(dirname(save_path))
    ggplot2::ggsave(save_path, p, width = 9.5, height = 4.2, dpi = 300)
  }
  invisible(p)
}