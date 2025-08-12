check_ciss_coverage <- function(df, save_plot_path = NULL) {
  suppressPackageStartupMessages({ library(ggplot2) })
  g <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = ciss)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = "ECB CISS (Monthly)", x = NULL, y = "Index")
  if (!is.null(save_plot_path)) {
    dir.create(dirname(save_plot_path), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(save_plot_path, g, width = 7, height = 3.5, dpi = 150)
    cat(" Saved timeline plot:", save_plot_path, "\n")
  }
  invisible(TRUE)
}
