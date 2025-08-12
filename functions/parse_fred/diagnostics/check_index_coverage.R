check_index_coverage <- function(df, save_plot_path = NULL) {
  suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(tidyr) })
  covg <- df %>% summarise(across(-date, ~ mean(!is.na(.x)))) %>% pivot_longer(everything())
  g <- ggplot(covg, aes(x = reorder(name, value), y = value)) +
    geom_col() + coord_flip() +
    labs(x = "Index", y = "Coverage (share non-NA)", title = "Uncertainty Index Coverage")
  if (!is.null(save_plot_path)) {
    dir.create(dirname(save_plot_path), recursive = TRUE, showWarnings = FALSE)
    ggsave(save_plot_path, g, width = 6, height = 4, dpi = 150)
    cat(" Saved coverage plot:", save_plot_path, "\n")
  }
  invisible(covg)
}
