#' Generate Missingness Heatmap
#'
#' @param df A long-format tibble with columns: date, commodity, price
#' @param save_plot_path Optional path to save plot as PNG
#' @param verbose Whether to print plot info
#' @export
generate_missingness_heatmap <- function(df, save_plot_path = NULL, verbose = TRUE) {
  stopifnot(all(c("date", "commodity", "price") %in% names(df)))
  
  # Create missingness matrix
  df_missing <- df |>
    dplyr::mutate(missing = is.na(price)) |>
    dplyr::group_by(commodity, date) |>
    dplyr::summarise(missing = any(missing), .groups = "drop")
  
  # Plot heatmap
  p <- ggplot2::ggplot(df_missing, ggplot2::aes(x = date, y = commodity, fill = missing)) +
    ggplot2::geom_tile(color = NA) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "red", `FALSE` = "white")) +
    ggplot2::labs(title = "Missingness Heatmap", x = "Date", y = "Commodity", fill = "Missing") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 6))
  
  if (!is.null(save_plot_path)) {
    fs::dir_create(fs::path_dir(save_plot_path))
    ggplot2::ggsave(save_plot_path, plot = p, width = 11, height = 6)
    if (verbose) cat("Saved missingness heatmap to:", save_plot_path, "\n")
  } else {
    print(p)
  }
  
  invisible(TRUE)
}
