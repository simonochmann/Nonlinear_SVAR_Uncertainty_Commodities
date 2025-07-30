#' Plot Time Coverage of Cleaned Uncertainty Indices
#'
#' Creates a horizontal timeline plot of available data ranges per index,
#' with labeled start/end dates, optional core sample shading, and sample size metadata.
#'
#' @param index_list Named list of tibbles, each with `date` and `value`.
#' @param title Plot title.
#' @param core_range Optional vector of length 2 with start and end dates to highlight.
#'
#' @return A ggplot2 timeline plot (gg object).
#' @export
#'
#' @examples
#' plot_merge_timeline(list(ciss = df_ciss, vix = df_vix))
plot_merge_timeline <- function(index_list,
                                title = "Uncertainty Index Timeline",
                                core_range = c(as.Date("2000-01-01"), as.Date("2020-12-31"))) {
  stopifnot(is.list(index_list), !is.null(names(index_list)))
  
  # Extract metadata
  timeline_df <- purrr::imap_dfr(index_list, ~{
    tibble::tibble(
      index = .y,
      start = min(.x$date, na.rm = TRUE),
      end = max(.x$date, na.rm = TRUE),
      n = nrow(.x)
    )
  })
  
  # Add rich label
  timeline_df <- timeline_df |>
    dplyr::mutate(label = glue::glue("{index} (n={n})"))
  
  # Plot
  p <- ggplot2::ggplot(timeline_df, ggplot2::aes(y = label)) +
    # Optional shaded core range
    {
      if (!is.null(core_range)) ggplot2::annotate(
        "rect",
        xmin = core_range[1], xmax = core_range[2],
        ymin = -Inf, ymax = Inf,
        fill = "grey90", alpha = 0.4
      )
    } +
    # Timeline segments
    ggplot2::geom_segment(ggplot2::aes(x = start, xend = end, yend = label),
                          size = 3, color = "#4682B4") +
    # Start and end points
    ggplot2::geom_point(ggplot2::aes(x = start), size = 2.3, shape = 21, fill = "white", stroke = 1) +
    ggplot2::geom_point(ggplot2::aes(x = end), size = 2.3, shape = 21, fill = "white", stroke = 1) +
    # Start and end labels
    ggplot2::geom_text(ggplot2::aes(x = start, label = format(start, "%Y")),
                       hjust = 1.1, vjust = 0.4, size = 3.2, color = "black") +
    ggplot2::geom_text(ggplot2::aes(x = end, label = format(end, "%Y")),
                       hjust = -0.1, vjust = 0.4, size = 3.2, color = "black") +
    # Final polish
    ggplot2::labs(
      title = title,
      subtitle = glue::glue("Time coverage across {nrow(timeline_df)} indices"),
      x = "Date", y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      axis.text.y = ggplot2::element_text(face = "bold")
    )
  
  return(p)
}