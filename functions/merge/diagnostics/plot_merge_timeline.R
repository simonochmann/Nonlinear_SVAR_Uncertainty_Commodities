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
                                start = NULL,
                                end   = NULL,
                                scale = c("none","z","index100","minmax"),
                                overlap_only = FALSE,
                                save_path = NULL,
                                show = TRUE) {
  scale <- match.arg(scale)
  
  stopifnot(is.list(index_list), length(index_list) >= 1)
  
  # Build tidy df strictly from what we were passed
  tidy <- purrr::imap_dfr(index_list, function(df, nm) {
    stopifnot(is.data.frame(df), "date" %in% names(df), nm %in% names(df))
    tibble::tibble(index = nm,
                   date  = as.Date(df$date),
                   value = as.numeric(df[[nm]]))
  }) |> dplyr::filter(!is.na(value))
  
  # explicit window
  if (!is.null(start)) tidy <- dplyr::filter(tidy, date >= as.Date(start))
  if (!is.null(end))   tidy <- dplyr::filter(tidy, date <= as.Date(end))
  
  # Limit to common overlap if requested
  if (overlap_only && n_distinct(tidy$index) > 1) {
    rng <- tidy |>
      dplyr::group_by(index) |>
      dplyr::summarise(dmin = min(date), dmax = max(date), .groups = "drop")
    ov_start <- max(rng$dmin); ov_end <- min(rng$dmax)
    tidy <- dplyr::filter(tidy, date >= ov_start, date <= ov_end)
  }
  
  # Scale
  scaled <- dplyr::group_by(tidy, index)
  
  if (scale == "z") {
    scaled <- dplyr::mutate(
      scaled,
      m = mean(value, na.rm = TRUE),
      s = stats::sd(value, na.rm = TRUE),
      value_plot = dplyr::if_else(is.finite(s) & s > 0, (value - m)/s, NA_real_)
    )
  } else if (scale == "index100") {
    scaled <- dplyr::arrange(scaled, date) |>
      dplyr::mutate(base = dplyr::first(value[!is.na(value)]),
                    value_plot = dplyr::if_else(is.finite(base) & base != 0,
                                                100 * value / base, NA_real_))
  } else if (scale == "minmax") {
    scaled <- dplyr::mutate(
      scaled,
      lo = min(value, na.rm = TRUE),
      hi = max(value, na.rm = TRUE),
      rng = hi - lo,
      value_plot = dplyr::if_else(is.finite(rng) & rng > 0, (value - lo)/rng, NA_real_)
    )
  } else {
    scaled <- dplyr::mutate(scaled, value_plot = value)
  }
  
  scaled <- dplyr::ungroup(scaled)
  
  # Sanity print so we see what’s plotted
  rng <- range(scaled$date, na.rm = TRUE)
  message(sprintf("plot_merge_timeline(): %s → %s | n=%d | scale=%s | indices: %s",
                  format(rng[1]), format(rng[2]), nrow(scaled), scale,
                  paste(unique(scaled$index), collapse = ", ")))
  
  # Quick check: show head stats to ensure not constant
  suppressMessages({
    dbg <- scaled |>
      dplyr::group_by(index) |>
      dplyr::summarise(var_plot = stats::var(value_plot, na.rm = TRUE), .groups = "drop")
    print(dbg)
  })
  
  p <- ggplot2::ggplot(scaled, ggplot2::aes(date, value_plot, color = index)) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::labs(x = NULL,
                  y = if (scale == "none") NULL else paste0("scaled (", scale, ")"),
                  title = "Uncertainty index timeline") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
  
  if (!is.null(save_path)) {
    dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(save_path, p, width = 9, height = 4, dpi = 150)
    message("Saved timeline plot to: ", save_path)
  }
  
  if (isTRUE(show)) print(p)
  invisible(p)
}