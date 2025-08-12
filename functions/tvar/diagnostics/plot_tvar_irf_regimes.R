plot_tvar_irf_regimes <- function(
    model,
    impulse,
    response,
    horizon = 10,
    ci_level = 0.95,
    save_path = NULL,
    title = NULL,
    subtitle = NULL,
    colors = c("low" = "#1f77b4", "high" = "#d62728"),
    verbose = FALSE,
    return_data = FALSE
) {
  # Validate input
  stopifnot("irf" %in% names(model))
  stopifnot(all(c("low", "high") %in% names(model$irf)))
  stopifnot(is.character(impulse), is.character(response))
  
  # Extract IRF arrays
  get_irf_df <- function(regime) {
    irf_array <- model$irf[[regime]]
    if (!(impulse %in% names(irf_array))) {
      stop(glue::glue("Impulse variable '{impulse}' not found in regime '{regime}' IRFs."))
    }
    if (!(response %in% dimnames(irf_array[[impulse]])[[2]])) {
      stop(glue::glue("Response variable '{response}' not found in impulse '{impulse}' IRF for regime '{regime}'."))
    }
    draws <- irf_array[[impulse]][, response, 1:horizon, drop = FALSE]
    irf_df <- tibble::tibble(value = as.vector(draws)) |>
      dplyr::mutate(h = rep(seq_len(horizon), each = dim(draws)[1]),
                    regime = regime)
    return(irf_df)
  }
  
  df_irf <- dplyr::bind_rows(get_irf_df("low"), get_irf_df("high"))
  
  # Summarize IRF draws
  df_summary <- df_irf |>
    dplyr::group_by(regime, h) |>
    dplyr::summarise(
      lower = quantile(value, probs = (1 - ci_level)/2, na.rm = TRUE),
      upper = quantile(value, probs = 1 - (1 - ci_level)/2, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Plot
  p <- ggplot2::ggplot(df_summary, ggplot2::aes(x = h, y = median, fill = regime, color = regime)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::facet_wrap(~ regime, scales = "free_y") +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(
      title = title %||% glue::glue("IRF: '{impulse}' Shock → '{response}' Response by Regime"),
      subtitle = subtitle %||% glue::glue("{round(ci_level * 100)}% CI over {horizon} horizons"),
      x = "Horizon",
      y = "Impulse Response"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(legend.position = "none")
  
  if (!is.null(save_path)) {
    ggplot2::ggsave(filename = save_path, plot = p, width = 9, height = 5.5)
    if (verbose) message(glue::glue("Saved IRF regime plot to: {save_path}"))
  } else {
    print(p)
  }
  
  if (return_data) return(df_summary)
  invisible(p)
}
