# functions/decomposition/diagnostics/plot_fevd_stacked_area.R
# Stacked FEVD shares by impulse, faceted by response × regime (and uncertainty if present)

plot_fevd_stacked_area <- function(fevd_tbl, save_path, dpi = 220) {
  suppressPackageStartupMessages({
    library(dplyr); library(ggplot2); library(fs)
  })
  
  df <- fevd_tbl
  
  # --- Standardize column names -------------------------------------------------
  # Accept either 'response' or 'variable' for the response dimension
  if (!"response" %in% names(df) && "variable" %in% names(df)) {
    df <- dplyr::rename(df, response = .data$variable)
  }
  
  needed <- c("response", "impulse", "t", "fevd_share")
  if (!all(needed %in% names(df))) {
    stop("[plot_fevd_stacked_area] FEVD table must contain: ",
         paste(needed, collapse = ", "),
         ". Got: ", paste(names(df), collapse = ", "))
  }
  if (!"regime" %in% names(df)) df$regime <- "combined"
  
  # Types & safety
  df <- df %>%
    mutate(
      response   = as.character(.data$response),
      impulse    = as.character(.data$impulse),
      regime     = as.character(.data$regime),
      t          = as.integer(.data$t),
      fevd_share = as.numeric(.data$fevd_share)
    )
  
  has_unc <- "uncertainty" %in% names(df)
  
  # --- Normalize shares per group (no joins; avoids many-to-many warnings) -----
  group_keys <- c(if (has_unc) "uncertainty", "regime", "response", "t")
  df <- df %>%
    group_by(across(all_of(group_keys))) %>%
    mutate(total = sum(.data$fevd_share, na.rm = TRUE),
           share = dplyr::if_else(.data$total > 0, .data$fevd_share / .data$total, 0)) %>%
    ungroup()
  
  # --- Plot --------------------------------------------------------------------
  facet_formula <- if (has_unc) response ~ uncertainty + regime else response ~ regime
  
  p <- ggplot(df, aes(x = .data$t, y = .data$share, fill = .data$impulse)) +
    geom_area(position = "stack", alpha = 0.95) +
    facet_grid(facet_formula, scales = "free_y") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title = "FEVD shares (stacked)",
         x = "Horizon",
         y = "Share of variance explained",
         fill = "Impulse (shock)") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  
  fs::dir_create(fs::path_dir(save_path))
  ggplot2::ggsave(save_path, p, width = 14, height = 9, dpi = dpi)
  message("[diag] wrote FEVD stacked area: ", fs::path_rel(save_path))
  invisible(save_path)
}
