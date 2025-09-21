#' Stacked FEVD shares by impulse, faceted by response × regime (and uncertainty if present)
#' Keeps top-K impulses and top-M responses.
plot_fevd_stacked_area <- function(fevd_tbl, save_path, top_k = 8, top_responses = 12, dpi = 400) {
  suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(fs) })
  
  df <- fevd_tbl
  if (!"response" %in% names(df) && "variable" %in% names(df)) df <- dplyr::rename(df, response = .data$variable)
  needed <- c("response", "impulse", "t", "fevd_share")
  if (!all(needed %in% names(df))) stop("[plot_fevd_stacked_area] FEVD table must contain: response, impulse, t, fevd_share.")
  if (!"regime" %in% names(df)) df$regime <- "combined"
  has_unc <- "uncertainty" %in% names(df)
  
  df <- df %>%
    dplyr::mutate(response = as.character(.data$response),
                  impulse  = as.character(.data$impulse),
                  regime   = as.character(.data$regime),
                  t        = as.integer(.data$t),
                  fevd_share = as.numeric(.data$fevd_share))
  
  group_keys <- c(if (has_unc) "uncertainty", "regime", "response", "t")
  df <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_keys))) %>%
    dplyr::mutate(total = sum(.data$fevd_share, na.rm = TRUE),
                  share = dplyr::if_else(.data$total > 0, .data$fevd_share / .data$total, 0)) %>%
    dplyr::ungroup()
  
  # Top impulses (global) and top responses (most “volatile” by L1 share over t, impulses)
  top_imp <- df %>% dplyr::group_by(.data$impulse) %>%
    dplyr::summarise(imp = sum(.data$share, na.rm = TRUE), .groups = "drop") %>%
    dplyr::slice_max(order_by = .data$imp, n = top_k, with_ties = FALSE) %>% dplyr::pull(.data$impulse)
  
  top_resp <- df %>% dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(group_keys,"t")))) %>%
    dplyr::summarise(l1 = sum(abs(.data$share), na.rm = TRUE), .groups = "drop_last") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(group_keys,c("t","response"))))) %>%
    dplyr::slice_max(order_by = .data$l1, n = top_responses, with_ties = FALSE) %>%
    dplyr::ungroup() %>% dplyr::select(dplyr::all_of(setdiff(group_keys,"t")), .data$response)
  
  df <- df %>%
    dplyr::mutate(impulse_group = dplyr::if_else(.data$impulse %in% top_imp, .data$impulse, "Other")) %>%
    dplyr::inner_join(top_resp, by = intersect(names(df), names(top_resp)))
  
  facet_formula <- if (has_unc) response ~ uncertainty + regime else response ~ regime
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$t, y = .data$share, fill = .data$impulse_group)) +
    ggplot2::geom_area(position = "stack") +
    ggplot2::facet_grid(facet_formula, scales = "free_y") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(title = "FEVD shares (stacked)",
                  x = "Horizon",
                  y = "Share of variance explained",
                  fill = "Impulse") +
    theme_nlsvar()
  
  fs::dir_create(fs::path_dir(save_path))
  ggplot2::ggsave(save_path, p, width = 13.5, height = 8.8, dpi = dpi, bg = "white")
  message("[diag] wrote FEVD stacked area: ", fs::path_rel(save_path))
  invisible(save_path)
}