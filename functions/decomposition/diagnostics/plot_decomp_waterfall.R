# functions/decomposition/diagnostics/plot_decomp_waterfall.R

#' Waterfall / Lollipop: contributions at a selected horizon (scenario × variable facets)
#' - For dense grids, switches to lollipop and labels only top-M shocks per facet (to avoid axis clutter).
#'
#' @param contrib_tbl tibble: scenario, shock_id, regime, variable, t, contribution
#' @param horizon integer or "auto_max_abs"/"auto_max_net" (default: auto_max_abs)
#' @param variable_top_k show top-K response variables per scenario (by L1 at chosen horizon)
#' @param top_n shocks per facet (tail -> "Other")
#' @param label_top label top-M shocks by name per facet (lollipop mode only)
#' @param style "auto" | "waterfall" | "lollipop"
#' @param units "auto","raw","percent","bps","ppm"
plot_decomp_waterfall <- function(
    contrib_tbl,
    horizon = NULL,
    scenario = NULL,
    variable = NULL,
    variable_top_k = 12,
    top_n = 8,
    label_top = 2,
    style = c("auto","waterfall","lollipop"),
    include_residual = TRUE,
    units = c("auto","raw","percent","bps","ppm"),
    save_path = NULL,
    width = 11, height = 7, dpi = 400
) {
  style <- match.arg(style)
  units <- match.arg(units)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("forcats", quietly = TRUE)
  requireNamespace("fs", quietly = TRUE)
  
  df <- contrib_tbl
  if (!is.null(scenario)) df <- dplyr::filter(df, .data$scenario %in% scenario)
  if (!is.null(variable)) df <- dplyr::filter(df, .data$variable %in% variable)
  if (!include_residual)  df <- dplyr::filter(df, .data$shock_id != "residual")
  
  # ---- pick horizon per facet --------------------------------------------------
  if (is.null(horizon)) horizon <- "auto_max_abs"
  if (is.character(horizon)) {
    sums <- df %>%
      dplyr::group_by(.data$scenario, .data$variable, .data$t) %>%
      dplyr::summarise(l1 = sum(abs(.data$contribution), na.rm = TRUE),
                       net = sum(.data$contribution, na.rm = TRUE), .groups = "drop")
    pick <- if (identical(horizon,"auto_max_net")) {
      sums %>% dplyr::group_by(.data$scenario, .data$variable) %>%
        dplyr::slice_max(order_by = abs(.data$net), n = 1, with_ties = FALSE) %>% dplyr::ungroup()
    } else {
      sums %>% dplyr::group_by(.data$scenario, .data$variable) %>%
        dplyr::slice_max(order_by = .data$l1, n = 1, with_ties = FALSE) %>% dplyr::ungroup()
    }
    df <- df %>% dplyr::inner_join(pick %>% dplyr::select(.data$scenario,.data$variable,.data$t),
                                   by = c("scenario","variable","t"))
  } else {
    df <- dplyr::filter(df, .data$t == horizon)
  }
  
  # ---- aggregate regimes & keep top-N shocks; tail -> "Other" ------------------
  agg0 <- df %>%
    dplyr::group_by(.data$scenario, .data$variable, .data$shock_id) %>%
    dplyr::summarise(contribution = sum(.data$contribution, na.rm = TRUE), .groups = "drop")
  
  keep <- agg0 %>%
    dplyr::group_by(.data$scenario, .data$variable) %>%
    dplyr::slice_max(order_by = abs(.data$contribution), n = top_n, with_ties = FALSE) %>%
    dplyr::ungroup()
  other <- dplyr::anti_join(agg0, keep, by = c("scenario","variable","shock_id")) %>%
    dplyr::group_by(.data$scenario, .data$variable) %>%
    dplyr::summarise(shock_id = "Other", contribution = sum(.data$contribution, na.rm = TRUE), .groups = "drop")
  agg <- dplyr::bind_rows(keep, other)
  
  # ---- limit to top-K response variables per scenario --------------------------
  if (is.null(variable)) {
    topv <- agg %>%
      dplyr::group_by(.data$scenario, .data$variable) %>%
      dplyr::summarise(l1 = sum(abs(.data$contribution), na.rm = TRUE), .groups = "drop") %>%
      dplyr::group_by(.data$scenario) %>%
      dplyr::slice_max(order_by = .data$l1, n = variable_top_k, with_ties = FALSE) %>%
      dplyr::ungroup()
    agg <- agg %>% dplyr::inner_join(topv %>% dplyr::select(.data$scenario,.data$variable),
                                     by = c("scenario","variable"))
  }
  
  # ---- units & helpers ---------------------------------------------------------
  sc <- auto_units(agg$contribution, mode = units)
  agg <- dplyr::mutate(agg, value = .data$contribution * sc$scale,
                       sign = dplyr::if_else(.data$value >= 0, "Positive", "Negative"),
                       shock_lab = wrap_lab(.data$shock_id, 16))
  
  # ---- decide geometry ---------------------------------------------------------
  n_vars <- dplyr::n_distinct(agg$variable)
  chosen_style <- if (style == "auto") if (n_vars <= 6) "waterfall" else "lollipop" else style
  
  if (chosen_style == "waterfall") {
    wf <- agg %>%
      dplyr::group_by(.data$scenario, .data$variable) %>%
      dplyr::arrange(.data$value, .by_group = TRUE) %>%
      dplyr::mutate(id = dplyr::row_number(),
                    y0 = dplyr::lag(cumsum(.data$value), default = 0),
                    y1 = .data$y0 + .data$value) %>%
      dplyr::ungroup()
    
    p <- ggplot2::ggplot(wf) +
      ggplot2::geom_rect(ggplot2::aes(xmin = .data$id - 0.48, xmax = .data$id + 0.48,
                                      ymin = .data$y0, ymax = .data$y1, fill = .data$sign),
                         colour = "grey30", linewidth = 0.25) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.35, colour = "grey40") +
      ggplot2::scale_x_continuous(breaks = wf$id, labels = wf$shock_lab, expand = c(0.02, 0.02)) +
      ggplot2::scale_y_continuous(labels = sc$lab, breaks = scales::breaks_pretty(n = 4)) +
      ggplot2::facet_grid(variable ~ scenario, scales = "free_y") +
      ggplot2::scale_fill_manual(values = c("Positive" = "#4C9F70", "Negative" = "#D95F5F")) +
      ggplot2::labs(
        title = sprintf("Forecast decomposition — waterfall at t = %s", if (is.numeric(horizon)) horizon else "auto"),
        x = NULL,
        y = if (sc$suffix == "") "Contribution" else paste0("Contribution (", sc$suffix, ")"),
        fill = NULL
      ) +
      ggplot2::coord_flip() +
      theme_nlsvar() +
      ggplot2::theme(legend.position = "bottom")
    
  } else { # ---- lollipop for dense facets, label only top-M --------------------
    # per-facet range (for label positioning)
    lims <- agg %>%
      dplyr::group_by(.data$scenario, .data$variable) %>%
      dplyr::summarise(xmax = max(abs(.data$value), na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(xpad = dplyr::if_else(is.finite(.data$xmax) & .data$xmax > 0, 0.06 * .data$xmax, 0.06))
    
    # label only top 'label_top' shocks per facet
    label_top <- max(0, min(label_top, top_n))
    labs_df <- if (label_top > 0) {
      agg %>%
        dplyr::group_by(.data$scenario, .data$variable) %>%
        dplyr::slice_max(order_by = abs(.data$value), n = label_top, with_ties = FALSE) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(lims, by = c("scenario","variable")) %>%
        dplyr::mutate(
          x_lab     = dplyr::if_else(.data$value >= 0, .data$value + .data$xpad, .data$value - .data$xpad),
          hjust_num = dplyr::if_else(.data$value >= 0, 0, 1)  # <-- numeric, not a unit()
        )
    } else {
      NULL
    }
    
    p <- ggplot2::ggplot(agg, ggplot2::aes(y = .data$shock_lab, x = .data$value, colour = .data$sign)) +
      ggplot2::geom_segment(ggplot2::aes(yend = .data$shock_lab, x = 0, xend = .data$value),
                            linewidth = 0.5, lineend = "round") +
      ggplot2::geom_point(size = 1.8) +
      ggplot2::geom_vline(xintercept = 0, linewidth = 0.35, colour = "grey40") +
      ggplot2::facet_grid(variable ~ scenario, scales = "free_x") +
      ggplot2::scale_x_continuous(
        labels = sc$lab,
        breaks = scales::breaks_pretty(n = 3),
        expand = ggplot2::expansion(mult = c(0.05, 0.15))  # room for labels
      ) +
      ggplot2::scale_colour_manual(values = c("Positive" = "#4C9F70", "Negative" = "#D95F5F")) +
      ggplot2::labs(
        title = sprintf("Forecast decomposition — contributions at t = %s",
                        if (is.numeric(horizon)) horizon else "auto"),
        x = if (sc$suffix == "") "Contribution" else paste0("Contribution (", sc$suffix, ")"),
        y = NULL, colour = NULL
      ) +
      theme_nlsvar() +
      ggplot2::theme(
        legend.position = "bottom",
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(5.5, 24, 5.5, 5.5)  # extra right margin
      ) +
      ggplot2::coord_cartesian(clip = "off")
    
    if (!is.null(labs_df) && nrow(labs_df)) {
      p <- p + ggplot2::geom_text(
        data = labs_df,
        ggplot2::aes(x = .data$x_lab, y = .data$shock_lab, label = .data$shock_id, hjust = .data$hjust_num),
        inherit.aes = FALSE, size = 2.8
      )
    }
  
  if (!is.null(save_path)) {
    save_png(p, save_path, width = width, height = height, dpi = dpi)
    message("[diag] wrote waterfall: ", fs::path_rel(save_path))
    return(invisible(p))
  }
  p
  }
}