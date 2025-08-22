# functions/scenarios/diagnostics/plot_scenario_paths.R

#' Plot baseline vs shocked scenario paths (optionally with CI bands)
#'
#' Expects a tidy tibble with columns at least:
#'   t, variable, value, scenario, is_baseline (logical), and optionally regime.
#'   If CI bands exist, columns like lo_90/hi_90 (or lo_95, hi_95, etc.).
#'
#' Produces one ggplot per (scenario × variable), faceting by regime if present.
#'
#' @param tidy Tibble with columns described above.
#' @param band_preference Numeric vector of CI coverages to prefer when multiple
#'   bands are present. Default c(0.90, 0.95, 0.80).
#' @param free_y Logical, facet scales free in y? Default FALSE.
#' @param show_points Logical, add small points on lines? Default FALSE.
#' @return Named list of ggplot objects keyed by "<scenario>_<variable>_paths".
#' @export
plot_scenario_paths <- function(
    tidy,
    band_preference = c(0.90, 0.95, 0.80),
    free_y = FALSE,
    show_points = FALSE
) {
  # ---- guards ----
  req <- c("t","variable","value","scenario","is_baseline")
  miss <- setdiff(req, names(tidy))
  if (length(miss)) stop("Input `tidy` missing columns: ", paste(miss, collapse=", "))
  
  # normalize types
  tidy$t <- as.integer(tidy$t)
  tidy$is_baseline <- as.logical(tidy$is_baseline)
  
  # helper: detect which lo_*/hi_* pair to use (if any)
  .detect_bands <- function(df) {
    cols <- names(df)
    lo_tags <- sub("^lo_", "", grep("^lo_", cols, value = TRUE))
    hi_tags <- sub("^hi_", "", grep("^hi_", cols, value = TRUE))
    tags <- intersect(lo_tags, hi_tags)
    if (!length(tags)) return(NULL)
    # map preferences (0.90 -> "90") and try in order
    pref_tags <- gsub("\\.", "", sprintf("%02.0f", 100 * band_preference))
    tag <- pref_tags[pref_tags %in% tags][1]
    if (is.na(tag)) tag <- tags[1]
    list(lo = paste0("lo_", tag), hi = paste0("hi_", tag), tag = tag)
  }
  
  # ensure deterministic ordering
  tidy <- tidy |>
    dplyr::arrange(scenario, variable, dplyr::across(dplyr::all_of(intersect("regime", names(tidy)))), t)
  
  # split by scenario × variable
  groups <- tidy |>
    dplyr::group_split(scenario, variable)
  
  out <- vector("list", length(groups))
  names(out) <- character(length(groups))
  
  for (i in seq_along(groups)) {
    df <- groups[[i]]
    scen <- df$scenario[1]
    var  <- df$variable[1]
    key  <- paste0(scen, "_", var, "_paths")
    
    # choose bands (if present) but only apply to shocked paths
    band_cols <- .detect_bands(df)
    
    df$is_baseline_lbl <- ifelse(df$is_baseline, "baseline", "shocked")
    df$is_baseline_lbl <- factor(df$is_baseline_lbl, levels = c("baseline","shocked"))
    
    p <- ggplot2::ggplot(df, ggplot2::aes(x = t, y = value,
                                          color = is_baseline_lbl,
                                          linetype = is_baseline_lbl,
                                          group = interaction(is_baseline_lbl, !!as.name(if ("regime" %in% names(df)) "regime" else "is_baseline_lbl"))))
    
    # ribbon for shocked path if bands exist
    if (!is.null(band_cols)) {
      df_shocked <- df[!df$is_baseline, , drop = FALSE]
      if (nrow(df_shocked) > 0 && all(band_cols %in% names(df_shocked))) {
        p <- p + ggplot2::geom_ribbon(
          data = df_shocked,
          ggplot2::aes(ymin = .data[[band_cols$lo]], ymax = .data[[band_cols$hi]], group = NULL),
          inherit.aes = FALSE,
          alpha = 0.20
        )
      }
    }
    
    p <- p +
      ggplot2::geom_line() +
      { if (show_points) ggplot2::geom_point(size = 0.8) else ggplot2::geom_blank() } +
      ggplot2::scale_color_manual(values = c("baseline" = "#6e6e6e", "shocked" = "#1f77b4")) +
      ggplot2::scale_linetype_manual(values = c("baseline" = "dashed", "shocked" = "solid")) +
      ggplot2::labs(
        title = paste0(scen, " — ", var),
        subtitle = if (!is.null(band_cols)) paste0("Bands: ±", band_cols$tag, "%") else NULL,
        x = "Horizon (t)", y = "Level / Δ"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.title.position = "plot"
      )
    
    # facet by regime if present
    if ("regime" %in% names(df)) {
      p <- p + ggplot2::facet_wrap(~ regime, scales = if (free_y) "free_y" else "fixed")
    }
    
    out[[i]] <- p
    names(out)[i] <- key
  }
  
  out
}