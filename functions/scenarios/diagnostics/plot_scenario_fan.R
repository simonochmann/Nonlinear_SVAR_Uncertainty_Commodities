# functions/scenarios/diagnostics/plot_scenario_fan.R

#' Fan chart for scenario paths (nested CI ribbons + center line)
#'
#' Expects a tidy tibble with columns:
#'   - required: t, variable, value, scenario, is_baseline (logical)
#'   - optional: regime, draw, lo_*, hi_* (precomputed bands)
#'
#' If no lo_*/hi_* columns are present and a `draw` column exists, bands are
#' computed from draws via percentiles. Otherwise, existing bands are used.
#'
#' @param tidy Tibble as described above.
#' @param levels Numeric vector of desired coverages in (0,1) for nested fans
#'   (outer→inner order). Default c(0.50, 0.80, 0.90, 0.95).
#' @param center "median" (default) or "mean" for the center line (used when computing from draws).
#' @param show_baseline Logical; overlay dashed baseline path if present. Default TRUE.
#' @param free_y Logical; facet scales free by regime. Default FALSE.
#' @param title_prefix Optional string prepended to plot titles. Default NULL.
#' @return Named list of ggplot objects keyed "<scenario>_<variable>_fan".
#' @export
plot_scenario_fan <- function(
    tidy,
    levels       = c(0.50, 0.80, 0.90, 0.95),
    center       = c("median","mean"),
    show_baseline= TRUE,
    free_y       = FALSE,
    title_prefix = NULL
) {
  center <- match.arg(center)
  
  # ---- guards ----
  req <- c("t","variable","value","scenario","is_baseline")
  miss <- setdiff(req, names(tidy))
  if (length(miss)) stop("`tidy` is missing columns: ", paste(miss, collapse=", "))
  
  tidy <- tibble::as_tibble(tidy)
  tidy$t <- as.integer(tidy$t)
  tidy$is_baseline <- as.logical(tidy$is_baseline)
  
  has_regime <- "regime" %in% names(tidy)
  has_draw   <- "draw"   %in% names(tidy)
  
  # helper: "0.90" → "90"
  lvl_tag <- function(p) gsub("\\.", "", sprintf("%02.0f", 100 * p))
  
  # ---- pick shocked-only data (fan is for shocked path) ----------------------
  shocked <- tidy |> dplyr::filter(!is_baseline)
  if (!nrow(shocked)) stop("No shocked rows found (is_baseline == FALSE).")
  
  # baseline for optional overlay
  baseline <- NULL
  if (isTRUE(show_baseline) && any(tidy$is_baseline)) {
    baseline <- tidy |>
      dplyr::filter(is_baseline) |>
      dplyr::select(scenario, variable, t, dplyr::all_of(if (has_regime) "regime" else NULL), value)
  }
  
  # ---- detect existing bands or compute from draws ---------------------------
  lo_cols <- grep("^lo_", names(shocked), value = TRUE)
  hi_cols <- grep("^hi_", names(shocked), value = TRUE)
  have_bands <- length(intersect(sub("^lo_", "", lo_cols), sub("^hi_", "", hi_cols))) > 0
  
  # desired tags in outer→inner order
  desired_tags <- vapply(levels, lvl_tag, character(1))
  
  if (!have_bands && !has_draw) {
    stop("No lo_*/hi_* band columns and no `draw` column to compute bands from.")
  }
  
  # Prepare a summary table with columns: scenario, variable, (regime), t, value(center), lo_<tag>, hi_<tag>...
  if (have_bands) {
    # Use existing bands; ensure the requested levels exist (if not, use available)
    tags_lo <- sub("^lo_", "", lo_cols)
    tags_hi <- sub("^hi_", "", hi_cols)
    avail <- intersect(tags_lo, tags_hi)
    use_tags <- if (any(desired_tags %in% avail)) {
      # keep only desired (in order), drop missing
      desired_tags[desired_tags %in% avail]
    } else {
      # fall back to all available (sorted ascending numerically)
      avail[order(as.numeric(avail))]
    }
    
    shocked_sum <- shocked |>
      dplyr::select(scenario, variable, t,
                    dplyr::all_of(if (has_regime) "regime" else NULL),
                    value,
                    dplyr::all_of(paste0("lo_", use_tags)),
                    dplyr::all_of(paste0("hi_", use_tags)))
    
  } else {
    # compute from draws via percentiles
    alpha <- (1 - levels) / 2
    shocked_sum <- shocked |>
      dplyr::group_by(scenario, variable, dplyr::across(dplyr::all_of(if (has_regime) "regime")), t) |>
      dplyr::summarise(
        value = if (center == "median") stats::median(value, na.rm = TRUE) else mean(value, na.rm = TRUE),
        !!!setNames(vector("list", length(levels)), paste0("lo_", desired_tags)),
        !!!setNames(vector("list", length(levels)), paste0("hi_", desired_tags)),
        .groups = "drop"
      )
    
    # compute quantiles per group and inject
    splits <- shocked |>
      dplyr::group_by(scenario, variable, dplyr::across(dplyr::all_of(if (has_regime) "regime")), t) |>
      dplyr::group_split()
    
    shocked_sum <- dplyr::bind_rows(purrr::map(splits, function(df) {
      key <- df[1, c("scenario","variable", if (has_regime) "regime", "t"), drop = FALSE]
      ct  <- if (center == "median") stats::median(df$value, na.rm = TRUE) else mean(df$value, na.rm = TRUE)
      out <- key; out$value <- ct
      for (i in seq_along(levels)) {
        tag <- desired_tags[i]
        out[[paste0("lo_", tag)]] <- stats::quantile(df$value, probs = alpha[i],     na.rm = TRUE, names = FALSE, type = 7)
        out[[paste0("hi_", tag)]] <- stats::quantile(df$value, probs = 1 - alpha[i], na.rm = TRUE, names = FALSE, type = 7)
      }
      out
    }))
    use_tags <- desired_tags
  }
  
  # ---- build plots per (scenario × variable) ---------------------------------
  shocked_sum <- shocked_sum |>
    dplyr::arrange(scenario, variable, dplyr::across(dplyr::all_of(if (has_regime) "regime")), t)
  
  groups <- shocked_sum |>
    dplyr::group_split(scenario, variable)
  
  out <- vector("list", length(groups))
  names(out) <- character(length(groups))
  
  # alpha schedule: outer (largest) = lightest; inner (smallest) = darkest
  alphas <- scales::rescale(seq_along(use_tags), to = c(0.15, 0.45))
  alphas <- alphas[rank(use_tags, ties.method = "first")]  # ensure consistent with tag order
  
  for (i in seq_along(groups)) {
    df <- groups[[i]]
    scen <- df$scenario[1]
    var  <- df$variable[1]
    key  <- paste0(scen, "_", var, "_fan")
    
    p <- ggplot2::ggplot()
    
    # add nested ribbons, widest first
    # sort tags from largest coverage to smallest
    ord_tags <- use_tags[order(as.numeric(use_tags), decreasing = TRUE)]
    for (j in seq_along(ord_tags)) {
      tag <- ord_tags[j]
      lo_nm <- paste0("lo_", tag); hi_nm <- paste0("hi_", tag)
      if (all(!is.na(df[[lo_nm]])) && all(!is.na(df[[hi_nm]]))) {
        p <- p + ggplot2::geom_ribbon(
          data = df,
          ggplot2::aes(x = t, ymin = .data[[lo_nm]], ymax = .data[[hi_nm]]),
          fill = "#1f77b4",
          alpha = alphas[match(tag, use_tags)]
        )
      }
    }
    
    # center line
    p <- p + ggplot2::geom_line(
      data = df, ggplot2::aes(x = t, y = value),
      color = "#1f77b4", linewidth = 0.9
    )
    
    # optional baseline overlay
    if (!is.null(baseline)) {
      base_df <- baseline |>
        dplyr::filter(scenario == scen, variable == var,
                      dplyr::if_else(has_regime, TRUE, TRUE))
      if (nrow(base_df)) {
        p <- p + ggplot2::geom_line(
          data = base_df,
          ggplot2::aes(x = t, y = value),
          color = "#6e6e6e", linetype = "dashed", linewidth = 0.6
        )
      }
    }
    
    p <- p +
      ggplot2::labs(
        title = paste0(if (!is.null(title_prefix)) paste0(title_prefix, " — ") else "", scen, " — ", var),
        subtitle = paste0("Fan: ", paste0(gsub("^0+", "", ord_tags), "%", collapse = " / ")),
        x = "Horizon (t)", y = "Level / Δ"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        plot.title.position = "plot",
        legend.position = "none"
      )
    
    # facet by regime if present
    if (has_regime) {
      p <- p + ggplot2::facet_wrap(~ regime, scales = if (free_y) "free_y" else "fixed")
    }
    
    out[[i]] <- p
    names(out)[i] <- key
  }
  
  out
}