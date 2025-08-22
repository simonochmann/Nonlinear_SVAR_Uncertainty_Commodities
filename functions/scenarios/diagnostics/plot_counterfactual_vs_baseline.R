# functions/scenarios/diagnostics/plot_counterfactual_vs_baseline.R

#' Plot Δ path (shocked - baseline) with optional confidence bands
#'
#' Accepts a tidy panel with baseline and shocked paths, optionally with
#' simulation draws. If draws are present (`draw` column), Δ bands are computed
#' per-draw and summarized (percentile or normal). If not, the function tries to
#' reuse shocked bands when baseline has no bands (common when baseline==0).
#'
#' Columns expected (minimum): t, variable, value, scenario, is_baseline
#' Optional: regime, draw, lo_*, hi_* (precomputed bands)
#'
#' @param tidy Tibble as described above.
#' @param prefer_levels Numeric vector of desired CI coverages to use for plotting
#'   when multiple are present/computed. Default: c(0.90, 0.95, 0.80).
#' @param ci_method "percentile" or "normal" (used only when computing from draws). Default "percentile".
#' @param center "mean" or "median" for the plotted Δ line when computing from draws. Default "mean".
#' @param cumulative Logical. If TRUE, plot cumulative Δ (time-cumsum within each panel).
#' @param percent Logical. If TRUE, plot %Δ = 100 * (shocked - baseline) / (|baseline| + eps).
#' @param percent_eps Small positive to avoid division by zero when percent=TRUE. Default 1e-8.
#' @param free_y Facet scales free in y? Default FALSE.
#' @param show_points Add points atop lines? Default FALSE.
#' @param annotate_peaks Mark the max |Δ| point per panel. Default TRUE.
#' @param min_draws Minimum draws required to form bands from draws (warn if fewer). Default 100.
#'
#' @return Named list of ggplot objects keyed "<scenario>_<variable>_delta".
#' @export
plot_counterfactual_vs_baseline <- function(
    tidy,
    prefer_levels = c(0.90, 0.95, 0.80),
    ci_method     = c("percentile","normal"),
    center        = c("mean","median"),
    cumulative    = FALSE,
    percent       = FALSE,
    percent_eps   = 1e-8,
    free_y        = FALSE,
    show_points   = FALSE,
    annotate_peaks= TRUE,
    min_draws     = 100L
) {
  ci_method <- match.arg(ci_method)
  center    <- match.arg(center)
  
  # ---- guards ----
  req <- c("t","variable","value","scenario","is_baseline")
  miss <- setdiff(req, names(tidy))
  if (length(miss)) stop("Input `tidy` missing columns: ", paste(miss, collapse=", "))
  
  tidy <- tibble::as_tibble(tidy)
  tidy$t <- as.integer(tidy$t)
  tidy$is_baseline <- as.logical(tidy$is_baseline)
  
  has_regime <- "regime" %in% names(tidy)
  has_draw   <- "draw"   %in% names(tidy)
  
  # helper: CI tag like 0.90 -> "90"
  lvl_tag <- function(p) gsub("\\.", "", sprintf("%02.0f", 100 * p))
  
  # ---- compute Δ (and optional %Δ / cumulative) ------------------------------
  # If draws exist: compute Δ per draw first.
  if (has_draw) {
    base_draw <- tidy |>
      dplyr::filter(is_baseline) |>
      dplyr::select(scenario, variable, t, dplyr::all_of(if (has_regime) "regime" else NULL), draw, base = value)
    
    shock_draw <- tidy |>
      dplyr::filter(!is_baseline) |>
      dplyr::select(scenario, variable, t, dplyr::all_of(if (has_regime) "regime" else NULL), draw, shocked = value)
    
    d <- dplyr::inner_join(base_draw, shock_draw,
                           by = c("scenario","variable","t", if (has_regime) "regime", "draw"))
    
    d <- d |>
      dplyr::mutate(
        delta_raw = shocked - base,
        delta = if (percent) 100 * (shocked - base) / (abs(base) + percent_eps) else delta_raw
      )
    
    # cumulative within each panel × draw
    if (isTRUE(cumulative)) {
      d <- d |>
        dplyr::arrange(scenario, variable, dplyr::across(dplyr::all_of(if (has_regime) "regime")), t, draw) |>
        dplyr::group_by(scenario, variable, dplyr::across(dplyr::all_of(if (has_regime) "regime")), draw) |>
        dplyr::mutate(delta = cumsum(delta)) |>
        dplyr::ungroup()
    }
    
    # summarize across draws to get center + bands
    nd <- dplyr::n_distinct(d$draw)
    if (nd < min_draws) {
      warning("Only ", nd, " distinct draws; bands may be unstable (min_draws=", min_draws, ").")
    }
    
    if (identical(ci_method, "percentile")) {
      alpha <- (1 - prefer_levels) / 2
      dsum <- d |>
        dplyr::group_by(scenario, variable, dplyr::across(dplyr::all_of(if (has_regime) "regime")), t) |>
        dplyr::summarise(
          center = if (center == "mean") mean(delta, na.rm = TRUE) else stats::median(delta, na.rm = TRUE),
          !!!setNames(
            replicate(length(prefer_levels)*2, numeric(1), simplify = FALSE),
            unlist(lapply(prefer_levels, function(p) {
              tag <- lvl_tag(p); c(paste0("lo_", tag), paste0("hi_", tag))
            }))
          ),
          .groups = "drop"
        )
      # fill quantiles
      fill_q <- function(vec, probs, prefix) {
        qs <- stats::quantile(vec, probs = probs, na.rm = TRUE, names = FALSE, type = 7)
        names(qs) <- paste0(prefix, "_", lvl_tag(1 - 2*abs(0.5 - probs))) # not used directly
        qs
      }
      # compute per group (we already summarised; recompute by group-wise apply)
      dsplit <- d |>
        dplyr::group_by(scenario, variable, dplyr::across(dplyr::all_of(if (has_regime) "regime")), t) |>
        dplyr::group_split()
      
      inject_bands <- function(grp_df, out_row) {
        alpha <- (1 - prefer_levels) / 2
        q_lo <- stats::quantile(grp_df$delta, probs = alpha,    na.rm = TRUE, names = FALSE, type = 7)
        q_hi <- stats::quantile(grp_df$delta, probs = 1 - alpha,na.rm = TRUE, names = FALSE, type = 7)
        for (i in seq_along(prefer_levels)) {
          tag <- lvl_tag(prefer_levels[i])
          out_row[[paste0("lo_", tag)]] <- q_lo[i]
          out_row[[paste0("hi_", tag)]] <- q_hi[i]
        }
        out_row
      }
      # rebuild dsum by applying inject_bands per split group
      dsum <- dplyr::bind_rows(purrr::imap(dsplit, function(grp, idx) {
        key <- grp[1, c("scenario","variable", if (has_regime) "regime", "t"), drop = FALSE]
        out_row <- key
        out_row$center <- if (center == "mean") mean(grp$delta, na.rm = TRUE) else stats::median(grp$delta, na.rm = TRUE)
        inject_bands(grp, out_row)
      }))
      
      delta_df <- dsum |>
        dplyr::rename(value = center)
      
    } else { # normal approx
      dsum <- d |>
        dplyr::group_by(scenario, variable, dplyr::across(dplyr::all_of(if (has_regime) "regime")), t) |>
        dplyr::summarise(
          m  = mean(delta, na.rm = TRUE),
          sd = stats::sd(delta, na.rm = TRUE),
          .groups = "drop"
        )
      out <- dsum
      for (p in prefer_levels) {
        z <- stats::qnorm((1 + p) / 2)
        tag <- lvl_tag(p)
        out[[paste0("lo_", tag)]] <- out$m - z * out$sd
        out[[paste0("hi_", tag)]] <- out$m + z * out$sd
      }
      delta_df <- out |>
        dplyr::rename(value = m) |>
        dplyr::select(-sd)
    }
  } else {
    # no draws: compute Δ from single paths
    base <- tidy |>
      dplyr::filter(is_baseline) |>
      dplyr::select(scenario, variable, t, dplyr::all_of(if (has_regime) "regime" else NULL), baseline = value)
    
    shock <- tidy |>
      dplyr::filter(!is_baseline) |>
      dplyr::select(scenario, variable, t, dplyr::all_of(if (has_regime) "regime" else NULL), shocked = value,
                    dplyr::matches("^lo_"), dplyr::matches("^hi_"))
    
    delta_df <- dplyr::full_join(base, shock, by = c("scenario","variable","t", if (has_regime) "regime")) |>
      dplyr::mutate(
        value = if (percent) 100 * (shocked - baseline) / (abs(baseline) + percent_eps) else (shocked - baseline)
      )
    
    if (isTRUE(cumulative)) {
      delta_df <- delta_df |>
        dplyr::arrange(scenario, variable, dplyr::across(dplyr::all_of(if (has_regime) "regime")), t) |>
        dplyr::group_by(scenario, variable, dplyr::across(dplyr::all_of(if (has_regime) "regime"))) |>
        dplyr::mutate(value = cumsum(value)) |>
        dplyr::ungroup()
    }
    
    # attempt band reuse if baseline had no bands and was deterministic
    # (i.e., if lo_*/hi_* exist only for shocked). This is a heuristic.
    has_lohi <- function(nm) any(grepl(paste0("^", nm, "_"), names(delta_df)))
    # nothing extra to do; we just keep shocked bands columns attached
  }
  
  # ---- choose which band pair to display (if any) ----------------------------
  detect_band_cols <- function(cols) {
    lo_tags <- sub("^lo_", "", grep("^lo_", cols, value = TRUE))
    hi_tags <- sub("^hi_", "", grep("^hi_", cols, value = TRUE))
    tags <- intersect(lo_tags, hi_tags)
    if (!length(tags)) return(NULL)
    pref_tags <- gsub("\\.", "", sprintf("%02.0f", 100 * prefer_levels))
    tag <- pref_tags[pref_tags %in% tags][1]
    if (is.na(tag)) tag <- tags[1]
    list(lo = paste0("lo_", tag), hi = paste0("hi_", tag), tag = tag)
  }
  band_cols <- detect_band_cols(names(delta_df))
  
  # ---- build plots per (scenario × variable) ---------------------------------
  delta_df <- delta_df |>
    dplyr::arrange(scenario, variable, dplyr::across(dplyr::all_of(if (has_regime) "regime")), t)
  
  groups <- delta_df |>
    dplyr::group_split(scenario, variable)
  
  out <- vector("list", length(groups))
  names(out) <- character(length(groups))
  
  ylab <- if (percent) "% Δ (shocked − baseline)" else if (cumulative) "Cumulative Δ" else "Δ (shocked − baseline)"
  
  for (i in seq_along(groups)) {
    df <- groups[[i]]
    scen <- df$scenario[1]
    var  <- df$variable[1]
    key  <- paste0(scen, "_", var, "_delta")
    
    p <- ggplot2::ggplot(df, ggplot2::aes(x = t, y = value,
                                          group = !!as.name(if (has_regime) "regime" else "variable")))
    
    if (!is.null(band_cols) && all(band_cols %in% names(df))) {
      p <- p + ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data[[band_cols$lo]], ymax = .data[[band_cols$hi]]),
        alpha = 0.20
      )
    }
    
    p <- p +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
      ggplot2::geom_line(linewidth = 0.7, color = "#1f77b4") +
      { if (show_points) ggplot2::geom_point(size = 0.8) else ggplot2::geom_blank() } +
      ggplot2::labs(
        title = paste0(scen, " — Δ ", var),
        subtitle = if (!is.null(band_cols)) paste0("Bands: ±", band_cols$tag, "%") else NULL,
        x = "Horizon (t)",
        y = ylab
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        plot.title.position = "plot"
      )
    
    if (has_regime) {
      p <- p + ggplot2::facet_wrap(~ regime, scales = if (free_y) "free_y" else "fixed")
    }
    
    # annotate peak |Δ|
    if (isTRUE(annotate_peaks)) {
      if (has_regime) {
        peaks <- df |>
          dplyr::group_by(regime) |>
          dplyr::slice_min(order_by = -abs(value), n = 1, with_ties = FALSE) |>
          dplyr::ungroup()
      } else {
        peaks <- df |>
          dplyr::slice_min(order_by = -abs(value), n = 1, with_ties = FALSE)
      }
      p <- p +
        ggplot2::geom_point(data = peaks, ggplot2::aes(x = t, y = value), inherit.aes = FALSE, size = 1.5) +
        ggplot2::geom_text(
          data = peaks,
          ggplot2::aes(x = t, y = value, label = paste0("t*", "=", t, "\n", sprintf("%0.3g", value))),
          vjust = -0.5, size = 3
        )
    }
    
    out[[i]] <- p
    names(out)[i] <- key
  }
  
  out
}