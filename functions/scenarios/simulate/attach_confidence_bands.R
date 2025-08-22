# functions/scenarios/simulate/attach_confidence_bands.R

#' Attach confidence bands to scenario paths (collapse draws → center + bands)
#'
#' If a `draw` column exists, this function aggregates across draws and returns
#' one row per {scenario, variable, (regime), t, is_baseline}, with:
#'   - value: center across draws (mean or median)
#'   - lo_XX / hi_XX columns for each requested level (XX = 90, 95, …)
#'
#' If no `draw` column exists:
#'   - If precomputed band columns (`lo_*`, `hi_*`) are present, input is returned unchanged.
#'   - Otherwise, the input is returned unchanged with a message (nothing to attach).
#'
#' @param tidy Tibble with columns at least:
#'   t, variable, value, scenario, is_baseline; optionally regime, draw.
#' @param levels Numeric vector of desired coverages in (0,1), e.g. c(0.90, 0.95).
#' @param method "percentile" (quantiles across draws) or "normal" (mean ± z·sd).
#' @param center "mean" or "median" — center of the returned path when collapsing draws.
#'               Note: with method="normal", bands use mean ± z·sd; center may still be "median".
#' @param include_baseline Logical; if FALSE (default), bands are only attached to
#'                         shocked paths (is_baseline == FALSE). Baseline still gets
#'                         collapsed to the chosen center if draws exist.
#' @param min_draws Integer; warn if fewer draws are available. Default 100.
#' @param keep_draws Logical; if TRUE and a `draw` column exists, both the collapsed
#'                   summary and the original draw-level rows are returned (bound together).
#'                   Default FALSE (return only the collapsed summary with bands).
#'
#' @return Tibble: aggregated (and optionally bands-attached) panel.
#' @export
attach_confidence_bands <- function(
    tidy,
    levels           = 0.90,
    method           = c("percentile","normal"),
    center           = c("mean","median"),
    include_baseline = FALSE,
    min_draws        = 100L,
    keep_draws       = FALSE
) {
  method <- match.arg(method)
  center <- match.arg(center)
  
  # ---- guards ----------------------------------------------------------------
  req <- c("t","variable","value","scenario","is_baseline")
  miss <- setdiff(req, names(tidy))
  if (length(miss)) stop("`tidy` is missing columns: ", paste(miss, collapse = ", "))
  
  tidy <- tibble::as_tibble(tidy)
  tidy$t <- as.integer(tidy$t)
  tidy$is_baseline <- as.logical(tidy$is_baseline)
  
  has_regime <- "regime" %in% names(tidy)
  has_draw   <- "draw"   %in% names(tidy)
  
  # helper: 0.90 -> "90"
  lvl_tag <- function(p) gsub("\\.", "", sprintf("%02.0f", 100 * p))
  
  # If no draw column, pass through
  if (!has_draw) {
    # If bands already present, just return as-is
    has_any_bands <- any(grepl("^lo_", names(tidy))) && any(grepl("^hi_", names(tidy)))
    if (!has_any_bands) {
      message("[attach_confidence_bands] No `draw` column and no existing bands; returning input unchanged.")
    }
    return(tidy)
  }
  
  # ---- sanity on draw counts --------------------------------------------------
  nd <- dplyr::n_distinct(tidy$draw)
  if (nd < min_draws) {
    warning("[attach_confidence_bands] Only ", nd, " distinct draws; bands may be unstable (min_draws=", min_draws, ").")
  }
  
  # ---- collapse draws to center + bands --------------------------------------
  keys_base <- c("scenario","variable", if (has_regime) "regime", "t", "is_baseline")
  
  if (identical(method, "percentile")) {
    alpha <- (1 - levels) / 2
    
    # split groups to compute quantiles cleanly
    splits <- tidy |>
      dplyr::group_by(dplyr::across(dplyr::all_of(keys_base))) |>
      dplyr::group_split()
    
    out <- dplyr::bind_rows(purrr::map(splits, function(df) {
      key <- df[1, keys_base, drop = FALSE]
      ct  <- if (center == "median") stats::median(df$value, na.rm = TRUE) else mean(df$value, na.rm = TRUE)
      
      # compute desired quantiles
      qs_lo <- stats::quantile(df$value, probs = alpha,     na.rm = TRUE, names = FALSE, type = 7)
      qs_hi <- stats::quantile(df$value, probs = 1 - alpha, na.rm = TRUE, names = FALSE, type = 7)
      
      row <- key
      row$value <- ct
      for (i in seq_along(levels)) {
        tag <- lvl_tag(levels[i])
        row[[paste0("lo_", tag)]] <- qs_lo[i]
        row[[paste0("hi_", tag)]] <- qs_hi[i]
      }
      row
    }))
    
  } else { # method == "normal"
    out <- tidy |>
      dplyr::group_by(dplyr::across(dplyr::all_of(keys_base))) |>
      dplyr::summarise(
        m  = mean(value, na.rm = TRUE),
        sd = stats::sd(value, na.rm = TRUE),
        .groups = "drop"
      )
    # center value (mean or median-of-draws — we only have mean/sd here, so recompute median if needed)
    if (center == "median") {
      med_df <- tidy |>
        dplyr::group_by(dplyr::across(dplyr::all_of(keys_base))) |>
        dplyr::summarise(value = stats::median(value, na.rm = TRUE), .groups = "drop")
      out <- dplyr::left_join(out, med_df, by = keys_base)
    } else {
      out$value <- out$m
    }
    # attach bands: mean ± z*sd
    for (p in levels) {
      z <- stats::qnorm((1 + p) / 2)
      tag <- lvl_tag(p)
      out[[paste0("lo_", tag)]] <- out$m - z * out$sd
      out[[paste0("hi_", tag)]] <- out$m + z * out$sd
    }
    out <- dplyr::select(out, -m, -sd)
  }
  
  # ---- optionally suppress baseline bands ------------------------------------
  if (!isTRUE(include_baseline)) {
    band_cols <- grep("^(lo_|hi_)", names(out), value = TRUE)
    if (length(band_cols)) {
      out[ out$is_baseline, band_cols ] <- NA_real_
    }
  }
  
  # ---- optionally keep original draw-level rows -------------------------------
  if (isTRUE(keep_draws)) {
    # make sure columns align (add empty band cols to tidy if needed)
    band_cols <- setdiff(names(out), names(tidy))
    for (bc in band_cols) tidy[[bc]] <- NA_real_
    # remove draw column from summary (it won't exist)
    # ensure same column order
    common <- union(names(tidy), names(out))
    tidy[setdiff(common, names(tidy))] <- NA
    out[setdiff(common, names(out))]   <- NA
    out <- out[, common, drop = FALSE]
    tidy <- tidy[, common, drop = FALSE]
    return(dplyr::bind_rows(out, tidy))
  }
  
  out
}