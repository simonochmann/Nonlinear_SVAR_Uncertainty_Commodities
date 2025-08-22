# functions/decomposition/metrics/compute_signal_metrics.R
find_peak_horizons <- function(delta_tbl) {
  grp <- intersect(c("uncertainty","scenario","regime"), names(delta_tbl))
  delta_tbl %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) %>%
    dplyr::summarise(
      t_peak = t[which.max(abs(delta))][1],
      peak_abs_delta = max(abs(delta), na.rm=TRUE),
      .groups = "drop"
    )
}

compute_energy_metrics <- function(delta_tbl, decay = 0.90) {
  grp <- intersect(c("uncertainty","scenario","regime","variable"), names(delta_tbl))
  delta_tbl %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) %>%
    dplyr::summarise(
      energy = sum(delta^2, na.rm=TRUE),                     # ∑ Δ²
      persistence = sum((decay^(t-1)) * abs(delta), na.rm=TRUE),  # decay-weighted |Δ|
      peak_abs = max(abs(delta), na.rm=TRUE),
      .groups = "drop"
    )
}

dispersion_at_peak <- function(delta_tbl, peaks_tbl) {
  key <- intersect(c("uncertainty","scenario","regime"), names(delta_tbl))
  delta_tbl %>%
    dplyr::inner_join(peaks_tbl, by = key) %>%
    dplyr::filter(t == t_peak) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key))) %>%
    dplyr::summarise(
      csd_abs = sd(abs(delta), na.rm=TRUE),                 # cross-sectional dispersion at peak
      top3_share_abs = {
        v <- sort(abs(delta), decreasing=TRUE)
        sum(v[1:min(3,length(v))]) / sum(v, na.rm=TRUE)
      },
      .groups = "drop"
    )
}

contrib_concentration <- function(contrib_tbl, delta_tbl, at = c("peak","terminal")) {
  at <- match.arg(at)
  key <- intersect(c("uncertainty","scenario","regime"), names(contrib_tbl))
  H <- max(contrib_tbl$t, na.rm=TRUE)
  if (at == "peak") {
    peaks <- find_peak_horizons(delta_tbl)
    ref <- contrib_tbl %>%
      dplyr::inner_join(peaks, by = key) %>% dplyr::filter(t == t_peak)
  } else {
    ref <- contrib_tbl %>% dplyr::filter(t == H)
  }
  
  ref %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key))) %>%
    dplyr::mutate(w = abs(contribution),
                  s = ifelse(sum(w,na.rm=TRUE)>0, w/sum(w,na.rm=TRUE), 0)) %>%
    dplyr::summarise(
      herfindahl = sum(s^2, na.rm=TRUE),                    # concentration (0,1]
      top3_share = sum(sort(s, decreasing=TRUE)[1:min(3,length(s))], na.rm=TRUE),
      .groups = "drop"
    )
}

uncertainty_consistency <- function(contrib_tbl, at = "peak", delta_tbl = NULL) {
  # Spearman rank-corr of shock attributions across uncertainties per scenario
  if (!"uncertainty" %in% names(contrib_tbl)) return(NULL)
  key <- intersect(c("scenario","regime"), names(contrib_tbl))
  H <- max(contrib_tbl$t, na.rm=TRUE)
  pick <- function(df) {
    if (at == "terminal") df %>% dplyr::filter(t == H) else {
      stopifnot(!is.null(delta_tbl))
      peaks <- find_peak_horizons(delta_tbl)
      df %>% dplyr::inner_join(peaks, by = c("uncertainty", key)) %>% dplyr::filter(t == t_peak)
    }
  }
  X <- pick(contrib_tbl) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("uncertainty", key, "shock_id")))) %>%
    dplyr::summarise(val = sum(contribution, na.rm=TRUE), .groups="drop")
  
  out <- list()
  for (sc in unique(X$scenario)) for (rg in unique(X$regime)) {
    M <- X %>% dplyr::filter(scenario==sc, regime==rg) %>%
      tidyr::pivot_wider(names_from = uncertainty, values_from = val, values_fill = 0) %>%
      dplyr::select(-shock_id, -scenario, -regime)
    if (ncol(M) >= 2) {
      C <- suppressWarnings(stats::cor(M, method="spearman"))
      out[[length(out)+1L]] <- tibble::tibble(
        scenario = sc, regime = rg,
        pair = paste(colnames(C)[row(C) < col(C)], colnames(C)[col(C) > row(C)], sep="~"),
        rho  = C[row(C) < col(C)]
      )
    }
  }
  dplyr::bind_rows(out)
}
