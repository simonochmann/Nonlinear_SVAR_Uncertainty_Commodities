# functions/scenarios/diagnostics/plot_irf_envelope_vs_scenario.R

#' Overlay scenario paths with the implied IRF envelope (sanity check)
#'
#' For each response variable, plots:
#'   - the scenario path (either level or Δ = shocked - baseline), and
#'   - the conservative IRF envelope implied by the scenario's impulses, schedule,
#'     identification matrix A, and regime-specific IRFs.
#'
#' The IRF envelope is constructed by linear superposition of impulse-response
#' bands: for each time and response j, we sum intervals s_i * [lo_{i→j}(k), hi_{i→j}(k)]
#' across impulse channels i with sign-aware interval multiplication by the
#' structural impact weights s_i (from A %*% shock_vec * schedule_t). This yields
#' a conservative ribbon that contains the composite response if IRF bands are valid.
#'
#' Expected model structure (flexible, with fallbacks):
#'   - model$variables (character K)
#'   - model$irf[[regime]][[impulse]][[response]] = numeric vector (center IRF)
#'   - optionally model$irf_bands[[regime]][[impulse]][[response]] with elements like
#'       "lo_90","hi_90" (preferred), or "lo","hi" (assumed same level)
#'
#' Requires helper functions already in this repo:
#'   build_contemporaneous_A(), build_shock_vector(), scale_shock_by_sigma(),
#'   schedule_shocks_over_horizon()
#'
#' @param tidy Tidy tibble with baseline & shocked scenario paths:
#'   columns: t, variable, value, scenario, is_baseline (logical), optionally regime.
#' @param model TVAR/VAR model containing variables, IRFs (and optionally bands).
#' @param scenario Validated scenario spec (list).
#' @param overlay "delta" (default) to compare Δ path against IRF envelope of Δ,
#'   or "level" to compare shocked level against IRF envelope (baseline assumed 0).
#' @param band_level Desired CI coverage for IRF ribbon (e.g., 0.90). Will pick
#'   closest matching "lo_90/hi_90" if present; else tries lo/hi; else no ribbon.
#' @param regime_override Optional regime key ("low"/"high"/"combined"). If NULL,
#'   uses scenario$regime_conditioning ("none"→"combined", etc.).
#' @param free_y Logical; facet scales free. Default FALSE.
#' @param show_points Logical; add points on scenario line. Default FALSE.
#' @param verbose Logical; message() progress/warnings. Default FALSE.
#'
#' @return Named list of ggplot objects keyed "<scenario>_<variable>_irf_overlay".
#' @export
plot_irf_envelope_vs_scenario <- function(
    tidy,
    model,
    scenario,
    overlay          = c("delta","level"),
    band_level       = 0.90,
    regime_override  = NULL,
    free_y           = FALSE,
    show_points      = FALSE,
    verbose          = FALSE
) {
  overlay <- match.arg(overlay)
  `%||%` <- function(x,y) if (is.null(x)) y else x
  
  # ---- guards ----
  req <- c("t","variable","value","scenario","is_baseline")
  miss <- setdiff(req, names(tidy))
  if (length(miss)) stop("`tidy` missing columns: ", paste(miss, collapse = ", "))
  if (is.null(model$variables)) stop("`model$variables` missing.")
  if (is.null(model$irf))       stop("`model$irf` missing.")
  
  variables <- model$variables
  K <- length(variables)
  scen_name <- scenario$name %||% unique(tidy$scenario)[1]
  H <- as.integer(scenario$horizon %||% max(tidy$t, na.rm = TRUE))
  
  # Regime choice
  regime_key <- regime_override %||% switch(
    tolower(scenario$regime_conditioning %||% "none"),
    "force_low"  = "low",
    "force_high" = "high",
    "none"       = "combined",
    "combined"
  )
  irf_src <- model$irf[[regime_key]] %||% model$irf$combined %||% model$irf$low %||% model$irf$high
  if (is.null(irf_src)) stop("IRF source not found for regime '", regime_key, "' (and fallbacks).")
  
  irf_bands_src <- model$irf_bands[[regime_key]] %||% model$irf_bands$combined %||% NULL
  
  # identification matrix A
  A <- build_contemporaneous_A(
    model = model,
    identification = scenario$identification %||% "unit",
    variables = variables,
    regime = regime_key,
    verbose = verbose
  )
  
  # Utility: fetch IRF center & bands for (impulse i -> response j)
  lvl_tag <- gsub("\\.", "", sprintf("%02.0f", 100 * band_level))
  .get_irf_entry <- function(impulse, response) {
    # center
    v_center <- try(irf_src[[impulse]][[response]], silent = TRUE)
    if (inherits(v_center, "try-error") || is.null(v_center))
      stop("Missing IRF center for impulse='", impulse, "' -> response='", response, "'.")
    v_center <- as.numeric(v_center)
    if (length(v_center) < H) v_center <- c(v_center, rep(0, H - length(v_center)))
    v_center <- v_center[seq_len(H)]
    # bands (optional)
    v_lo <- v_hi <- NULL
    if (!is.null(irf_bands_src) && !is.null(irf_bands_src[[impulse]]) &&
        !is.null(irf_bands_src[[impulse]][[response]])) {
      b <- irf_bands_src[[impulse]][[response]]
      # prefer lo_<lvl>, hi_<lvl>
      lo_name <- paste0("lo_", lvl_tag)
      hi_name <- paste0("hi_", lvl_tag)
      if (lo_name %in% names(b) && hi_name %in% names(b)) {
        v_lo <- as.numeric(b[[lo_name]]); v_hi <- as.numeric(b[[hi_name]])
      } else if (all(c("lo","hi") %in% names(b))) {
        v_lo <- as.numeric(b$lo); v_hi <- as.numeric(b$hi)
        if (verbose) message("Using generic lo/hi bands for ", impulse, "→", response, ".")
      }
      if (!is.null(v_lo) && length(v_lo) < H) v_lo <- c(v_lo, rep(0, H - length(v_lo)))
      if (!is.null(v_hi) && length(v_hi) < H) v_hi <- c(v_hi, rep(0, H - length(v_hi)))
      if (!is.null(v_lo)) v_lo <- v_lo[seq_len(H)]
      if (!is.null(v_hi)) v_hi <- v_hi[seq_len(H)]
    }
    list(center = v_center, lo = v_lo, hi = v_hi)
  }
  
  # Convenience cache to avoid repeated extraction
  irf_cache <- new.env(parent = emptyenv())
  .irf <- function(i, j) {
    key <- paste(i, j, sep = "->")
    if (!exists(key, envir = irf_cache, inherits = FALSE)) {
      assign(key, .get_irf_entry(i, j), envir = irf_cache)
    }
    get(key, envir = irf_cache, inherits = FALSE)
  }
  
  # Build scenario-implied IRF composite (center + envelope) per response
  # We handle multi-impulse scenarios, baskets, weights, scaling, and scheduling.
  sigma <- model$sigma %||% stats::setNames(rep(1, K), variables)
  aliases <- scenario$aliases %||% NULL
  
  # helper to create a custom shock vector (absolute units) for a scenario impulse
  .shock_vec_from_impulse <- function(imp) {
    if (is.character(imp$variable)) {
      # per-variable scaling to absolute units
      sizes_abs <- vapply(imp$variable, function(vn) {
        scale_shock_by_sigma(
          size   = as.numeric(imp$size),
          scale  = tolower(imp$scale %||% "sd"),
          sigma  = sigma,
          var    = vn,
          aliases= aliases
        )
      }, numeric(1))
      names(sizes_abs) <- imp$variable
      # optional weights
      if (!is.null(imp$weights)) {
        w <- as.numeric(imp$weights)
        if (length(w) != length(sizes_abs)) stop("weights length must match impulse$variable.")
        s <- sum(abs(w)); if (s == 0) stop("All weights are zero in impulse basket.")
        w <- w / s
        # reference magnitude: mean abs(sizes) to avoid outlier dominance
        ref_mag <- mean(abs(sizes_abs))
        sizes_abs <- stats::setNames(w * ref_mag * sign(sizes_abs[1]), names(sizes_abs))
      }
      build_shock_vector(variables, impulse = sizes_abs, normalize = "none")
    } else if (is.numeric(imp$variable) && !is.null(names(imp$variable))) {
      # already named numeric vector
      build_shock_vector(variables, impulse = imp$variable, normalize = "none")
    } else {
      stop("Impulse `variable` must be character (names) or named numeric vector.")
    }
  }
  
  # Precompute scenario Δ or level series for overlay
  overlay_df <- if (overlay == "delta") {
    base <- tidy |> dplyr::filter(is_baseline) |>
      dplyr::select(scenario, variable, t, dplyr::all_of(if ("regime" %in% names(tidy)) "regime" else NULL), base = value)
    shocked <- tidy |> dplyr::filter(!is_baseline) |>
      dplyr::select(scenario, variable, t, dplyr::all_of(if ("regime" %in% names(tidy)) "regime" else NULL), shocked = value)
    dplyr::inner_join(base, shocked, by = c("scenario","variable","t", if ("regime" %in% names(tidy)) "regime")) |>
      dplyr::mutate(value = shocked - base) |>
      dplyr::select(scenario, variable, t, dplyr::all_of(if ("regime" %in% names(tidy)) "regime" else NULL), value)
  } else {
    tidy |> dplyr::filter(!is_baseline) |>
      dplyr::select(scenario, variable, t, dplyr::all_of(if ("regime" %in% names(tidy)) "regime" else NULL), value)
  }
  
  overlay_df <- overlay_df |> dplyr::filter(scenario == scen_name)
  
  # Initialize containers for envelope (per response variable)
  resp_list <- setNames(vector("list", length(variables)), variables)
  for (resp in variables) {
    center <- numeric(H)
    lo <- hi <- rep(0, H)  # start at 0; we'll sum intervals
    
    # loop over each declared impulse in the scenario
    for (imp in scenario$impulses) {
      shock_vec <- .shock_vec_from_impulse(imp)
      sched <- schedule_shocks_over_horizon(
        horizon = H,
        start   = imp$schedule$start,
        length  = imp$schedule$length,
        type    = tolower(imp$schedule$type %||% "constant"),
        decay   = as.numeric(imp$schedule$decay %||% 0.5),
        profile = imp$schedule$profile %||% NULL,
        normalize = isTRUE(imp$schedule$normalize),
        allow_truncate = TRUE
      )
      # structural impact weights (length K), applied when schedule != 0
      impact0 <- as.numeric(A %*% shock_vec)
      
      # accumulate contributions over time
      for (tau in which(sched != 0)) {
        mult <- sched[tau]
        # for each future horizon h >= tau (lag k)
        for (h in tau:H) {
          k <- h - tau + 1L
          # sum over impulse channels i
          c_low  <- 0
          c_high <- 0
          c_cent <- 0
          for (i in seq_len(K)) {
            s_i <- impact0[i] * mult
            if (s_i == 0) next
            irf_e <- .irf(variables[i], resp)
            # center contribution
            irf_c <- irf_e$center[k]
            c_cent <- c_cent + s_i * irf_c
            # band contribution (if available)
            if (!is.null(irf_e$lo) && !is.null(irf_e$hi)) {
              lo_i <- irf_e$lo[k]; hi_i <- irf_e$hi[k]
              if (s_i >= 0) {
                c_low  <- c_low  + s_i * lo_i
                c_high <- c_high + s_i * hi_i
              } else {
                # flip interval when multiplying by negative
                c_low  <- c_low  + s_i * hi_i
                c_high <- c_high + s_i * lo_i
              }
            } else {
              # no bands; we'll keep 0 contribution to lo/hi (effectively no ribbon)
            }
          }
          center[h] <- center[h] + c_cent
          lo[h]     <- lo[h]     + c_low
          hi[h]     <- hi[h]     + c_high
        }
      }
    } # end impulses loop
    
    # if no bands at all, set lo/hi to NA to avoid plotting empty ribbon
    if (all(lo == 0) && all(hi == 0)) {
      lo[] <- NA_real_; hi[] <- NA_real_
    }
    resp_list[[resp]] <- tibble::tibble(
      t = seq_len(H),
      variable = resp,
      irf_center = center,
      irf_lo = lo,
      irf_hi = hi
    )
  }
  
  env_df <- dplyr::bind_rows(resp_list)
  
  # ---- build plots per response ------------------------------------------------
  # Restrict overlay_df to variables that exist in env_df (should be all)
  overlay_df <- overlay_df |> dplyr::semi_join(env_df |> dplyr::distinct(variable), by = "variable")
  
  groups <- overlay_df |> dplyr::group_split(variable)
  out <- vector("list", length(groups))
  names(out) <- character(length(groups))
  
  for (i in seq_along(groups)) {
    df_sc <- groups[[i]]
    var   <- df_sc$variable[1]
    key   <- paste0(scen_name, "_", var, "_irf_overlay")
    
    df_irf <- env_df |> dplyr::filter(variable == var)
    
    p <- ggplot2::ggplot() +
      # IRF ribbon if available
      { if (any(!is.na(df_irf$irf_lo))) ggplot2::geom_ribbon(
        data = df_irf,
        ggplot2::aes(x = t, ymin = irf_lo, ymax = irf_hi),
        alpha = 0.18
      ) else ggplot2::geom_blank()
      } +
      # IRF center line
      ggplot2::geom_line(
        data = df_irf,
        ggplot2::aes(x = t, y = irf_center),
        linetype = "dashed", linewidth = 0.7, color = "#6e6e6e"
      ) +
      # Scenario line
      ggplot2::geom_line(
        data = df_sc,
        ggplot2::aes(x = t, y = value),
        linewidth = 0.9, color = "#1f77b4"
      ) +
      { if (show_points) ggplot2::geom_point(data = df_sc, ggplot2::aes(x = t, y = value), size = 0.9) else ggplot2::geom_blank() } +
      ggplot2::labs(
        title = paste0(scen_name, " — ", var),
        subtitle = paste0("Overlay: scenario ", if (overlay=="delta") "Δ path" else "level",
                          " vs. IRF envelope (", regime_key, ", ", scenario$identification %||% "unit",
                          if (!is.null(irf_bands_src)) paste0(", ~", gsub("^0+", "", lvl_tag), "% CI") else ", no bands", ")"),
        x = "Horizon (t)",
        y = if (overlay=="delta") "Δ (shocked − baseline)" else "Level"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        plot.title.position = "plot"
      )
    
    if ("regime" %in% names(df_sc)) {
      # If there are multiple regimes in tidy (rare for a single scenario), facet.
      p <- p + ggplot2::facet_wrap(~ regime, scales = if (free_y) "free_y" else "fixed")
    }
    
    out[[i]] <- p
    names(out)[i] <- key
  }
  
  out
}