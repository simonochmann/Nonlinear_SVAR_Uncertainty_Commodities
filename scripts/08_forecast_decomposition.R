# scripts/08_forecast_decomposition.R
# Orchestrates forecast decomposition (Δ = shocked - baseline), exports, diagnostics, logs.
# Enhancements A–E:
#  A) Real FEVD from IRFs (optional switch)
#  B) Optional Shapley path attribution scaffold
#  C) Regime-aware contributions + per-uncertainty slicing
#  D) Diagnostics pack (mass balance per-uncertainty, figures, optional signal metrics)
#  E) Reproducible exports (FEVD CSVs, per-uncertainty outputs, logs)

suppressPackageStartupMessages({
  library(here); library(fs); library(readr); library(dplyr); library(tidyr); library(glue)
})

# Project setup
source("scripts/setup.R")  

suppressMessages(here::i_am("scripts/08_forecast_decomposition.R"))

# sanity check
here::here()  
fs::file_exists(here::here("functions","decomposition","setup","build_forecast_baseline.R"))

# Load decomposition modules
# setup
source(here::here("functions","decomposition","setup","build_forecast_baseline.R"))
source(here::here("functions","decomposition","setup","build_forecast_with_shocks.R"))

# core decomposition
source(here::here("functions","decomposition","decompose","decompose_forecast.R"))
source(here::here("functions","decomposition","decompose","decompose_by_shock.R"))
source(here::here("functions","decomposition","decompose","decompose_by_regime.R"))
source(here::here("functions","decomposition","decompose","compute_tvar_fevd.R"))
source(here::here("functions","decomposition","decompose","reconcile_additivity.R"))

# Shapley
source(here::here("functions","decomposition","shapley","shapley_contributions.R"))

# diagnostics
source(here::here("functions","decomposition","diagnostics","plot_decomp_waterfall.R"))
source(here::here("functions","decomposition","diagnostics","plot_decomp_stacked_area.R"))
source(here::here("functions","decomposition","diagnostics","plot_regime_share_bars.R"))
source(here::here("functions","decomposition","diagnostics","plot_fevd_stacked_area.R"))
source(here::here("functions","decomposition","diagnostics","_plot_utils.R"))

# exports
source(here::here("functions","decomposition","export","write_decomp_tables_csv.R"))
source(here::here("functions","decomposition","export","write_decomp_tables_tex.R"))
source(here::here("functions","decomposition","export","write_decomp_panels_tex.R"))

# logs
source(here::here("functions","decomposition","logs","log_decomp_run_md.R"))
source(here::here("functions","decomposition","logs","log_decomp_metadata_json.R"))

# validators
source(here::here("functions","decomposition","validate","validate_decomposition_inputs.R"))
source(here::here("functions","decomposition","validate","validate_decomposition_mass.R"))

# FEVD from IRFs (A)
source(here::here("functions","decomposition","fevd","build_fevd_from_irf.R"))
source(here::here("functions","decomposition","fevd","dedupe_normalise_fevd.R"))

# Optional metrics helper (D) 
if (file.exists(here::here("functions","decomposition","metrics","compute_signal_metrics.R"))) {
  source(here::here("functions","decomposition","metrics","compute_signal_metrics.R"))
}

# Orchestration toggles / config 
CFG <- list(
  # INPUT
  INPUT_TIDY_PATH   = NULL,  
  OUT_DIR           = here::here("data","scenarios"),
  FIG_DIR           = here::here("figures","decomposition"),
  LOG_DIR           = here::here("logs","decomposition"),
  
  # FEATURES
  FEVD_MODE         = "irf",   
  FEVD_REGIMES      = c("combined"),
  USE_SHAPLEY       = FALSE,       
  WRITE_TEX_TABLES  = TRUE,
  MAKE_PLOTS        = TRUE,
  COMPUTE_METRICS   = TRUE,        
  
  # plot/export sizing
  DPI               = 400,
  
  # reconciliation/validation
  RECONCILE_METHOD  = "residual_bucket",  
  MASS_TOL          = 1e-10,
  
  # Shapley controls
  SHAP_N_PERM       = 512L,
  SHAP_EXACT_IF_KLE = 7L,
  SHAP_SEED         = 42L
)

`%||%` <- function(x,y) if (is.null(x)) y else x

# Helpers 
ensure_dirs <- function(...) fs::dir_create(c(...))

resolve_tidy_input <- function() {
  cands <- c(
    here::here("data","scenarios","scenario_results_tidy_latest.csv"),
    here::here("data","scenarios","scenario_results_tidy_fast.csv")
  )
  hit <- cands[fs::file_exists(cands)]
  if (!length(hit)) stop("[08] No tidy scenario CSV found. Run 07 first.")
  hit[1]
}

build_trivial_fevd_shares <- function(delta_tbl) {
  keys <- c("regime","variable","t")
  if ("uncertainty" %in% names(delta_tbl)) keys <- c("uncertainty", keys)
  delta_tbl %>%
    dplyr::distinct(dplyr::across(all_of(keys))) %>%
    dplyr::transmute(
      dplyr::across(all_of(setdiff(keys, "variable"))),
      response   = variable,
      impulse    = "total",
      fevd_share = 1
    )
}

# Paths / dirs 
ensure_dirs(CFG$OUT_DIR, CFG$FIG_DIR, CFG$LOG_DIR)

# Stopwatch 
t_start <- proc.time()[["elapsed"]]

# Load cached scenario tidy & build baseline/shocked 
tidy_path <- CFG$INPUT_TIDY_PATH %||% resolve_tidy_input()
message("[08] Using tidy input: ", fs::path_rel(tidy_path))

base_res <- build_forecast_baseline(scenario_tidy = tidy_path)   # regime, variable, t, value (+/- uncertainty)
shck_res <- build_forecast_with_shocks(scenario_tidy = tidy_path) # scenario, regime, variable, t, value (+/- uncertainty)

baseline <- base_res$data
shocked  <- shck_res$data

# add uncertainty if present in both inputs
join_keys <- c("regime","variable","t")
if ("uncertainty" %in% names(baseline) && "uncertainty" %in% names(shocked)) {
  join_keys <- c("uncertainty", join_keys)
}

#  Δ table
delta_tbl <- shocked %>%
  dplyr::left_join(baseline %>% dplyr::rename(baseline = value), by = join_keys) %>%
  dplyr::mutate(delta = value - dplyr::coalesce(baseline, 0)) %>%
  dplyr::select(dplyr::any_of(c("uncertainty")), scenario, regime, variable, t, delta)

# Minimal lines plot / delta CSV
if (CFG$MAKE_PLOTS) {
  plot_path <- here::here("figures","decomposition","delta_lines.png")
  fs::dir_create(dirname(plot_path))
  suppressPackageStartupMessages(library(ggplot2))
  
  wide <- shocked %>%
    dplyr::rename(shocked = value) %>%
    dplyr::left_join(baseline %>% dplyr::rename(baseline = value), by = join_keys) %>%
    dplyr::mutate(
      baseline = dplyr::coalesce(baseline, 0),
      shocked  = dplyr::coalesce(shocked,  0),
      delta    = shocked - baseline
    )
  
  facet_formula <- if ("uncertainty" %in% names(wide))
    as.formula("~ uncertainty + scenario + regime") else
      as.formula("~ scenario + regime")
  
  p_delta <- ggplot(wide, aes(t, delta, color = variable)) +
    geom_line(alpha = 0.8) +
    facet_wrap(facet_formula, scales = "free_y") +
    labs(title = "Forecast Δ (shocked - baseline)", y = "Δ", x = "horizon") +
    theme_minimal(base_size = 11)
  
  ggsave(plot_path, p_delta, width = 12, height = 7, dpi = CFG$DPI)
  message("[08] wrote: ", fs::path_rel(plot_path))
  
  readr::write_csv(wide, here::here("data","scenarios","forecast_decomposition_delta.csv"))
  sum_tbl <- wide %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("uncertainty","scenario","regime","variable")))) %>%
    dplyr::summarise(
      max_abs_delta = max(abs(delta), na.rm = TRUE),
      t_of_max      = t[which.max(abs(delta))][1],
      .groups = "drop"
    )
  readr::write_csv(sum_tbl, here::here("data","scenarios","forecast_decomposition_summary.csv"))
}

# Decompose (per-uncertainty, then bind) 
do_one <- function(unc = NULL) {
  b <- if (!is.null(unc) && "uncertainty" %in% names(baseline)) dplyr::filter(baseline, uncertainty == unc) else baseline
  s <- if (!is.null(unc) && "uncertainty" %in% names(shocked))  dplyr::filter(shocked,  uncertainty == unc) else shocked
  d <- if (!is.null(unc) && "uncertainty" %in% names(delta_tbl)) dplyr::filter(delta_tbl, uncertainty == unc) else delta_tbl
  
  # validate each uncertainty slice
  validate_decomposition_inputs(b, s, require_same_horizon = TRUE)
  
  # FEVD shares 
  fevd_mode <- tolower(CFG$FEVD_MODE %||% "trivial")
  if (identical(fevd_mode, "trivial")) {
    fevd_shares <- build_trivial_fevd_shares(d)
    fevd_info   <- list(mode = "trivial", path = NULL)
  } else {
    # real FEVD from IRFs for the variables/horizon present in this slice
    H_target <- max(d$t, na.rm = TRUE)
    fevd_tbl <- build_fevd_from_irf(d, H = H_target, regimes = CFG$FEVD_REGIMES)
    if (!is.null(unc)) fevd_tbl <- dplyr::filter(fevd_tbl, uncertainty == unc)
    fevd_tbl <- dedupe_normalise_fevd(fevd_tbl)
    # Persist FEVD CSVs (E)
    fevd_dir <- fs::path(CFG$OUT_DIR, "fevd"); fs::dir_create(fevd_dir)
    fevd_path <- fs::path(fevd_dir, if (!is.null(unc)) glue("{unc}_fevd_shares.csv") else "fevd_shares.csv")
    readr::write_csv(fevd_tbl, fevd_path)
    # Align columns for decompose_forecast()
    keep_cols <- intersect(c("uncertainty","regime","response","impulse","t","fevd_share"), names(fevd_tbl))
    fevd_shares <- dplyr::select(fevd_tbl, dplyr::all_of(keep_cols))
    fevd_info   <- list(mode = "irf", path = fevd_path)
  }
  
  # Decomposition core 
  out <- decompose_forecast(
    baseline    = b,
    shocked     = s,
    fevd_shares = fevd_shares,                   
    method      = if (identical(fevd_mode,"trivial")) "fevd" else "fevd",
    reconcile   = CFG$RECONCILE_METHOD
  )
  
  # Stamp uncertainty
  if (!is.null(unc)) {
    out$delta_tbl      <- dplyr::mutate(out$delta_tbl,      uncertainty = unc, .before = 1)
    out$contrib_tbl    <- dplyr::mutate(out$contrib_tbl,    uncertainty = unc, .before = 1)
    out$reconciled_tbl <- dplyr::mutate(out$reconciled_tbl, uncertainty = unc, .before = 1)
  }
  
  # Attach FEVD meta for logging
  out$.__fevd__ <- fevd_info
  out
}

if ("uncertainty" %in% names(delta_tbl)) {
  uncertainties <- sort(unique(delta_tbl$uncertainty))
  res_list <- lapply(uncertainties, do_one)
  res <- list(
    delta_tbl      = dplyr::bind_rows(lapply(res_list, `[[`, "delta_tbl")),
    contrib_tbl    = dplyr::bind_rows(lapply(res_list, `[[`, "contrib_tbl")),
    reconciled_tbl = dplyr::bind_rows(lapply(res_list, `[[`, "reconciled_tbl")),
    .__fevd__      = lapply(res_list, `[[`, ".__fevd__")
  )
  names(res$.__fevd__) <- uncertainties
} else {
  res <- do_one(NULL)
}

# Detail one-pagers for the top response variables per scenario
export_detail_waterfalls <- function(ct, out_dir, unc, per_scenario_top = 6, top_n = 12) {
  dir <- fs::path(out_dir, glue::glue("detail_{unc}")); fs::dir_create(dir)
  
  topv <- ct %>%
    dplyr::group_by(scenario, variable) %>%
    dplyr::summarise(l1 = sum(abs(contribution), na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(scenario) %>%
    dplyr::slice_max(order_by = l1, n = per_scenario_top, with_ties = FALSE) %>%
    dplyr::ungroup()
  
  for (sc in unique(topv$scenario)) {
    vars <- topv |> dplyr::filter(scenario == sc) |> dplyr::pull(variable)
    for (v in vars) {
      plot_decomp_waterfall(
        contrib_tbl    = dplyr::filter(ct, scenario == sc, variable == v),
        horizon        = "auto_max_abs",
        variable_top_k = 99,          # no reduction inside single facet
        top_n          = top_n,
        style          = "waterfall", # classic waterfall for detail
        label_top      = 99,          # not used in waterfall mode
        save_path      = fs::path(dir, glue::glue("waterfall_detail_{sc}_{v}.png")),
        dpi            = CFG$DPI
      )
    }
  }
}

# Optional metrics (D) 
metrics_paths <- character(0)  
if (isTRUE(CFG$COMPUTE_METRICS) && exists("compute_signal_metrics", mode = "function")) {
  met_dir <- fs::path(CFG$OUT_DIR, "metrics"); fs::dir_create(met_dir)
  if ("uncertainty" %in% names(res$reconciled_tbl)) {
    for (unc in sort(unique(res$reconciled_tbl$uncertainty))) {
      ct <- dplyr::filter(res$reconciled_tbl, uncertainty == unc)
      mt <- compute_signal_metrics(ct)
      p  <- fs::path(met_dir, glue("signal_metrics_{unc}.csv"))
      readr::write_csv(mt, p)
      metrics_paths[unc] <- p          
    }
  } else {
    mt <- compute_signal_metrics(res$reconciled_tbl)
    p  <- fs::path(met_dir, "signal_metrics.csv")
    readr::write_csv(mt, p)
    metrics_paths["all"] <- p          
  }
}

# Exports (CSV) 
csv_paths <- write_decomp_tables_csv(
  delta_tbl   = res$delta_tbl,
  contrib_tbl = res$reconciled_tbl,
  out_dir     = CFG$OUT_DIR
)

# Also write a simple contributions CSV for packager
readr::write_csv(res$reconciled_tbl, fs::path(CFG$OUT_DIR, "decomposition_contributions.csv"))

# LaTeX tables
if (CFG$WRITE_TEX_TABLES) {
  tex_dir <- here::here("output","tables"); fs::dir_create(tex_dir)
  if ("uncertainty" %in% names(res$reconciled_tbl)) {
    for (unc in sort(unique(res$reconciled_tbl$uncertainty))) {
      ct <- dplyr::filter(res$reconciled_tbl, uncertainty == unc)
      write_decomp_tables_tex(
        contrib_tbl      = ct,
        save_dir         = fs::path(tex_dir, glue("{unc}")),
        horizon          = NULL,      # auto: terminal per facet
        top_n            = 8,
        include_residual = TRUE
      )
      write_decomp_panels_tex(
        contrib_tbl = ct,
        save_path   = fs::path(tex_dir, glue("{unc}_decomposition_panels.tex")),
        top_n       = 10,
        include_residual = TRUE
      )
    }
  } else {
    write_decomp_tables_tex(
      contrib_tbl      = res$reconciled_tbl,
      save_dir         = tex_dir,
      horizon          = NULL,
      top_n            = 8,
      include_residual = TRUE
    )
    write_decomp_panels_tex(
      contrib_tbl = res$reconciled_tbl,
      save_path   = fs::path(tex_dir, "decomposition_panels.tex"),
      top_n       = 10,
      include_residual = TRUE
    )
  }
}

# Diagnostics (plots) 
if (CFG$MAKE_PLOTS) {
  if ("uncertainty" %in% names(res$reconciled_tbl)) {
    for (unc in sort(unique(res$reconciled_tbl$uncertainty))) {
      ct <- dplyr::filter(res$reconciled_tbl, uncertainty == unc)
      dt <- dplyr::filter(res$delta_tbl,      uncertainty == unc)
      
      plot_decomp_waterfall(
        contrib_tbl = ct,
        horizon     = "auto_max_abs",
        variable_top_k = 12,
        top_n = 8,
        label_top = 0,
        style = "auto",
        save_path   = fs::path(CFG$FIG_DIR, glue("waterfall_tH_{unc}.png")),
        dpi         = CFG$DPI
      )
      
      plot_decomp_stacked_area(
        contrib_tbl = ct,
        variable_top_k = 12,
        top_k = 8,
        save_path   = fs::path(CFG$FIG_DIR, glue("stacked_contributions_{unc}.png")),
        dpi         = CFG$DPI
      )
      
      dt <- dplyr::filter(res$delta_tbl, uncertainty == unc)
      if (dplyr::n_distinct(dt$regime) > 1) {
        plot_regime_share_bars(
          delta_tbl   = dt, metric = "abs",
          save_path   = fs::path(CFG$FIG_DIR, glue("regime_shares_abs_{unc}.png")),
          dpi         = CFG$DPI
        )
      } else {
        message("[diag] skipped regime shares for {unc}: only 'combined' regime present.")
      }
      
      export_detail_waterfalls(ct, CFG$FIG_DIR, unc, per_scenario_top = 6, top_n = 12)
      
      if (!is.null(res$.__fevd__[[unc]]) && identical(res$.__fevd__[[unc]]$mode, "irf")) {
        fevd_path <- res$.__fevd__[[unc]]$path
        if (!is.null(fevd_path) && fs::file_exists(fevd_path)) {
          fevd_tbl <- readr::read_csv(fevd_path, show_col_types = FALSE)
          if (nrow(fevd_tbl)) {
            plot_fevd_stacked_area(
              fevd_tbl   = fevd_tbl,
              save_path  = fs::path(CFG$FIG_DIR, glue("fevd_stacked_{unc}.png")),
              top_responses = 12,
              dpi        = CFG$DPI
            )
          }
        }
      }
    }
  } else {
    plot_decomp_waterfall(
      contrib_tbl = res$reconciled_tbl,
      horizon     = "auto_max_abs",   
      save_path   = fs::path(CFG$FIG_DIR, "waterfall_tH.png"),
      dpi         = CFG$DPI
    )
    plot_decomp_stacked_area(
      contrib_tbl = res$reconciled_tbl,
      save_path   = fs::path(CFG$FIG_DIR, "stacked_contributions.png"),
      dpi         = CFG$DPI
    )
    plot_regime_share_bars(
      delta_tbl   = res$delta_tbl,
      metric      = "abs",
      save_path   = fs::path(CFG$FIG_DIR, "regime_shares_abs.png"),
      dpi         = CFG$DPI
    )
  }
}

# Shapley
if (isTRUE(CFG$USE_SHAPLEY)) {
  scenario_shock_map <- shocked %>%
    dplyr::distinct(dplyr::across(dplyr::any_of(c("uncertainty", "scenario")))) %>%
    dplyr::mutate(shock_id = scenario, weight = 1)
  
  single_shock_paths <- shocked %>%
    dplyr::transmute(
      dplyr::across(dplyr::any_of(c("uncertainty"))),
      shock_id = scenario, regime, variable, t, value
    ) %>%
    dplyr::distinct()
  
  path_fun <- NULL
  
  shap_tbl <- shapley_contributions(
    baseline            = baseline,
    scenario_shock_map  = scenario_shock_map,
    single_shock_paths  = single_shock_paths,   
    path_fun            = path_fun,
    n_perm              = CFG$SHAP_N_PERM,
    exact_if_k_le       = CFG$SHAP_EXACT_IF_KLE,
    seed                = CFG$SHAP_SEED
  )
  
  reconciled_shap <- reconcile_additivity(
    contrib_tbl = shap_tbl %>% dplyr::rename(contribution = shapley),
    delta_tbl   = delta_tbl,
    method      = CFG$RECONCILE_METHOD,
    tol         = CFG$MASS_TOL
  )
  
  readr::write_csv(reconciled_shap, fs::path(CFG$OUT_DIR, "decomposition_shapley_contributions.csv"))
}

# Validation: mass balance 
if ("uncertainty" %in% names(res$reconciled_tbl)) {
  mass_ok <- TRUE
  for (unc in sort(unique(res$reconciled_tbl$uncertainty))) {
    ct <- dplyr::filter(res$reconciled_tbl, uncertainty == unc)
    dt <- dplyr::filter(res$delta_tbl,      uncertainty == unc)
    mass_chk <- validate_decomposition_mass(
      contrib_tbl = ct,
      delta_tbl   = dt,
      tol         = CFG$MASS_TOL,
      mode        = "report"
    )
    if (!mass_chk$ok) {
      mass_ok <- FALSE
      bad <- dplyr::filter(mass_chk$residuals_tbl, !within_tol)
      readr::write_csv(bad, fs::path(CFG$LOG_DIR, glue("mass_balance_violations_{unc}.csv")))
    }
  }
  if (!mass_ok) warning("[08] Mass balance failed for at least one uncertainty. See logs.")
} else {
  mass_chk <- validate_decomposition_mass(
    contrib_tbl = res$reconciled_tbl,
    delta_tbl   = res$delta_tbl,
    tol         = CFG$MASS_TOL,
    mode        = "report"
  )
  if (!mass_chk$ok) {
    bad <- dplyr::filter(mass_chk$residuals_tbl, !within_tol)
    readr::write_csv(bad, fs::path(CFG$LOG_DIR, "mass_balance_violations.csv"))
  }
}

# Logging 
elapsed <- proc.time()[["elapsed"]] - t_start

# Collect FEVD CSV paths
fevd_paths <- character(0)
if (!is.null(res$.__fevd__)) {
  if (is.list(res$.__fevd__) && !is.null(names(res$.__fevd__))) {
    fevd_paths <- unlist(lapply(res$.__fevd__, function(x) x$path %||% NA_character_), use.names = FALSE)
  } else if (!is.null(res$.__fevd__$path)) {
    fevd_paths <- res$.__fevd__$path
  }
  fevd_paths <- fevd_paths[!is.na(fevd_paths) & nzchar(fevd_paths)]
}

# Build output_paths as a pure character vector 
output_paths <- c(
  delta_csv   = fs::path(CFG$OUT_DIR, "forecast_decomposition_delta.csv"),
  summary_csv = fs::path(CFG$OUT_DIR, "forecast_decomposition_summary.csv"),
  contrib_csv = fs::path(CFG$OUT_DIR, "decomposition_contributions.csv")
)
# append FEVD + metrics
if (length(fevd_paths)) {
  names(fevd_paths) <- paste0("fevd_", seq_along(fevd_paths))
  output_paths <- c(output_paths, fevd_paths)
}
if (length(metrics_paths)) {
  names(metrics_paths) <- if (is.null(names(metrics_paths))) paste0("metrics_", seq_along(metrics_paths))
  else paste0("metrics_", names(metrics_paths))
  output_paths <- c(output_paths, metrics_paths)
}

fevd_meta <-
  if (!is.null(res$.__fevd__)) {
    if (is.list(res$.__fevd__) && !is.null(names(res$.__fevd__))) {
      lapply(res$.__fevd__, function(x) list(mode = x$mode, path = x$path))
    } else {
      list(mode = res$.__fevd__$mode, path = res$.__fevd__$path)
    }
  } else NULL

meta <- list(
  step            = "08_forecast_decomposition",
  method          = if (tolower(CFG$FEVD_MODE) == "trivial") "fevd(trivial)" else "fevd(irf)",
  fevd_regimes    = CFG$FEVD_REGIMES,
  fevd_meta       = fevd_meta,
  reconcile       = CFG$RECONCILE_METHOD,
  tol             = CFG$MASS_TOL,
  scenarios       = sort(unique(res$delta_tbl$scenario)),
  variables       = sort(unique(res$delta_tbl$variable)),
  regimes         = sort(unique(res$delta_tbl$regime)),
  uncertainties   = if ("uncertainty" %in% names(res$delta_tbl)) sort(unique(res$delta_tbl$uncertainty)) else NULL,
  horizon         = max(res$delta_tbl$t, na.rm = TRUE),
  n_rows_delta    = nrow(res$delta_tbl),
  n_rows_contrib  = nrow(res$reconciled_tbl),
  n_shocks        = length(unique(res$reconciled_tbl$shock_id)),
  runtime_sec     = round(elapsed, 3),
  seed            = CFG$SHAP_SEED,
  notes           = paste0(
    "Plots=", CFG$MAKE_PLOTS,
    "; TEX=", CFG$WRITE_TEX_TABLES,
    "; METRICS=", CFG$COMPUTE_METRICS,
    "; SHAPLEY=", CFG$USE_SHAPLEY, "."
  ),
  input_paths     = c(tidy_path),
  output_paths    = output_paths    
)

log_decomp_run_md(meta, save_dir = CFG$LOG_DIR)
log_decomp_metadata_json(meta, save_dir = CFG$LOG_DIR, include_hashes = TRUE)

message("[08] done in ", round(elapsed, 2), "s.")