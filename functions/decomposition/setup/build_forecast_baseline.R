# functions/decomposition/setup/build_forecast_baseline.R
# Build baseline (no-shock) forecast paths for decomposition
#
# Prefers cached baseline rows from scenario_results_tidy_*.csv (step 07).
# If absent, can optionally compute a no-shock forecast from a supplied TVAR model.
#
# Input (cached tidy from 07) must contain:
#   scenario, regime, variable, t, is_baseline, value
# If present, the 'uncertainty' column is preserved and becomes part of the
# uniqueness key (so multi-uncertainty runs don't collide).
#
# Output columns:
#   [uncertainty?], regime, variable, t, value

build_forecast_baseline <- function(
    scenario_tidy = here::here("data","scenarios","scenario_results_tidy_fast.csv"),
    prefer_cached = TRUE,
    model        = NULL,
    forecast_fun = NULL,
    init_state   = NULL,
    horizon      = NULL,
    variables    = NULL,
    out_path     = here::here("data","scenarios","baseline_forecast.csv"),
    log_dir      = here::here("logs","decomposition"),
    overwrite    = TRUE
) {
  # --- deps
  requireNamespace("fs", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("glue", quietly = TRUE)
  requireNamespace("jsonlite", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  
  `%||%` <- function(x,y) if (is.null(x)) y else x
  
  # --- tiny helpers
  .is_path <- function(x) is.character(x) && length(x) == 1 && !is.na(x)
  .read_tidy <- function(x) {
    if (.is_path(x)) {
      if (!fs::file_exists(x)) stop(glue::glue("[baseline] scenario file not found: {x}"))
      readr::read_csv(x, show_col_types = FALSE)
    } else {
      tibble::as_tibble(x)
    }
  }
  .require_cols <- function(df, cols) {
    miss <- setdiff(cols, names(df))
    if (length(miss)) stop(glue::glue("[baseline] missing required columns: {paste(miss, collapse=', ')}"))
  }
  .write_csv_safely <- function(df, path, overwrite = TRUE) {
    fs::dir_create(fs::path_dir(path))
    if (fs::file_exists(path) && !overwrite) stop(glue::glue("[baseline] file exists: {path}"))
    readr::write_csv(df, path)
    path
  }
  .log_minimal <- function(info, dir) {
    fs::dir_create(dir)
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    md_p  <- fs::path(dir, glue::glue("build_forecast_baseline_{stamp}.md"))
    js_p  <- fs::path(dir, glue::glue("build_forecast_baseline_{stamp}.json"))
    md <- paste0(
      "# build_forecast_baseline\n\n",
      "*Created:* ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n\n",
      "- Source: ", info$source, "\n",
      "- Uncertainties: ", paste(info$uncertainties %||% "(none)", collapse = ", "), "\n",
      "- N rows: ", info$n_rows, "\n",
      "- Vars: ", paste(info$variables, collapse = ", "), "\n",
      "- Horizon: ", info$horizon %||% "(NA)", "\n",
      "- Regimes: ", paste(info$regimes, collapse = ", "), "\n",
      "- Path: ", info$out_path, "\n"
    )
    cat(md, file = md_p, sep = "")
    jsonlite::write_json(info, js_p, pretty = TRUE, auto_unbox = TRUE)
    invisible(list(md = md_p, json = js_p))
  }
  
  # --- 1) Try to get cached baseline from scenario tidy
  baseline_df <- NULL
  source_label <- NULL
  
  if (!is.null(scenario_tidy) && prefer_cached) {
    tidy <- .read_tidy(scenario_tidy)
    .require_cols(tidy, c("scenario","regime","variable","t","value","is_baseline"))
    
    # Keep only baseline rows
    base_rows <- dplyr::filter(tidy, is_baseline %in% c(TRUE, 1))
    
    # Optional variable filter (before aggregation)
    if (!is.null(variables)) base_rows <- dplyr::filter(base_rows, variable %in% variables)
    
    if (nrow(base_rows) > 0) {
      has_unc <- "uncertainty" %in% names(base_rows)
      
      # Aggregate across scenarios; keep per-uncertainty when present
      if (has_unc) {
        base_core <- base_rows |>
          dplyr::group_by(uncertainty, regime, variable, t) |>
          dplyr::summarise(value = median(value, na.rm = TRUE), .groups = "drop")
        # Order & select
        sel_cols <- c("uncertainty","regime","variable","t","value")
        base_core <- base_core |>
          dplyr::select(dplyr::all_of(sel_cols)) |>
          dplyr::arrange(dplyr::across(dplyr::all_of(c("uncertainty","variable","regime","t"))))
      } else {
        base_core <- base_rows |>
          dplyr::group_by(regime, variable, t) |>
          dplyr::summarise(value = median(value, na.rm = TRUE), .groups = "drop") |>
          dplyr::select(dplyr::all_of(c("regime","variable","t","value"))) |>
          dplyr::arrange(dplyr::across(dplyr::all_of(c("variable","regime","t"))))
      }
      
      baseline_df <- base_core
      source_label <- "cached"
    }
  }
  
  # --- 2) Fallback: compute from model (no-shock path)
  if (is.null(baseline_df)) {
    if (is.null(model) || is.null(forecast_fun)) {
      stop("[baseline] No cached baseline found and no (model, forecast_fun) provided.")
    }
    if (is.null(horizon) || !is.numeric(horizon) || horizon < 1) {
      stop("[baseline] Provide a positive integer 'horizon' for model-based fallback.")
    }
    
    fc <- forecast_fun(model = model, h = horizon, init_state = init_state, variables = variables)
    # expected columns: t, variable, value, (optional) regime
    .require_cols(fc, c("t","variable","value"))
    if (!"regime" %in% names(fc)) {
      fc$regime <- "mixed"  # if your no-shock path does not condition on regimes
    }
    
    baseline_df <- tibble::as_tibble(fc) |>
      dplyr::select(dplyr::all_of(intersect(c("uncertainty","regime","variable","t","value"), names(fc)))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(intersect(c("uncertainty","variable","regime","t"), names(fc)))))
    
    source_label <- "model_fallback"
  }
  
  # --- 3) Validate: unique keys, no duplicate combos
  key_cols <- intersect(c("uncertainty","regime","variable","t"), names(baseline_df))
  dup_ct <- baseline_df |>
    dplyr::count(dplyr::across(dplyr::all_of(key_cols))) |>
    dplyr::filter(n > 1) |>
    nrow()
  if (dup_ct > 0) {
    stop(glue::glue("[baseline] duplicate ({paste(key_cols, collapse=', ')}) rows: {dup_ct}"))
  }
  
  # --- 4) Persist canonical CSV
  out_path <- .write_csv_safely(baseline_df, out_path, overwrite = overwrite)
  
  # --- 5) Log + return
  meta <- list(
    source        = source_label,
    uncertainties = sort(unique(baseline_df$uncertainty)) %||% NULL,
    n_rows        = nrow(baseline_df),
    variables     = sort(unique(baseline_df$variable)),
    regimes       = sort(unique(baseline_df$regime)),
    horizon       = max(baseline_df$t, na.rm = TRUE),
    out_path      = out_path
  )
  .log_minimal(meta, log_dir)
  
  list(data = baseline_df, path = out_path, meta = meta)
}