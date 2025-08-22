# functions/decomposition/setup/build_forecast_with_shocks.R
# Build shocked forecast paths for decomposition
#
# Expects scenario_tidy (path or tibble) from 07 with columns at least:
#   scenario, regime, variable, t, is_baseline, value
# If present, the 'uncertainty' column is preserved and becomes part of the
# uniqueness key (avoids duplicates when multiple uncertainty sources exist).

build_forecast_with_shocks <- function(
    scenario_tidy = here::here("data","scenarios","scenario_results_tidy_fast.csv"),
    scenarios     = NULL,
    prefer_cached = TRUE,
    model         = NULL,
    forecast_fun  = NULL,
    shocks        = NULL,
    init_state    = NULL,
    horizon       = NULL,
    variables     = NULL,
    out_path      = here::here("data","scenarios","shocked_forecast.csv"),
    log_dir       = here::here("logs","decomposition"),
    overwrite     = TRUE
) {
  requireNamespace("fs", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("glue", quietly = TRUE)
  requireNamespace("jsonlite", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  
  `%||%` <- function(x,y) if (is.null(x)) y else x
  .is_path <- function(x) is.character(x) && length(x) == 1 && !is.na(x)
  .read_tidy <- function(x) {
    if (.is_path(x)) {
      if (!fs::file_exists(x)) stop(glue::glue("[shocked] scenario file not found: {x}"))
      readr::read_csv(x, show_col_types = FALSE)
    } else {
      tibble::as_tibble(x)
    }
  }
  .require_cols <- function(df, cols) {
    miss <- setdiff(cols, names(df))
    if (length(miss)) stop(glue::glue("[shocked] missing required columns: {paste(miss, collapse=', ')}"))
  }
  .write_csv_safely <- function(df, path, overwrite = TRUE) {
    fs::dir_create(fs::path_dir(path))
    if (fs::file_exists(path) && !overwrite) stop(glue::glue("[shocked] file exists: {path}"))
    readr::write_csv(df, path)
    path
  }
  .log_minimal <- function(info, dir) {
    fs::dir_create(dir)
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    md_p  <- fs::path(dir, glue::glue("build_forecast_with_shocks_{stamp}.md"))
    js_p  <- fs::path(dir, glue::glue("build_forecast_with_shocks_{stamp}.json"))
    md <- paste0(
      "# build_forecast_with_shocks\n\n",
      "*Created:* ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n\n",
      "- Source: ", info$source, "\n",
      "- Scenarios: ", paste(info$scenarios, collapse = ", "), "\n",
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
  
  shocked_df <- NULL
  source_label <- NULL
  scen_used <- NULL
  
  if (!is.null(scenario_tidy) && prefer_cached) {
    tidy <- .read_tidy(scenario_tidy)
    .require_cols(tidy, c("scenario","regime","variable","t","value","is_baseline"))
    
    # Keep shocked rows only
    tidy <- dplyr::filter(tidy, !(isTRUE(is_baseline) | is_baseline %in% c(TRUE, 1)))
    
    # Optional scenario filter
    if (!is.null(scenarios)) tidy <- dplyr::filter(tidy, scenario %in% scenarios)
    
    # Optional variable filter
    if (!is.null(variables)) tidy <- dplyr::filter(tidy, variable %in% variables)
    
    if (nrow(tidy) > 0) {
      has_unc <- "uncertainty" %in% names(tidy)
      # Selection order and stable arrange
      sel_cols <- c(if (has_unc) "uncertainty", "scenario","regime","variable","t","value")
      ord_cols <- intersect(c("uncertainty","scenario","variable","regime","t"), sel_cols)
      
      shocked_df <- tidy |>
        dplyr::select(dplyr::all_of(sel_cols)) |>
        dplyr::arrange(dplyr::across(dplyr::all_of(ord_cols)))
      
      # Uniqueness check with correct key
      key_cols <- c("scenario","regime","variable","t")
      if (has_unc) key_cols <- c("uncertainty", key_cols)
      
      dup_ct <- shocked_df |>
        dplyr::count(dplyr::across(dplyr::all_of(key_cols))) |>
        dplyr::filter(n > 1) |>
        nrow()
      
      if (dup_ct > 0) {
        stop(glue::glue("[shocked] duplicate ({paste(key_cols, collapse=', ')}) rows: {dup_ct}"))
      }
      
      scen_used <- sort(unique(shocked_df$scenario))
      source_label <- "cached"
    }
  }
  
  # Fallback: build via model + forecast_fun (rarely used in your flow)
  if (is.null(shocked_df)) {
    if (is.null(model) || is.null(forecast_fun)) {
      stop("[shocked] No cached shocked forecasts found and no (model, forecast_fun) provided.")
    }
    if (is.null(horizon) || !is.numeric(horizon) || horizon < 1) {
      stop("[shocked] Provide a positive integer 'horizon' for model-based fallback.")
    }
    fc <- forecast_fun(model = model, h = horizon, init_state = init_state, variables = variables, shocks = shocks)
    .require_cols(fc, c("t","variable","value"))
    if (!"regime" %in% names(fc)) fc$regime <- "mixed"
    if (!"scenario" %in% names(fc)) fc$scenario <- "scenario_1"
    
    sel_cols <- intersect(c("uncertainty","scenario","regime","variable","t","value"), names(fc))
    ord_cols <- intersect(c("uncertainty","scenario","variable","regime","t"), sel_cols)
    
    shocked_df <- tibble::as_tibble(fc) |>
      dplyr::select(dplyr::all_of(sel_cols)) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(ord_cols)))
    
    # Uniqueness check
    key_cols <- intersect(c("uncertainty","scenario","regime","variable","t"), names(shocked_df))
    dup_ct <- shocked_df |>
      dplyr::count(dplyr::across(dplyr::all_of(key_cols))) |>
      dplyr::filter(n > 1) |>
      nrow()
    if (dup_ct > 0) {
      stop(glue::glue("[shocked] duplicate ({paste(key_cols, collapse=', ')}) rows: {dup_ct}"))
    }
    
    scen_used <- sort(unique(shocked_df$scenario))
    source_label <- "model_fallback"
  }
  
  out_path <- .write_csv_safely(shocked_df, out_path, overwrite = overwrite)
  
  meta <- list(
    source        = source_label,
    scenarios     = scen_used,
    uncertainties = sort(unique(shocked_df$uncertainty)) %||% NULL,
    n_rows        = nrow(shocked_df),
    variables     = sort(unique(shocked_df$variable)),
    regimes       = sort(unique(shocked_df$regime)),
    horizon       = max(shocked_df$t, na.rm = TRUE),
    out_path      = out_path
  )
  .log_minimal(meta, log_dir)
  
  list(data = shocked_df, path = out_path, meta = meta)
}
