# functions/scenarios/simulate/simulate_scenarios_batch.R

#' Run a batch of YAML scenarios against a TVAR/VAR model
#'
#' For each YAML file:
#'   1) Load + validate spec
#'   2) Set deterministic RNG (from spec$seed or spec$name)
#'   3) Build no-shock baseline and shocked paths (IRF superposition)
#'   4) Assemble tidy output, save per-scenario artifacts (optional)
#'
#' Parallelism (optional): if `parallel = TRUE` and package `furrr` is installed,
#' scenarios are evaluated in parallel. Otherwise falls back to sequential.
#'
#' @param model List-like model with fields: $variables (char K), $irf (nested list),
#'        optionally $sigma (named numeric or list by regime), $Sigma, $regimes.
#' @param scenario_files Character vector of paths to *.yml/*.yaml files, or a single
#'        directory path (in which case all yml/yaml files are discovered).
#' @param out_dir Directory to write CSV artifacts (per-scenario) and the combined CSV (optional).
#' @param log_dir Directory to write run logs/metadata digests.
#' @param compute_bands Logical; if TRUE, call attach_confidence_bands() on combined tidy output.
#' @param band_levels Numeric vector of CI levels, e.g., c(0.90). Only used if compute_bands=TRUE.
#' @param band_method "percentile"|"normal". Only used if compute_bands=TRUE.
#' @param band_center "mean"|"median". Only used if compute_bands=TRUE.
#' @param save_each Logical; write a CSV per scenario to out_dir. Default TRUE.
#' @param save_combined Logical; write a combined tidy CSV to out_dir. Default FALSE.
#' @param parallel Logical; use furrr for parallel execution when available. Default FALSE.
#' @param workers Optional integer; number of workers when parallel=TRUE (if NULL, furrr default).
#' @param error_policy "stop" to error on first failure; "continue" to collect errors but continue.
#' @param verbose Logical; emit progress/info messages.
#'
#' @return A list with:
#'   - tidy: tibble (long panel) with t, variable, regime, scenario, value, is_baseline (+ bands if requested)
#'   - runs: list of per-scenario results with fields {spec, meta, baseline, shocked, tidy}
#'   - errors: tibble of any per-scenario errors (empty if none)
#'   - meta: list with batch-level metadata (timestamps, n_scenarios, parallel details)
#' @export
simulate_scenarios_batch <- function(
    model,
    scenario_files,
    out_dir,
    log_dir,
    compute_bands = FALSE,
    band_levels   = 0.90,
    band_method   = c("percentile","normal"),
    band_center   = c("mean","median"),
    save_each     = TRUE,
    save_combined = FALSE,
    parallel      = FALSE,
    workers       = NULL,
    error_policy  = c("continue","stop"),
    verbose       = TRUE
) {
  band_method  <- match.arg(band_method)
  band_center  <- match.arg(band_center)
  error_policy <- match.arg(error_policy)
  
  `%||%` <- function(x,y) if (is.null(x)) y else x
  
  # ---- sanity & setup --------------------------------------------------------
  if (missing(model) || !is.list(model))
    stop("`model` must be a list-like object with $variables and $irf.")
  variables <- model$variables
  if (is.null(variables) || !is.character(variables) || !length(variables))
    stop("`model$variables` must be a non-empty character vector.")
  
  # resolve scenario files
  files <- scenario_files
  if (length(files) == 1L && dir.exists(files)) {
    files <- fs::dir_ls(files, regexp = "\\.(yml|yaml)$", type = "file")
  }
  files <- files[fs::file_exists(files)]
  if (!length(files)) stop("No scenario YAML files found.")
  
  fs::dir_create(out_dir)
  fs::dir_create(log_dir)
  
  if (verbose) message("[batch] Found ", length(files), " scenario file(s).")
  
  # choose execution backend
  exec_map <- function(.x, .f) purrr::map(.x, .f)
  if (isTRUE(parallel) && requireNamespace("furrr", quietly = TRUE)) {
    if (!is.null(workers)) {
      if (requireNamespace("future", quietly = TRUE)) {
        future::plan(future::multisession, workers = as.integer(workers))
      } else {
        warning("Package 'future' not available; using furrr default plan.")
      }
    }
    exec_map <- function(.x, .f) furrr::future_map(.x, .f, .progress = verbose)
    if (verbose) message("[batch] Parallel execution enabled (furrr).")
  } else if (isTRUE(parallel)) {
    warning("parallel=TRUE requested but 'furrr' not installed; running sequentially.")
  }
  
  # per-scenario runner (with error capture)
  run_one <- function(fp) {
    tryCatch({
      spec  <- load_scenario_yaml(fp)
      spec  <- validate_scenario_spec(spec, model_variables = variables)
      # deterministic RNG: prefer spec$seed, else use name-based seed
      seed_key <- spec$seed %||% spec$name %||% basename(fp)
      rng      <- seed_control(seed = seed_key, key = paste0("scenario:", spec$name %||% basename(fp)))
      on.exit(rng$restore(), add = TRUE)
      
      H <- as.integer(spec$horizon)
      # Baseline path
      baseline <- generate_counterfactual_baseline(
        model   = model,
        horizon = H,
        variables = variables,
        method  = "irf_zero"
      )
      # Shocked path
      shocked <- simulate_structural_shock(
        model     = model,
        variables = variables,
        scenario  = spec,
        sigma     = model$sigma %||% NULL,
        responses = spec$responses %||% NULL,
        identification = spec$identification %||% "unit",
        verbose   = FALSE
      )
      
      # Assemble tidy for this scenario
      tidy_one <- dplyr::bind_rows(
        baseline |> dplyr::mutate(regime = attr(shocked, "meta")$regime_key %||% "combined",
                                  scenario = spec$name, is_baseline = TRUE),
        shocked  |> dplyr::mutate(is_baseline = FALSE)
      )
      
      # Save per-scenario CSV (optional)
      if (isTRUE(save_each)) {
        out_fp <- file.path(out_dir, paste0(spec$name, "_tidy.csv"))
        readr::write_csv(tidy_one, out_fp)
      }
      
      # Minimal machine metadata for this scenario
      meta <- list(
        file         = fp,
        scenario     = spec$name,
        horizon      = H,
        identification = spec$identification,
        regime_conditioning = spec$regime_conditioning %||% "none",
        seed_meta    = rng$meta,
        A_meta       = attr(shocked, "meta")$A_meta %||% NULL,
        created_at   = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
      )
      
      list(
        ok       = TRUE,
        spec     = spec,
        meta     = meta,
        baseline = baseline,
        shocked  = shocked,
        tidy     = tidy_one
      )
    }, error = function(e) {
      list(
        ok     = FALSE,
        file   = fp,
        error  = conditionMessage(e),
        trace  = paste(capture.output(print(sys.calls())), collapse = "\n")
      )
    })
  }
  
  # execute
  res_list <- exec_map(files, run_one)
  
  # split success/errors
  ok_idx <- vapply(res_list, function(x) isTRUE(x$ok), logical(1))
  successes <- res_list[ok_idx]
  failures  <- res_list[!ok_idx]
  
  if (length(failures)) {
    err_tbl <- tibble::tibble(
      file  = vapply(failures, `[[`, character(1), "file"),
      error = vapply(failures, `[[`, character(1), "error")
    )
    readr::write_csv(err_tbl, file.path(log_dir, paste0("scenario_errors_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")))
    if (identical(error_policy, "stop")) {
      stop("One or more scenarios failed:\n", paste(err_tbl$file, err_tbl$error, sep = " :: ", collapse = "\n"))
    } else if (verbose) {
      message("[batch] ", nrow(err_tbl), " scenario(s) failed; continuing.")
    }
  } else {
    err_tbl <- tibble::tibble(file = character(0), error = character(0))
  }
  
  if (!length(successes)) {
    stop("No scenarios completed successfully.")
  }
  
  # assemble combined tidy
  tidy <- dplyr::bind_rows(purrr::map(successes, `[[`, "tidy"))
  
  # optional confidence bands on combined tidy
  if (isTRUE(compute_bands)) {
    tidy <- attach_confidence_bands(
      tidy,
      levels = band_levels,
      method = band_method,
      center = band_center,
      include_baseline = FALSE
    )
  }
  
  # save combined tidy (optional)
  if (isTRUE(save_combined)) {
    readr::write_csv(tidy, file.path(out_dir, "scenario_results_tidy.csv"))
  }
  
  # write machine metadata JSON (all scenarios)
  all_meta <- purrr::map(successes, `[[`, "meta")
  jsonlite::write_json(
    all_meta,
    file.path(log_dir, paste0("scenario_metadata_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")),
    auto_unbox = TRUE, pretty = TRUE
  )
  
  # quick human log
  human_log <- c(
    "# Scenario Batch Summary",
    paste0("- Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
    paste0("- Scenarios OK / Total: ", length(successes), " / ", length(files)),
    paste0("- Parallel: ", isTRUE(parallel)),
    paste0("- Variables: ", paste(model$variables, collapse = ", "))
  )
  readr::write_lines(human_log, file.path(log_dir, paste0("scenario_run_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".md")))
  
  # batch meta
  batch_meta <- list(
    n_files     = length(files),
    n_success   = length(successes),
    n_failed    = length(failures),
    parallel    = isTRUE(parallel),
    workers     = workers %||% NA_integer_,
    band_levels = if (compute_bands) band_levels else NULL,
    created_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  
  list(
    tidy   = tidy,
    runs   = successes,
    errors = err_tbl,
    meta   = batch_meta
  )
}