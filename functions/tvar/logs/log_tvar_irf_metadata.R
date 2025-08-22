# functions/tvar/logs/log_tvar_irf_metadata.R
# Robust IRF metadata logger. Works with model$irf (preferred) and/or saved CSVs.

log_tvar_irf_metadata <- function(
    model,
    output_dir   = "output/logs",
    log_file_json = "tvar_irf_metadata.json",
    log_file_md   = "tvar_irf_metadata.md",
    verbose = TRUE
) {
  stopifnot(!is.null(model))
  fs::dir_create(output_dir)
  
  # Helper: safe get
  safe <- function(x, default = NULL) if (is.null(x)) default else x
  
  # Pull core settings from model$irf 
  irf        <- safe(model$irf)
  settings   <- safe(irf$settings, list())
  impulses   <- safe(settings$impulses, character())
  responses  <- safe(settings$responses, character())
  horizon    <- safe(settings$horizon, NA_integer_)
  n_draws    <- safe(settings$n_draws, NA_integer_)
  ci_level   <- safe(settings$ci_level, NA_real_)
  shock_type <- safe(settings$shock_type, NA_character_)
  shock_size <- safe(settings$shock_size, NA_real_)
  seed       <- safe(settings$seed, NA_integer_)
  created_at <- as.character(safe(irf$created_at, Sys.time()))
  
  # Regime diagnostics 
  regimes <- safe(irf$regimes, list(low = list(), high = list()))
  # spectral radius per regime (if A exists)
  spec_radius <- function(A) {
    if (is.null(A)) return(NA_real_)
    k <- nrow(A); p <- ncol(A) / k
    comp <- matrix(0, nrow = k * p, ncol = k * p)
    comp[1:k, ] <- A
    if (p > 1) comp[(k + 1):(k * p), 1:(k * (p - 1))] <- diag(k * (p - 1))
    max(Mod(eigen(comp, only.values = TRUE)$values))
  }
  
  A_low  <- safe(model$regimes$low$A)
  A_high <- safe(model$regimes$high$A)
  rho_low  <- spec_radius(A_low)
  rho_high <- spec_radius(A_high)
  
  # regime sample sizes if present
  n_low  <- if (!is.null(model$regimes$low$Y))  nrow(model$regimes$low$Y)  else NA_integer_
  n_high <- if (!is.null(model$regimes$high$Y)) nrow(model$regimes$high$Y) else NA_integer_
  
  # Build tidy impulse–response index from settings
  ir_pairs <- tidyr::expand_grid(
    impulse  = if (length(impulses)) impulses else character(),
    response = if (length(responses)) responses else character()
  ) |>
    dplyr::mutate(pair = paste0(.data$impulse, " → ", .data$response))
  
  # If you saved CSVs via run_tvar_irf_analysis(), we can record which pairs were exported
  csv_dir <- dirname(file.path(output_dir, "..", "plots", "irfs", "dummy.txt"))
  csv_dir <- normalizePath(file.path(output_dir, "..", "plots", "irfs"), mustWork = FALSE)
  exported_pairs <- NULL
  if (dir.exists(csv_dir)) {
    csvs <- list.files(csv_dir, pattern = "^irf_(low|high)_.+_to_.+\\.csv$", full.names = TRUE)
    if (length(csvs)) {
      exported_pairs <- unique(gsub("^.*irf_(low|high)_([^_]+)_to_([^\\.]+)\\.csv$", "\\2 → \\3", csvs))
      ir_pairs <- ir_pairs |>
        dplyr::mutate(exported_csv = .data$pair %in% exported_pairs)
    }
  }
  
  # Summaries per regime 
  has_low  <- !is.null(regimes$low$summary)
  has_high <- !is.null(regimes$high$summary)
  
  #  Compose metadata object 
  meta <- list(
    created_at  = created_at,
    settings    = list(
      horizon    = horizon,
      n_draws    = n_draws,
      ci_level   = ci_level,
      shock_type = shock_type,
      shock_size = shock_size,
      seed       = seed
    ),
    variables   = list(
      impulses  = impulses,
      responses = responses
    ),
    regimes = list(
      low  = list(n = n_low,  spectral_radius = rho_low,  has_summary = has_low),
      high = list(n = n_high, spectral_radius = rho_high, has_summary = has_high)
    ),
    pairs = ir_pairs
  )
  
  # ---- Write JSON ----
  json_path <- file.path(output_dir, log_file_json)
  jsonlite::write_json(meta, path = json_path, auto_unbox = TRUE, pretty = TRUE)
  
  # ---- Write Markdown summary ----
  md_path <- file.path(output_dir, log_file_md)
  md <- c(
    "# TVAR IRF Metadata",
    "",
    glue::glue("- Created: **{created_at}**"),
    glue::glue("- Horizon: **{horizon}**  |  Draws: **{n_draws}**  |  CI: **{round(ci_level*100)}%**"),
    glue::glue("- Shock: **{shock_type}** (size = {shock_size})  |  Seed: **{seed}**"),
    "",
    "## Variables",
    glue::glue("- Impulses: `{paste(impulses, collapse=', ')}`"),
    glue::glue("- Responses: `{paste(responses, collapse=', ')}`"),
    "",
    "## Regimes",
    glue::glue("- Low:  n = {n_low},  spectral radius ≈ {round(rho_low, 6)},  summary: {has_low}"),
    glue::glue("- High: n = {n_high}, spectral radius ≈ {round(rho_high, 6)}, summary: {has_high}"),
    "",
    "## Impulse–Response Pairs",
    if (nrow(ir_pairs)) {
      paste0("- ", ir_pairs$pair,
             if ("exported_csv" %in% colnames(ir_pairs)) ifelse(ir_pairs$exported_csv, " (csv)", ""), 
             collapse = "\n")
    } else {
      "_No pairs available — did IRFs run?_"
    },
    ""
  )
  writeLines(md, con = md_path)
  
  if (isTRUE(verbose)) {
    message("IRF metadata logged to:\n- ", basename(json_path), "\n- ", basename(md_path))
  }
  invisible(list(json_path = json_path, md_path = md_path))
}