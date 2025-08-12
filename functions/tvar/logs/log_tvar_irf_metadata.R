log_tvar_irf_metadata <- function(model,
                                  output_dir = "output/logs/",
                                  log_file_json = "tvar_irf_metadata.json",
                                  log_file_md = "tvar_irf_metadata.md",
                                  verbose = TRUE) {
  if (is.null(model$irf)) {
    stop("model$irf is NULL. Run compute_tvar_irf() or run_tvar_irf_analysis() first.")
  }
  
  fs::dir_create(output_dir)
  
  # Extract global IRF attributes 
  horizon     <- attr(model$irf, "horizon", exact = TRUE)
  n_draws     <- attr(model$irf, "n_draws", exact = TRUE)
  ci_level    <- attr(model$irf, "ci_level", exact = TRUE)
  shock_type  <- attr(model$irf, "shock_type", exact = TRUE)
  shock_size  <- attr(model$irf, "shock_size", exact = TRUE)
  timestamp   <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Flatten the IRF structure into a tibble 
  irf_pairs <- purrr::map_dfr(
    .x = names(model$irf),
    .f = function(regime) {
      impulses <- model$irf[[regime]]
      purrr::map_dfr(
        .x = names(impulses),
        .f = function(impulse) {
          responses <- dimnames(impulses[[impulse]])[[2]]
          tibble::tibble(
            regime = regime,
            impulse = impulse,
            response = responses
          )
        }
      )
    }
  )
  
  # Save metadata to JSON 
  metadata <- list(
    horizon     = horizon,
    n_draws     = n_draws,
    ci_level    = ci_level,
    shock_type  = shock_type,
    shock_size  = shock_size,
    timestamp   = timestamp,
    irf_pairs   = irf_pairs
  )
  
  jsonlite::write_json(
    metadata,
    path = file.path(output_dir, log_file_json),
    pretty = TRUE,
    auto_unbox = TRUE
  )
  
  # Create grouped markdown table
  grouped_md <- irf_pairs |>
    dplyr::mutate(pair = paste0(impulse, " → ", response)) |>
    dplyr::group_by(regime) |>
    dplyr::summarise(pairs = paste(pair, collapse = ", "), .groups = "drop")
  
  md_lines <- c(
    "# TVAR IRF Metadata Log",
    paste0("- **Timestamp:** ", timestamp),
    paste0("- **Horizon:** ", horizon),
    paste0("- **Draws:** ", n_draws),
    paste0("- **Confidence Level:** ", ci_level),
    paste0("- **Shock Type:** ", shock_type),
    paste0("- **Shock Size:** ", shock_size),
    "",
    "## Impulse–Response Pairs by Regime",
    purrr::pmap_chr(grouped_md, function(regime, pairs) {
      paste0("**", regime, "**: ", pairs)
    })
  )
  
  readr::write_lines(md_lines, file.path(output_dir, log_file_md))
  
  if (verbose) {
    cat(cli::cli_alert_success("IRF metadata logged to:\n- {.path {log_file_json}}\n- {.path {log_file_md}}"))
  }
  
  return(invisible(metadata))
}