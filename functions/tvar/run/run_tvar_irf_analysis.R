# functions/tvar/run/run_tvar_irf_analysis.R
# Orchestrates IRF computation + exports tidy CSVs and basic panels

run_tvar_irf_analysis <- function(
    model,
    horizon = 12L,
    n_draws = 1000L,
    ci_level = 0.90,
    shock_type = c("unit","sd"),
    shock_size = 1,
    output_dir = "output/plots/irfs",
    save_csv = TRUE,
    seed = 42L,
    verbose = TRUE
) {
  shock_type <- match.arg(shock_type)
  if (!is.null(seed)) set.seed(seed)
  fs::dir_create(output_dir)
  
  model <- compute_tvar_irf(
    model            = model,
    horizon          = horizon,
    n_draws          = n_draws,
    ci_level         = ci_level,
    shock_type       = shock_type,
    shock_size       = shock_size,
    impulse_variable = NULL,
    response_variable= NULL,
    seed             = seed,
    progress         = TRUE,
    verbose          = verbose,
    save_dir         = if (isTRUE(save_csv)) output_dir else NULL
  )
  
  # Optional: quick default panel for first impulse/response
  imp0 <- model$irf$settings$impulses[1]
  resp0<- model$irf$settings$responses[1]
  try({
    plot_tvar_irf_regimes(
      model = model,
      impulse = imp0,
      response = resp0,
      horizon = horizon,
      ci_level = ci_level,
      save_path = file.path(output_dir, glue::glue("tvar_irf_regimes_{imp0}_to_{resp0}.png")),
      verbose = verbose
    )
  }, silent = TRUE)
  
  return(model)
}