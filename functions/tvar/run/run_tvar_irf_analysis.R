#' Run TVAR IRF Analysis for All Impulse–Response Pairs
#'
#' This function computes and plots IRFs for all impulse–response pairs
#' across both regimes ("low", "high") of a threshold VAR model.
#' IRFs are saved as PNG plots and optionally exported as CSV.
#'
#' @param model A threshold VAR model object returned by `estimate_tvar_model()`.
#' @param horizon Integer. Forecast horizon (number of periods ahead).
#' @param n_draws Integer. Number of draws for simulation.
#' @param ci_level Numeric. Confidence interval level (e.g. 0.90 for 90% CI).
#' @param shock_type Character. Type of shock: `"unit"` or `"random"`.
#' @param shock_size Numeric. Size of the shock to apply.
#' @param output_dir Character. Directory to save plots and CSVs.
#' @param save_csv Logical. Whether to save IRF arrays as CSV files.
#' @param verbose Logical. Whether to print progress logs.
#'
#' @return The updated `model` object with attached IRFs.
#' @export

run_tvar_irf_analysis <- function(
    model,
    horizon = 12,
    n_draws = 1000,
    ci_level = 0.90,
    shock_type = "unit",
    shock_size = 1,
    output_dir = "output/plots/irfs/",
    save_csv = TRUE,
    verbose = TRUE
) {
  impulse_vars <- colnames(model$regimes$low$Y)
  response_vars <- impulse_vars
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  for (impulse in impulse_vars) {
    if (verbose) cli::cli_h1("→ Impulse variable: {impulse}")
    
    # Simulate IRFs for this impulse
    model <- compute_tvar_irf(
      model            = model,
      horizon          = horizon,
      n_draws          = n_draws,
      shock_type       = shock_type,
      shock_size       = shock_size,
      impulse_variable = impulse,
      verbose          = verbose
    )
    
    for (response in response_vars) {
      if (verbose) cli::cli_alert_info("Saving IRF for {impulse} → {response}")
      
      # Plot path
      save_path_plot <- glue::glue("{output_dir}/tvar_irf_{impulse}_to_{response}.png")
      
      # Plot IRF for both regimes
      plot_tvar_irf_regimes(
        model     = model,
        impulse   = impulse,
        response  = response,
        horizon   = horizon,
        ci_level  = ci_level,
        save_path = save_path_plot,
        verbose   = FALSE
      )
      
      # Optional: Save as CSV
      if (save_csv) {
        for (regime in c("low", "high")) {
          irf_array <- model$irf[[regime]][[impulse]]
          irf_matrix <- irf_array[, response, ]
          irf_df <- as.data.frame(irf_matrix)
          colnames(irf_df) <- c("lower", "median", "upper")
          irf_df$t <- 0:(nrow(irf_df) - 1)
          save_path_csv <- glue::glue("{output_dir}/tvar_irf_{regime}_{impulse}_to_{response}.csv")
          readr::write_csv(irf_df, save_path_csv)
        }
      }
    }
  }
  
  if (verbose) cli::cli_alert_success("All impulse–response IRFs saved to {output_dir}")
  return(model)
}