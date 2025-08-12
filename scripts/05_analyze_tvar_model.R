source("scripts/setup.R")

source(here("functions/tvar/logs/log_tvar_model_summary.R"))
source(here("functions/tvar/diagnostics/plot_tvar_threshold_split.R"))
source(here("functions/tvar/diagnostics/plot_tvar_regime_paths.R"))
source(here("functions/tvar/diagnostics/plot_tvar_fitted_vs_actual.R"))
source(here("functions/tvar/diagnostics/plot_tvar_irf_regimes.R"))
source(here("functions/tvar/validate/validate_tvar_coefficient_matrix.R"))
source(here("functions/tvar/simulate/compute_tvar_irf.R"))
source(here("functions/tvar/run/run_tvar_irf_analysis.R"))
source(here("functions/tvar/logs/log_tvar_irf_metadata.R"))

# Load Model
tvar_model <- readRDS("models/tvar/vix_tvar_model_20250807_114802.rds")

log_tvar_model_summary(
  model = tvar_model,
  log_path = "output/logs/tvar_model_summary.txt",
  log_level = "info",
  style = "markdown",
  verbose = TRUE
)

threshold_split_result <- plot_tvar_threshold_split(
  df = bind_rows(tvar_model$regimes$low$X, tvar_model$regimes$high$X),
  regime_var = "value_L1",
  threshold = tvar_model$threshold_value[[1]],
  threshold_percentile = "50%",
  bins = 30,
  log_stats = TRUE,
  show_density = TRUE,
  show_rug = TRUE
)

var_name <- colnames(tvar_model$regimes$low$Y)[1]

if (!is.null(tvar_model$metadata$dates)) {
  time_index <- tvar_model$metadata$dates
} else {
  total_n <- nrow(tvar_model$regimes$low$Y) + nrow(tvar_model$regimes$high$Y)
  time_index <- seq_len(total_n)
}

result_regime_plot <- plot_tvar_regime_paths(
  model = tvar_model,
  variable = var_name,
  time_index = time_index,
  show_fitted = TRUE,
  show_residuals = FALSE,
  save_path = glue::glue("output/plots/tvar_regime_paths_{var_name}.png"),
  title = glue::glue("TVAR Regime Path for '{var_name}'"),
  subtitle = "With Fitted Overlay and Regime Shading",
  verbose = TRUE
)

validate_tvar_coefficient_matrix(tvar_model$regimes$low$A, expected_k = 2, expected_p = 1)

tvar_model <- compute_tvar_irf(
  model = tvar_model,
  horizon = 12,
  n_draws = 1000,
  shock_type = "unit",
  shock_size = 1,
  impulse_variable = NULL,
  verbose = TRUE
)

tvar_model <- compute_tvar_irf(
  model = tvar_model,
  horizon = 12,
  n_draws = 1000,
  shock_type = "unit",
  shock_size = 1,
  impulse_variable = "oil",
  verbose = TRUE
)

# Get time index
if (!is.null(tvar_model$metadata$dates)) {
  time_index <- tvar_model$metadata$dates
} else {
  total_n <- nrow(tvar_model$regimes$low$Y) + nrow(tvar_model$regimes$high$Y)
  time_index <- seq_len(total_n)
}

# Plot 1: Threshold Histogram
threshold_split_result <- plot_tvar_threshold_split(
  df = bind_rows(tvar_model$regimes$low$X, tvar_model$regimes$high$X),
  regime_var = "value_L1",
  threshold = tvar_model$threshold_value[[1]],
  threshold_percentile = "50%",
  bins = 30,
  log_stats = TRUE,
  show_density = TRUE,
  show_rug = TRUE
)

# Plot 2: Regime Paths
result_regime_plot <- plot_tvar_regime_paths(
  model = tvar_model,
  variable = var_name,
  time_index = time_index,
  show_fitted = TRUE,
  show_residuals = FALSE,
  save_path = glue::glue("output/plots/tvar_regime_paths_{var_name}.png"),
  title = glue::glue("TVAR Regime Path for '{var_name}'"),
  subtitle = "With Fitted Overlay and Regime Shading",
  verbose = TRUE
)

# Plot 3: Fitted vs Actual
result_fitted_vs_actual <- plot_tvar_fitted_vs_actual(
  model = tvar_model,
  variable = var_name,
  show_fit_lines = TRUE,
  show_residuals = TRUE,
  save_path = glue::glue("output/plots/tvar_fitted_vs_actual_{var_name}.png"),
  verbose = TRUE
)

irf_var <- colnames(tvar_model$regimes$low$Y)[1]

save_path_irf <- glue::glue("output/plots/tvar_irf_regimes_{irf_var}.png")

# run_tvar_irf_analysis.R
tvar_model <- run_tvar_irf_analysis(
  model = tvar_model,
  horizon = 12,
  n_draws = 1000,
  ci_level = 0.90,
  shock_type = "unit",
  shock_size = 1,
  output_dir = "output/plots/irfs/",
  save_csv = TRUE,
  verbose = TRUE
)


# Plot IRFs by regime
plot_tvar_irf_regimes(
  model = tvar_model,
  impulse = "oil",
  response = "oil",
  horizon = 12,
  ci_level = 0.90,
  save_path = save_path_irf,
  verbose = TRUE
)

# TVAR IRF Metadata Log
log_tvar_irf_metadata(
  model = tvar_model,
  output_dir = "output/logs/",
  log_file_json = "tvar_irf_metadata.json",
  log_file_md = "tvar_irf_metadata.md",
  verbose = TRUE
)