# scripts/04_estimate_tvar.R

source("scripts/setup.R")

source(here("functions/tvar/estimate/select_tvar_threshold.R"))
source(here("functions/tvar/estimate/estimate_tvar_model.R"))
source(here("functions/tvar/validate/validate_estimated_tvar.R"))
source(here("functions/tvar/save/save_tvar_model_object.R"))

# Load input data 
df_tvar <- readr::read_csv(here("data", "tvar", "tvar_input_vix.csv"))

# Threshold selection 
threshold_info <- select_tvar_threshold(
  df = df_tvar,
  threshold_var = "value",
  lag = 1,
  scale = TRUE,
  threshold_method = "quantile",
  method_param = 0.5,
  verbose = FALSE
)

# Estimate TVAR model 
tvar_model <- estimate_tvar_model(
  df             = df_tvar,
  threshold_info = threshold_info,
  lag            = 1,
  verbose        = FALSE
)

# Validate + Save
if (length(tvar_model$regimes) == 2) {
  tvar_model$metadata$input_df <- df_tvar
  validate_estimated_tvar(tvar_model, verbose = FALSE)
  saved_paths <- save_tvar_model_object(
    model_object = tvar_model,
    prefix       = "vix_",
    verbose      = FALSE
  )
} else {
  warning("TVAR model has less than two regimes. Skipping validation and saving.")
}
