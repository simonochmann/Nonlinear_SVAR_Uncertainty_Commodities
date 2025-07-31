source(here("scripts/setup.R"))
source(here("functions/tvar/estimate/select_tvar_threshold.R"))
source(here("functions/tvar/estimate/estimate_tvar_model.R"))
source(here("functions/tvar/validate/validate_estimated_tvar.R"))
source(here("functions/tvar/save/save_tvar_model_object.R"))

df_tvar <- readr::read_csv(here("data", "tvar", "tvar_input_vix.csv"))

threshold_info <- select_tvar_threshold(
  df = df_tvar,
  threshold_var = "value",
  lag = 1,
  scale = TRUE,
  threshold_method = "quantile",
  method_param = 0.5,
  verbose = TRUE
)

tvar_model <- estimate_tvar_model(
  df             = df_tvar,
  threshold_info = threshold_info,
  lag           = 1,
  verbose        = TRUE
)

validate_estimated_tvar(tvar_model, verbose = TRUE)

if (length(tvar_model$regimes) == 2) {
  
  # Inject input df into model metadata for snapshot traceability
  tvar_model$metadata$input_df <- df_tvar
  
  # Validate structure and dimensions
  validate_estimated_tvar(tvar_model, verbose = TRUE)
  
  # Save model and metadata
  source(here("functions/tvar/save/save_tvar_model_object.R"))
  saved_paths <- save_tvar_model_object(
    model_object = tvar_model,
    prefix       = "vix_",
    verbose      = TRUE
  )
  
  # Step 3: Confirmation
  message("\n All outputs saved:\n",
          "Model RDS: ", saved_paths$model_rds, "\n",
          "Metadata:  ", saved_paths$metadata_json, "\n",
          if (!is.null(saved_paths$input_snapshot))
            paste0("Input CSV: ", saved_paths$input_snapshot, "\n")
  )
  
} else {
  warning("TVAR model has less than two regimes. Skipping validation and saving.")
}


