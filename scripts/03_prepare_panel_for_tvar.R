# scripts/03_prepare_panel_for_tvar.R
# Constructs a cleaned, aligned panel for TVAR estimation

source(here("scripts/setup.R"))

source(here("functions/tvar/validate/validate_tvar_input.R"))
source(here("functions/tvar/clean/standardize_panel_variables.R"))
source(here("functions/tvar/clean/filter_common_sample.R"))
source(here("functions/tvar/save/save_tvar_input_dataset.R"))
source(here("functions/tvar/logs/log_tvar_preprocessing.R"))
source(here("functions/tvar/logs/log_tvar_metadata_json.R"))
source(here("functions/tvar/logs/log_standardization_activity.R"))
source(here("functions/tvar/logs/log_standardization_metadata_json.R"))

# Step 1: Load merged input
input_path <- here("data", "merged", "commodity_panel_with_vix.csv")  # ← switch VIX to batch later
df_raw <- read_csv(input_path)

# Step 2: Validate raw panel
validate_tvar_input(df_raw, expected_frequency = "monthly")

# Step 3: Standardize variables
standardized <- standardize_panel_variables(df_raw)
df_clean     <- standardized$data
std_meta     <- standardized$meta

# Step 4: Filter common sample (all vars non-NA)
filtered <- filter_common_sample(df_clean, verbose = TRUE)
df_tvar  <- filtered$data

# Step 5: Save final panel
output_path <- here("data", "tvar", "tvar_input_vix.csv")
dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
save_out <- save_tvar_input_dataset(df_tvar, path = output_path, hash = TRUE)

# Step 6: Log processing pipeline
log_tvar_preprocessing(
  n_obs      = nrow(df_tvar),
  n_vars     = ncol(df_tvar) - 1,
  date_range = range(df_tvar$date),
  tag        = "vix",
  sha256     = save_out$sha256,
  path       = output_path
)

log_tvar_metadata_json(
  df   = df_tvar,
  path = output_path,
  tag  = "vix"
)

log_standardization_activity(
  original_names = names(df_raw),
  cleaned_names  = std_meta$colnames,
  non_numeric_vars = names(df_clean)[sapply(df_clean[-1], function(x) all(is.na(x)))],
  tag  = "vix"
)

log_standardization_metadata_json(
  original_names = names(df_raw),
  cleaned_names  = std_meta$colnames,
  non_numeric_vars = names(df_clean)[sapply(df_clean[-1], function(x) all(is.na(x)))],
  output_path = output_path,
  tag = "vix"
)

message("\n TVAR input panel for 'vix' successfully created and logged.\n")