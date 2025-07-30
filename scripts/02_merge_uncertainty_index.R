# scripts/02_merge_uncertainty_index.R

# 0. Load setup + modules

source("scripts/setup.R")

# Load merge pipeline components
source(here("functions/merge/utils/standardize_index_name.R"))       
source(here("functions/merge/load/load_uncertainty_index_csv.R"))
source(here("functions/merge/clean/clean_uncertainty_index.R"))
source(here("functions/merge/clean/clean_ciss.R"))
source(here("functions/merge/clean/clean_jln.R"))
source(here("functions/merge/clean/clean_vix.R"))
source(here("functions/merge/clean/clean_vxo.R"))
source(here("functions/merge/validate/validate_uncertainty_index.R"))
source(here("functions/merge/diagnostics/plot_merge_timeline.R"))
source(here("functions/merge/diagnostics/check_uncertainty_metadata.R"))
source(here("functions/merge/save/save_uncertainty_index_dataset.R"))
source(here("functions/merge/save/save_merged_dataset.R"))
source(here("functions/merge/logs/log_merge_activity.R"))
source(here("functions/merge/logs/log_merge_metadata_json.R"))

# 1. User-defined inputs
index_raw_name <- "vix index"
source_file    <- here("data/uncertainty/vix.csv")
main_panel_path <- here("data/clean/commodity_panel.csv")  # Modular input
index_raw_name  <- "vix index"
source_file     <- here("data/uncertainty/vix.csv")

# Validate file existence early
stopifnot(file.exists(main_panel_path), file.exists(source_file))

# Load main panel
main_panel <- read_csv(main_panel_path, show_col_types = FALSE)

# 2. Standardize + Load
index_canonical <- standardize_index_name(index_raw_name)
index_df_raw    <- load_uncertainty_index_csv(source_file)

# 3. Clean + Validate
index_df_clean <- clean_uncertainty_index(index_df_raw, index_canonical)
validate_uncertainty_index(index_df_clean)

# 4. Merge
merged_df <- left_join(main_panel, index_df_clean, by = "date")

# Count matches based on non-missing values in the merged column
matched_rows <- sum(!is.na(merged_df[[index_canonical]]))

# 5. Diagnostics
plot_merge_timeline(index_list = rlang::set_names(list(index_df_clean), index_canonical))


# 6. Save Output
output_file <- here(glue("data/merged/commodity_panel_with_{tolower(index_canonical)}.csv"))
save_merged_dataset(merged_df, output_file)

# 7. Logging
# CSV activity log
log_merge_activity(
  index_name        = index_canonical,
  n_rows_main       = nrow(main_panel),
  n_rows_index      = nrow(index_df_clean),
  n_rows_matched    = matched_rows,
  source_file_index = source_file,
  output_path       = here("logs/merge_activity.csv"),
  verbose           = TRUE
)

# JSON metadata log
log_merge_metadata_json(
  index_name        = index_canonical,
  n_rows_main       = nrow(main_panel),
  n_rows_index      = nrow(index_df_clean),
  n_rows_matched    = matched_rows,
  source_file_index = source_file,
  output_path       = here("logs/merge_metadata.json"),
  verbose           = TRUE,
  extra             = list(merge_run_id = paste0("merge_", format(Sys.time(), "%Y%m%d_%H%M%S")))
)

# 8. Completion Message
message(glue::glue("Merge complete for index: {index_canonical}"))
