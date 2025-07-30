# 01_filter_commodity_panel.R
# Filter clean long-format commodity panel for complete series,
# compute log returns and volatility proxies, and export results.
# Includes full logging (JSON + .log) and panel diagnostics.

source("scripts/setup.R")

source("functions/filter/filter_complete_series.R")
source("functions/filter/compute_log_returns.R")
source("functions/filter/compute_volatility_proxy.R")
source("functions/filter/save_filtered_panel.R")

source("functions/filter/logs/log_dropped_commodities.R")
source("functions/filter/logs/log_retained_commodities.R")
source("functions/filter/logs/log_filtering_activity.R")
source("functions/filter/logs/log_filtering_metadata_json.R")


# Parameters
input_file      <- "data/raw/commodity_prices_long.csv"
min_obs         <- 180
vol_method      <- "squared"  # options: "squared", "abs", "garch", etc.
out_dir         <- "output/filtered"
log_txt_path    <- "logs/filtering/filtering_pipeline.log"
tags            <- c("monthly", "macro", "filtered")
note            <- "Baseline filtering after long-format parse"

# Load input data
df <- read.csv(input_file)
df$date <- as.Date(df$date)

n_before <- length(unique(df$commodity))

# 1.Filter incomplete series
df_filtered <- filter_complete_series(df, min_obs = min_obs)

# 2. Compute log returns 
df_returns <- compute_log_returns(df_filtered)

# 3. Compute volatility proxies
df_vol <- compute_volatility_proxy(df_returns, method = vol_method)

n_after <- length(unique(df_vol$commodity))

# 4. Save filtered panel (long + wide CSVs) 
save_filtered_panel(df_vol, out_dir = out_dir, overwrite = TRUE)

# 5. Identify dropped commodities and prepare dropped_df
dropped_df <- df |>
  dplyr::group_by(commodity) |>
  dplyr::summarise(n_obs = sum(!is.na(price)), .groups = "drop") |>
  dplyr::filter(!commodity %in% unique(df_vol$commodity))

# 5.1 Log dropped commodities (CSV + JSON)
log_dropped_commodities(dropped_df,
                        out_dir = "logs/filtering",
                        tags = tags,
                        note = note
)

# 6. Write human-readable .log entry 
log_filtering_activity(
  out_log = log_txt_path,
  n_commodities_before = n_before,
  n_commodities_after = n_after,
  min_obs = min_obs,
  method_vol_proxy = vol_method,
  out_dir = out_dir,
  tags = tags,
  note = note
)

# 7. Write metadata .json entry
log_filtering_metadata_json(
  df = df_vol,
  out_dir = out_dir,
  vol_method = vol_method,
  min_obs = min_obs,
  tags = tags,
  note = note
)