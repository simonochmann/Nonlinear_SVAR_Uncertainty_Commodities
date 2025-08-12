# scripts/02_merge_uncertainty_index.R
# Merge real uncertainty indices (VIX, VXO, JLN) into the canonical commodity panel

source("scripts/setup.R")

source(here("functions/merge/utils/standardize_index_name.R"))       
source(here("functions/merge/load/load_uncertainty_index_csv.R"))
source(here("functions/merge/clean/clean_uncertainty_index.R"))
source(here("functions/merge/clean/clean_vix.R"))
source(here("functions/merge/clean/clean_vxo.R"))
source(here("functions/merge/clean/clean_jln.R"))
source(here("functions/merge/validate/validate_uncertainty_index.R"))
source(here("functions/merge/diagnostics/plot_merge_timeline.R"))
source(here("functions/merge/diagnostics/check_uncertainty_metadata.R"))
source(here("functions/merge/save/save_uncertainty_index_dataset.R"))
source(here("functions/merge/save/save_merged_dataset.R"))
source(here("functions/merge/logs/log_merge_activity.R"))
source(here("functions/merge/logs/log_merge_metadata_json.R"))

# Global choices so runs are reproducible/auditable
MONTH_ANCHOR <- "start"   # "start" or "end" (date to stamp monthly values)
AGG_METHOD   <- "mean"    # "mean", "median", "last", "first"

# Normalize a cleaned index to with deterministic monthly roll-up
normalize_index <- function(df, index_canonical) {
  # Date column
  date_candidates <- intersect(names(df), c("date","Date","DATE","period","month"))
  if (length(date_candidates) == 0) stop("No date-like column found in cleaned index.")
  if (!"date" %in% names(df)) {
    df <- dplyr::rename(df, date = !!rlang::sym(date_candidates[1]))
  }
  if (!inherits(df$date, "Date")) {
    suppressWarnings({
      try1 <- as.Date(df$date)
      if (all(is.na(try1))) try1 <- as.Date(paste0(df$date, "-01")) # "YYYY-MM" etc.
      df$date <- try1
    })
  }
  stopifnot(inherits(df$date, "Date"))
  
  # Value column → index_canonical
  if (!index_canonical %in% names(df)) {
    cand <- names(df)[vapply(df, is.numeric, logical(1))]
    cand <- setdiff(cand, "date")
    if (length(cand) == 0) cand <- setdiff(names(df), "date")
    if (length(cand) == 0) stop("No candidate numeric value column in cleaned index.")
    dens <- vapply(df[cand], function(x) sum(!is.na(x)), integer(1))
    best <- cand[which.max(dens)]
    df <- dplyr::rename(df, !!index_canonical := !!rlang::sym(best))
  }
  
  # Daily → Monthly (deterministic)
  # Anchor date either to month start or month end
  anchor_date <- function(x) {
    if (tolower(MONTH_ANCHOR) == "end") {
      # last calendar day of the month
      lubridate::ceiling_date(x, unit = "month") - lubridate::days(1)
    } else {
      # first calendar day of the month (default)
      lubridate::floor_date(x, unit = "month")
    }
  }
  
  # Aggregator
  agg_fun <- switch(tolower(AGG_METHOD),
                    mean   = function(v) mean(v, na.rm = TRUE),
                    median = function(v) stats::median(v, na.rm = TRUE),
                    last   = function(v) { vv <- stats::na.omit(v); if (length(vv)) vv[length(vv)] else NA_real_ },
                    first  = function(v) { vv <- stats::na.omit(v); if (length(vv)) vv[1] else NA_real_ },
                    stop("Unknown AGG_METHOD: ", AGG_METHOD)
  )
  
  # If more than one obs per calendar month, aggregate
  multi_per_month <- nrow(dplyr::distinct(df, ym = format(df$date, "%Y-%m"))) < nrow(df)
  if (multi_per_month) {
    df <- df %>%
      dplyr::mutate(month = anchor_date(.data$date)) %>%
      dplyr::group_by(.data$month) %>%
      dplyr::summarise(
        date = dplyr::first(.data$month),
        !!index_canonical := agg_fun(.data[[index_canonical]]),
        .groups = "drop"
      )
  } else {
    # Ensure dates are anchored consistently even if input is already monthly
    df <- df %>% dplyr::mutate(date = anchor_date(.data$date))
  }
  
  # Keep tidy shape
  df <- df %>%
    dplyr::select(.data$date, !!rlang::sym(index_canonical)) %>%
    dplyr::arrange(.data$date)
  
  # Hard guards
  stopifnot(!any(is.na(df$date)))
  stopifnot(!anyDuplicated(df$date))
  stopifnot(is.numeric(df[[index_canonical]]))
  
  df
}

# Quick post-merge validator (no merge explosions; coverage report)
validate_merged <- function(merged_df, index_canonical) {
  need <- c("date", "commodity_id", index_canonical)
  stopifnot(all(need %in% names(merged_df)))
  
  # No duplicate (date, commodity_id) rows
  if (anyDuplicated(merged_df[c("date", "commodity_id")])) {
    stop("Duplicate (date, commodity_id) pairs after merge — check index for duplicate dates.")
  }
  
  # Overall coverage
  cov_overall <- mean(!is.na(merged_df[[index_canonical]]))
  
  # Per-commodity coverage (optional warning for very sparse series)
  by_commodity <- merged_df |>
    dplyr::group_by(commodity_id) |>
    dplyr::summarise(coverage = mean(!is.na(.data[[index_canonical]])), .groups = "drop")
  
  low <- by_commodity$commodity_id[by_commodity$coverage < 0.40]
  if (length(low) > 0) {
    warning(
      index_canonical, ": coverage below 40% for ",
      length(low), " commodities (e.g. ", paste(utils::head(low, 5), collapse = ", "),
      if (length(low) > 5) ", ..." else "", ")."
    )
  }
  
  invisible(list(
    range = range(merged_df$date, na.rm = TRUE),
    coverage_overall = cov_overall,
    coverage_by_commodity = by_commodity
  ))
}

hash_csv <- function(path) digest::digest(file = path, algo = "sha256")

# Run the full flow for a single index
run_merge_index <- function(index_raw_name,
                            source_file,
                            main_panel_path = here("data/clean/commodity_panel.csv"),
                            trim_to_overlap = FALSE,
                            make_timeline_plot = TRUE) {
  
  stopifnot(file.exists(main_panel_path), file.exists(source_file))
  
  # main panel
  main_panel <- readr::read_csv(main_panel_path, show_col_types = FALSE)
  required_main <- c("date","commodity_id","commodity_name","unit","price","ln_price","ret","vol_proxy")
  stopifnot(all(required_main %in% names(main_panel)))
  
  # load + standardize
  index_canonical <- standardize_index_name(index_raw_name)  # "VIX","VXO","JLN"
  index_df_raw    <- load_uncertainty_index_csv(source_file)
  index_df_clean  <- clean_uncertainty_index(index_df_raw, index_canonical)
  index_df_clean  <- normalize_index(index_df_clean, index_canonical)
  
  # validate expects (date, value)
  val_df <- dplyr::rename(index_df_clean, value = !!rlang::sym(index_canonical))
  validate_uncertainty_index(val_df)
  
  # optional trim of main panel to index overlap
  ix_start <- min(index_df_clean$date, na.rm = TRUE)
  ix_end   <- max(index_df_clean$date, na.rm = TRUE)
  if (trim_to_overlap) {
    main_panel <- main_panel %>% dplyr::filter(date >= ix_start, date <= ix_end)
    cat("Trimmed main panel to overlap:", format(ix_start), "→", format(ix_end),
        "(", nrow(main_panel), "rows)\n")
  }
  
  # merge
  merged_df <- dplyr::left_join(main_panel, index_df_clean, by = "date")
  
  # coverage table
  coverage <- merged_df %>%
    dplyr::summarise(
      n_rows    = dplyr::n(),
      n_matched = sum(!is.na(.data[[index_canonical]])),
      pct_matched = round(100 * n_matched / n_rows, 2)
    )
  print(coverage)
  matched_rows <- coverage$n_matched
  
  # post-merge quick checks
  validate_merged(merged_df, index_canonical)
  
  # per-index preview plot (no globals)
  if (isTRUE(make_timeline_plot)) {
    try(
      plot_merge_timeline(
        index_list = rlang::set_names(list(index_df_clean), index_canonical),
        scale = "none",
        overlap_only = FALSE,
        save_path = here::here("figures", paste0("timeline_", tolower(index_canonical), ".png"))
      ),
      silent = TRUE
    )
  }
  
  # save per-index merged panel + logs
  dir.create(here("data/merged"), recursive = TRUE, showWarnings = FALSE)
  out_file <- here(glue::glue("data/merged/commodity_panel_with_{tolower(index_canonical)}.csv"))
  save_merged_dataset(merged_df, out_file)
  
  log_merge_activity(
    index_name        = index_canonical,
    n_rows_main       = nrow(main_panel),
    n_rows_index      = nrow(index_df_clean),
    n_rows_matched    = matched_rows,
    source_file_index = source_file,
    output_path       = here("logs/merge_activity.csv"),
    verbose           = TRUE
  )
  
  # provenance-rich JSON log
  log_merge_metadata_json(
    index_name        = index_canonical,
    n_rows_main       = nrow(main_panel),
    n_rows_index      = nrow(index_df_clean),
    n_rows_matched    = matched_rows,
    source_file_index = source_file,
    output_path       = here("logs/merge_metadata.json"),
    verbose           = TRUE,
    extra             = list(
      merge_run_id = paste0("merge_", format(Sys.time(), "%Y%m%d_%H%M%S")),
      input_hashes = list(
        main_panel = hash_csv(main_panel_path),
        index_csv  = hash_csv(source_file)
      ),
      aggregation = list(anchor = MONTH_ANCHOR, method = AGG_METHOD)
    )
  )
  
  message(glue::glue("Merge complete for index: {index_canonical}"))
  
  list(index = index_canonical, cleaned = index_df_clean, merged = merged_df, coverage = coverage)
}

# Driver: VIX, VXO, JLN 
main_panel_path <- here("data/clean/commodity_panel.csv")

inputs <- list(
  list(name = "vix index", file = here("data/uncertainty/vix.csv")),
  list(name = "vxo index", file = here("data/uncertainty/vxo.csv")),
  list(name = "jln index", file = here("data/uncertainty/jln.csv"))
)

results <- lapply(
  inputs,
  function(x) run_merge_index(x$name, x$file, main_panel_path,
                              trim_to_overlap = FALSE, make_timeline_plot = TRUE)
)

# Combined coverage report
cov_tbl <- dplyr::bind_rows(lapply(results, `[[`, "coverage"))
cov_tbl$index <- vapply(results, `[[`, character(1), "index")
cov_tbl <- cov_tbl %>% dplyr::select(index, dplyr::everything())
print(cov_tbl)

# Combined timeline (z-scale on common overlap) 
dir.create(here("figures"), recursive = TRUE, showWarnings = FALSE)
index_list_all <- rlang::set_names(
  lapply(results, `[[`, "cleaned"),
  vapply(results, `[[`, character(1), "index")
)
try(
  plot_merge_timeline(
    index_list   = index_list_all,
    scale        = "z",
    overlap_only = TRUE,
    save_path    = here::here("figures","uncertainty_timeline_scaled.png")
  ),
  silent = TRUE
)

# Build a single panel with all indices side-by-side
combined <- readr::read_csv(main_panel_path, show_col_types = FALSE)
for (res in results) {
  nm <- res$index
  df <- res$cleaned                       # (date, <INDEX>)
  combined <- dplyr::left_join(combined, df, by = "date")
}
dir.create(here("data/merged"), recursive = TRUE, showWarnings = FALSE)
out_all <- here("data/merged/commodity_panel_with_vix_vxo_jln.csv")
readr::write_csv(combined, out_all)
cat("Combined panel with VIX, VXO, JLN written to:", out_all, "\n")
