# functions/filter/validate/validate_filtered_panel.R
validate_filtered_panel <- function(df, verbose = TRUE) {
  stopifnot(is.data.frame(df))
  
  required_cols <- c(
    "date","commodity_id","commodity_name","unit",
    "price","ln_price","ret","vol_proxy"
  )
  missing <- setdiff(required_cols, names(df))
  if (length(missing)) {
    stop("Filtered panel is missing required columns: ",
         paste(missing, collapse = ", "))
  }
  
  # Types
  if (!inherits(df$date, "Date")) stop("Column 'date' must be Date.")
  num_cols <- c("price","ln_price","ret","vol_proxy")
  not_num  <- num_cols[!vapply(df[num_cols], is.numeric, logical(1))]
  if (length(not_num)) {
    stop("Non-numeric columns where numeric expected: ",
         paste(not_num, collapse = ", "))
  }
  if (!is.character(df$commodity_id))   stop("'commodity_id' must be character.")
  if (!is.character(df$commodity_name)) stop("'commodity_name' must be character.")
  if (!is.character(df$unit))           stop("'unit' must be character (use NA_character_ if unknown).")
  
  # Key uniqueness: (commodity_id, date)
  if (any(duplicated(df[c("commodity_id","date")]))) {
    dup <- df |>
      dplyr::count(commodity_id, date, name = "n") |>
      dplyr::filter(n > 1)
    ndup <- nrow(dup)
    stop(ndup, " duplicated (commodity_id, date) rows detected. First few: ",
         paste(utils::head(paste0(dup$commodity_id,"@",dup$date), 5), collapse = ", "))
  }
  
  # Allow at most one NA in `ret` per commodity (first diff)
  # and at most one NA in `vol_proxy` if it’s derived from ret
  byc <- df |>
    dplyr::group_by(commodity_id) |>
    dplyr::summarise(
      n      = dplyr::n(),
      na_ret = sum(is.na(ret)),
      na_vol = sum(is.na(vol_proxy)),
      .groups = "drop"
    )
  bad_ret <- byc$commodity_id[byc$na_ret > 1L]
  if (length(bad_ret)) {
    warning("More than one NA in 'ret' for: ", paste(bad_ret, collapse = ", "))
  }
  bad_vol <- byc$commodity_id[byc$na_vol > 1L]
  if (length(bad_vol)) {
    warning("More than one NA in 'vol_proxy' for: ", paste(bad_vol, collapse = ", "))
  }
  
  if (isTRUE(verbose)) {
    rng <- range(df$date, na.rm = TRUE)
    cat("validate_filtered_panel(): OK | rows:", nrow(df),
        "| commodities:", dplyr::n_distinct(df$commodity_id),
        "| date range:", format(rng[1]), "→", format(rng[2]), "\n")
  }
  invisible(TRUE)
}