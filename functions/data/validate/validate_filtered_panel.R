# functions/data/validate/validate_filtered_panel.R
validate_filtered_panel <- function(df, min_obs = 60, max_na_share = 0.05, warmup_vol = 12) {
  stopifnot(is.data.frame(df), "date" %in% names(df))
  # Schema
  required <- c("date","commodity_id","commodity_name","unit","price","ln_price","ret","vol_proxy")
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols)) stop("Missing columns: ", paste(missing_cols, collapse=", "))
  if (!inherits(df$date, "Date")) stop("date must be Date")
  # Duplicates
  dup_n <- dplyr::count(df, commodity_id, date, name="n") |> dplyr::filter(n>1) |> nrow()
  if (dup_n>0) stop("Found duplicate (commodity_id,date) rows: ", dup_n)
  # NAs logic
  chk_na <- df |>
    dplyr::group_by(commodity_id) |>
    dplyr::summarise(
      n=n(),
      na_price = sum(is.na(price)),
      na_ln    = sum(is.na(ln_price)),
      na_ret   = sum(is.na(ret)[-1], na.rm=TRUE),
      na_vol   = sum(is.na(vol_proxy)[-(1:warmup_vol)], na.rm=TRUE),
      .groups="drop"
    )
  if (any(chk_na$na_price>0)) stop("NAs in price detected.")
  if (any(chk_na$na_ln>0)) stop("Unexpected NAs in ln_price.")
  if (any(chk_na$na_ret>0)) stop("Unexpected NAs in ret (beyond first diff).")
  if (any(chk_na$na_vol>0)) stop("Unexpected NAs in vol_proxy (beyond warm-up).")
  # Minimum length & NA share
  covr <- df |>
    dplyr::group_by(commodity_id) |>
    dplyr::summarise(
      n=n(),
      na_share = mean(is.na(ret)),
      .groups="drop"
    )
  if (any(covr$n < min_obs)) stop("Series with < min_obs present.")
  if (any(covr$na_share > max_na_share, na.rm=TRUE)) stop("Series with excessive NA share in ret.")
  # Frequency & alignment
  by_id <- df |>
    dplyr::group_by(commodity_id) |>
    dplyr::arrange(date, .by_group = TRUE) |>
    dplyr::summarise(median_step = median(diff(date)), .groups="drop")
  if (!all(by_id$median_step %in% c(28,29,30,31))) stop("Non-monthly frequency detected.")
  # Positivity of price
  if (any(df$price <= 0, na.rm=TRUE)) stop("Non-positive prices found.")
  invisible(TRUE)
}
