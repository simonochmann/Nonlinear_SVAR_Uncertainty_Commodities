#' prepare_uncertainty_proxies_v2
#'
#' Loads, validates, aligns, smooths, and standardizes multiple macro-financial uncertainty proxies.
#' Returns either a wide or long tidy data frame, with optional visual diagnostics.
#'
#' @param vxo_path Path to CSV with "date" and "vxo" columns
#' @param vix_path Path to CSV with "date" and "vix" columns
#' @param jln_path Path to CSV with "date" and "jln" columns
#' @param ciss_path Path to CSV with "date" and "ciss" columns
#' @param smooth Logical. Whether to apply smoothing (default: TRUE)
#' @param smoothing_method "rollmean" (default), "ema", or "none"
#' @param impute_missing Logical. Interpolate short NA gaps? (default: TRUE)
#' @param align_dates Logical. Keep only common overlapping period? (default: TRUE)
#' @param standardize Logical. Apply z-score normalization (default: TRUE)
#' @param output_format "wide" (default) or "long"
#' @param plot Logical. Show before/after plots (default: FALSE)
#' @param verbose Logical. Print diagnostics (default: TRUE)
#'
#' @return A wide or long tidy data frame of uncertainty proxies
#' @export
prepare_uncertainty_proxies <- function(vxo_path, vix_path, jln_path, ciss_path,
                                        smooth = TRUE,
                                        smoothing_method = "rollmean",
                                        impute_missing = TRUE,
                                        align_dates = TRUE,
                                        standardize = TRUE,
                                        output_format = "wide",
                                        plot = FALSE,
                                        verbose = TRUE) {
  
  # load and validate each series 
  load_series <- function(path, name) {
    if (!file.exists(path)) stop("File not found: ", path)
    df <- readr::read_csv(path, show_col_types = FALSE)
    if (!("date" %in% names(df))) stop("Missing 'date' column in ", name)
    if (!(name %in% names(df))) stop("Missing '", name, "' column in ", path)
    df <- df[, c("date", name)]
    df$date <- lubridate::ymd(df$date)
    df <- df[order(df$date), ]
    df$proxy <- name
    colnames(df) <- c("date", "value", "proxy")
    return(df)
  }
  
  # Load all proxies
  proxies <- dplyr::bind_rows(
    load_series(vxo_path, "vxo"),
    load_series(vix_path, "vix"),
    load_series(jln_path, "jln"),
    load_series(ciss_path, "ciss")
  )
  
  # Smooth each proxy
  if (smooth) {
    proxies <- proxies %>%
      dplyr::group_by(proxy) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(value = dplyr::case_when(
        smoothing_method == "rollmean" ~ zoo::rollmean(value, k = 3, fill = NA, align = "right"),
        smoothing_method == "ema" ~ TTR::EMA(value, n = 3),
        TRUE ~ value
      )) %>%
      dplyr::ungroup()
  }
  
  # Impute short NA gaps (linear interpolation)
  if (impute_missing) {
    proxies <- proxies %>%
      dplyr::group_by(proxy) %>%
      dplyr::mutate(value = zoo::na.approx(value, na.rm = FALSE, maxgap = 2)) %>%
      dplyr::ungroup()
  }
  
  # Align common date range
  if (align_dates) {
    common_dates <- proxies %>%
      tidyr::drop_na() %>%
      dplyr::group_by(proxy) %>%
      dplyr::summarise(min_date = min(date), max_date = max(date)) %>%
      dplyr::summarise(
        start = max(min_date),
        end = min(max_date)
      )
    proxies <- proxies %>%
      dplyr::filter(date >= common_dates$start, date <= common_dates$end)
  }
  
  # Standardize
  if (standardize) {
    proxies <- proxies %>%
      dplyr::group_by(proxy) %>%
      dplyr::mutate(value = scale(value)[, 1]) %>%
      dplyr::ungroup()
  }
  
  # Diagnostics
  if (verbose) {
    cat("prepare_uncertainty_proxies()\n")
    cat("Proxies loaded:", unique(proxies$proxy), "\n")
    cat("Total observations:", nrow(proxies), "\n")
    proxy_stats <- proxies %>%
      dplyr::group_by(proxy) %>%
      dplyr::summarise(
        Start = min(date),
        End = max(date),
        Obs = dplyr::n(),
        NAs = sum(is.na(value)),
        Min = min(value, na.rm = TRUE),
        Max = max(value, na.rm = TRUE),
        Mean = mean(value, na.rm = TRUE),
        SD = sd(value, na.rm = TRUE)
      )
    print(proxy_stats)
  }
  
  # Plot before/after per proxy
  if (plot) {
    ggplot2::ggplot(proxies, ggplot2::aes(x = date, y = value)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~proxy, scales = "free_y", ncol = 2) +
      ggplot2::labs(title = "Smoothed + Standardized Uncertainty Proxies",
                    x = NULL, y = "z-score (3-month smoothed)") +
      ggplot2::theme_minimal()
  }
  
  # Return in requested format
  if (output_format == "wide") {
    proxies_wide <- tidyr::pivot_wider(proxies, names_from = proxy, values_from = value)
    return(tidyr::drop_na(proxies_wide))
  } else {
    return(tidyr::drop_na(proxies))
  }
}
