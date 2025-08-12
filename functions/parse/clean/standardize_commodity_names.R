#' Standardize Commodity Column Names (explicit map + safe fuzzy)
#'
#' 1) Apply an explicit mapping for known Pink Sheet headers (prevents bad fuzzy).
#' 2) Normalize to clean snake_case.
#' 3) Optionally fuzzy-match the remaining unmapped columns to a canonical dictionary.
#' 4) Coalesce any duplicate columns created by mapping.
#'
#' @param df A data.frame or tibble containing raw commodity price data.
#' @param date_column Name of the date column (default: "date").
#' @param verbose Logical; print diagnostics (default: TRUE).
#' @param threshold Max string distance for fuzzy matching (default: 2).
#' @param return_mapping If TRUE, returns list(data, mapping).
#' @return Tibble with standardized, unique column names (or list with mapping table).
#' @export
standardize_commodity_names <- function(df,
                                        date_column = "date",
                                        verbose = TRUE,
                                        threshold = 2,
                                        return_mapping = FALSE) {
  stopifnot(is.data.frame(df), date_column %in% names(df))
  requireNamespace("stringdist", quietly = TRUE)
  requireNamespace("janitor", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)
  `%||%` <- function(a,b) if (is.null(a)) b else a
  
  old_names <- names(df)
  
  # 1) Explicit map to prevent bad fuzzy hits 
  # Map raw Pink Sheet headers to stable snake_case names.
  explicit_map <- c(
    # Energy
    "CRUDE_WTI" = "crude_wti",
    "NGAS_US"   = "ngas_us",
    "NGAS_EUR"  = "ngas_eur",
    "NGAS_JP"   = "ngas_jp",
    "iNATGAS"   = "i_natgas",
    
    # Precious metals
    "GOLD"      = "gold",
    "PLATINUM"  = "platinum",
    "SILVER"    = "silver",
    
    # Agriculture (subset)
    "COCOA"          = "cocoa",
    "COFFEE_ARABIC"  = "coffee_arabic",
    "MAIZE"          = "maize",            # use "corn" if you prefer
    "COTTON_A_INDX"  = "cotton_a_indx",
    "SOYBEANS"       = "soybeans",
    "SUGAR_WLD"      = "sugar_wld",
    "WHEAT_US_HRW"   = "wheat_us_hrw",
    
    # Industrial metals
    "ALUMINUM" = "aluminium",
    "COPPER"   = "copper",
    "LEAD"     = "lead",
    "NICKEL"   = "nickel",
    "Tin"      = "tin",
    "Zinc"     = "zinc",
    
    # Fertilizers / others (explicit => no fuzzy)
    "TSP"          = "tsp",
    "DAP"          = "dap",
    "PHOSROCK"     = "phosrock",
    "UREA_EE_BULK" = "urea_ee_bulk",
    
    # Oils (quiet warnings)
    "SOYBEAN_OIL"  = "soybean_oil",
    "SOYBEAN_MEAL" = "soybean_meal",
    "RAPESEED_OIL" = "rapeseed_oil",
    "SUNFLOWER_OIL"= "sunflower_oil"
  )
  
  # Apply explicit map directly on original names
  primed_names <- dplyr::recode(old_names, !!!explicit_map, .default = old_names)
  
  # 2) Normalize to snake_case (stable) 
  clean_names <- janitor::make_clean_names(primed_names)
  # keep date column name exactly as requested
  clean_names <- ifelse(tolower(clean_names) == janitor::make_clean_names(date_column),
                        date_column, clean_names)
  
  # 3) Optional fuzzy mapping for the rest 
  # Canonical dictionary (keys are snake_case cleaned targets)
  canonical_dict <- c(
    "crude_oil_wti" = "crude_wti",
    "natural_gas"   = "ngas_us",      # map generic to US by default
    "gold"          = "gold",
    "silver"        = "silver",
    "platinum"      = "platinum",
    "cocoa"         = "cocoa",
    "coffee"        = "coffee_arabic",# prefer arabica if ambiguous
    "corn"          = "maize",
    "cotton"        = "cotton_a_indx",
    "lumber"        = "lumber",
    "soybeans"      = "soybeans",
    "sugar"         = "sugar_wld",
    "wheat"         = "wheat_us_hrw",
    "aluminum"      = "aluminium",
    "copper"        = "copper",
    "lead"          = "lead",
    "nickel"        = "nickel",
    "tin"           = "tin",
    "zinc"          = "zinc"
  )
  
  # Blocklist: never fuzzy-map these (we already set them explicitly)
  fuzzy_block <- tolower(c(names(explicit_map), date_column))
  
  new_names <- clean_names
  mapped_from_fuzzy <- rep(NA_character_, length(new_names))
  mapped_dist <- rep(NA_integer_, length(new_names))
  
  # Fuzzy only for names not covered by explicit map & not date
  for (i in seq_along(new_names)) {
    nm <- new_names[i]
    if (tolower(nm) %in% fuzzy_block) next
    if (tolower(nm) == tolower(date_column)) next
    
    dists <- stringdist::stringdist(nm, names(canonical_dict))
    best <- which.min(dists)
    if (length(best) == 1 && is.finite(dists[best]) && dists[best] <= threshold) {
      new_names[i] <- unname(canonical_dict[best])
      mapped_from_fuzzy[i] <- names(canonical_dict)[best]
      mapped_dist[i] <- dists[best]
    }
  }
  
  # 4) Coalesce duplicates created by mapping 
  # If mapping produced duplicate column names, merge them (first non-NA by row).
  # Because df is wide time x commodities, coalescing duplicates is safe.
  names(df) <- new_names
  
  # build mapping table before de-dup coalesce
  mapping <- data.frame(
    Original = old_names,
    Cleaned  = clean_names,
    Mapped   = new_names,
    Distance = mapped_dist,
    stringsAsFactors = FALSE
  )
  
  dedup_coalesce <- function(dat) {
    dup_groups <- split(seq_along(names(dat)), names(dat))
    for (idx in dup_groups) {
      if (length(idx) > 1) {
        merged <- Reduce(dplyr::coalesce, dat[idx])
        dat <- dat[, -idx[-1], drop = FALSE]
        dat[[ names(dat)[idx[1] - (length(idx)-1)] ]] <- merged
      }
    }
    dat
  }
  df <- dedup_coalesce(df)
  
  # Recompute final names after any column drops
  final_names <- names(df)
  mapping$Mapped <- match(mapping$Mapped, final_names) %>% { final_names[.] %||% mapping$Mapped }
  
  df <- tibble::as_tibble(df)
  
  # Verbose diagnostics 
  if (verbose) {
    rlang::inform("standardize_commodity_names(): Mapping Summary")
    print(mapping, row.names = FALSE)
    
    # Anything not distance-mapped AND not explicitly recoded?
    unmatched_idx <- which(is.na(mapping$Distance) &
                             !old_names %in% names(explicit_map) &
                             tolower(mapping$Mapped) != tolower(date_column))
    if (length(unmatched_idx) > 0) {
      rlang::warn(paste0("Unmatched columns detected: ",
                         paste(mapping$Original[unmatched_idx], collapse = ", ")))
    }
    
    dups <- final_names[duplicated(final_names)]
    if (length(dups) > 0) {
      rlang::inform(paste("Coalesced duplicates:", paste(unique(dups), collapse = ", ")))
    }
  }
  
  if (return_mapping) {
    return(list(data = df, mapping = mapping))
  } else {
    return(df)
  }
}