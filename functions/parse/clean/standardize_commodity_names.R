#' Standardize Commodity Column Names (with Fuzzy Matching)
#'
#' Cleans and harmonizes column names using a reference dictionary with
#' fuzzy string matching. Ensures no duplicate names after mapping.
#'
#' @param df A data.frame or tibble containing raw commodity price data.
#' @param date_column Name of the date column (default: "date").
#' @param verbose Logical. If TRUE, prints match diagnostics (default: TRUE).
#' @param threshold Max string distance for fuzzy matching (default: 2).
#' @param return_mapping Logical. If TRUE, returns a list with `data` and `mapping`.
#'
#' @return Tibble with standardized, unique column names (or list with mapping table).
#' @export
#'
#' @examples
#' df_std <- standardize_commodity_names(df)
#' out <- standardize_commodity_names(df, return_mapping = TRUE)

standardize_commodity_names <- function(df,
                                        date_column = "date",
                                        verbose = TRUE,
                                        threshold = 2,
                                        return_mapping = FALSE) {
  stopifnot(is.data.frame(df), date_column %in% names(df))
  requireNamespace("stringdist", quietly = TRUE)
  requireNamespace("janitor", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)
  
  # Canonical mapping
  rename_map <- c(
    "crude_oil_wti" = "WTI",
    "natural_gas"   = "NaturalGas",
    "gold"          = "Gold",
    "silver"        = "Silver",
    "platinum"      = "Platinum",
    "cocoa"         = "Cocoa",
    "coffee"        = "Coffee",
    "corn"          = "Corn",
    "cotton"        = "Cotton",
    "lumber"        = "Lumber",
    "soybeans"      = "Soybeans",
    "sugar"         = "Sugar",
    "wheat"         = "Wheat",
    "aluminum"      = "Aluminium",
    "copper"        = "Copper",
    "lead"          = "Lead",
    "nickel"        = "Nickel",
    "tin"           = "Tin",
    "zinc"          = "Zinc"
  )
  
  old_names <- names(df)
  clean_names <- janitor::make_clean_names(old_names)
  new_names <- clean_names
  
  # Mapping diagnostics
  mapping <- data.frame(
    Original = old_names,
    Cleaned = clean_names,
    Mapped = NA_character_,
    Distance = NA_integer_,
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(new_names)) {
    name <- new_names[i]
    if (tolower(name) == tolower(date_column)) {
      mapping$Mapped[i] <- date_column
      mapping$Distance[i] <- 0
      next
    }
    
    dists <- stringdist::stringdist(name, names(rename_map))
    best <- which.min(dists)
    
    if (dists[best] <= threshold) {
      mapped <- rename_map[best]
      mapping$Mapped[i] <- mapped
      mapping$Distance[i] <- dists[best]
      new_names[i] <- mapped
    }
  }
  
  # Ensure uniqueness of mapped names
  final_names <- new_names
  dupes <- duplicated(final_names)
  
  if (any(dupes)) {
    counter <- list()
    for (i in which(dupes)) {
      base <- final_names[i]
      counter[[base]] <- counter[[base]] %||% 1
      counter[[base]] <- counter[[base]] + 1
      final_names[i] <- paste0(base, "_", counter[[base]])
    }
    mapping$Mapped <- final_names
  }
  
  # Set names and convert to tibble
  names(df) <- final_names
  df <- tibble::as_tibble(df)
  
  # Verbose summary
  if (verbose) {
    rlang::inform("standardize_commodity_names(): Mapping Summary")
    print(mapping, row.names = FALSE)
    
    unmatched <- mapping[is.na(mapping$Distance), ]
    if (nrow(unmatched) > 0) {
      rlang::warn(paste0("Unmatched columns detected: ", paste(unmatched$Original, collapse = ", ")))
    }
    
    duplicates <- mapping$Mapped[duplicated(mapping$Mapped)]
    if (length(duplicates) > 0) {
      rlang::inform(paste("Disambiguated duplicates:", paste(unique(duplicates), collapse = ", ")))
    }
  }
  
  if (return_mapping) {
    return(list(data = df, mapping = mapping))
  } else {
    return(df)
  }
}
