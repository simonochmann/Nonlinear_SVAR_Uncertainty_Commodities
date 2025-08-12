#' Clean CISS schema (pass-through; already {date, ciss})
clean_ciss_schema <- function(df, verbose = TRUE) {
  stopifnot(all(c("date","ciss") %in% names(df)))
  if (verbose) cat(" clean_ciss_schema(): received tidy {date, ciss}. No changes.\n")
  df %>% arrange(date)
}