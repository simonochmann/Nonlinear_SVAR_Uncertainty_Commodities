dedupe_normalise_fevd <- function(fevd_tbl) {
  keys <- intersect(c("uncertainty","regime","response","impulse","t"), names(fevd_tbl))
  base_keys <- setdiff(keys, "impulse")
  
  fevd_tbl %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(fevd_share = sum(.data$fevd_share, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(base_keys))) %>%
    dplyr::mutate(total = sum(.data$fevd_share, na.rm = TRUE),
                  fevd_share = dplyr::case_when(total > 0 ~ .data$fevd_share / total,
                                                TRUE      ~ 0)) %>%
    dplyr::select(-total) %>%
    dplyr::ungroup()
}
