compare_tvar_irf_runs <- function(model_path_a, model_path_b, impulse, response, save_path = NULL) {
  A <- readRDS(model_path_a); B <- readRDS(model_path_b)
  if (is.null(A$irf) || is.null(B$irf)) stop("Both models must contain $irf")
  pick <- function(M) {
    imp_i <- match(impulse, M$irf$settings$impulses)
    resp_i<- match(response, M$irf$settings$responses)
    list(low  = as.numeric(M$irf$regimes$low$summary [2, , resp_i, imp_i]),
         high = as.numeric(M$irf$regimes$high$summary[2, , resp_i, imp_i]))
  }
  a <- pick(A); b <- pick(B)
  H <- A$irf$settings$horizon
  df <- dplyr::bind_rows(
    data.frame(h = 0:H, val = a$low,  regime = "low",  run = "A"),
    data.frame(h = 0:H, val = b$low,  regime = "low",  run = "B"),
    data.frame(h = 0:H, val = a$high, regime = "high", run = "A"),
    data.frame(h = 0:H, val = b$high, regime = "high", run = "B")
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(h, val, linetype = run)) +
    ggplot2::geom_line() + ggplot2::facet_wrap(~ regime, nrow = 1) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(title = glue::glue("IRF median: {impulse} → {response}"), x = "h", y = "median") +
    ggplot2::theme_minimal(base_size = 12)
  if (!is.null(save_path)) { fs::dir_create(dirname(save_path)); ggplot2::ggsave(save_path, p, width = 10, height = 4, dpi = 300) }
  invisible(p)
}
