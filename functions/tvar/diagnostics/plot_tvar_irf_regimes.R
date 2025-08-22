# functions/tvar/diagnostics/plot_tvar_irf_regimes.R
# Plots IRFs by regime using model$irf summary; returns ggplot object

plot_tvar_irf_regimes <- function(
    model,
    impulse,
    response,
    horizon = NULL,      # default: take from model$irf
    ci_level = NULL,     # default: take from model$irf
    save_path = NULL,
    verbose = TRUE
) {
  if (is.null(model$irf)) stop("model$irf not found. Run compute_tvar_irf or orchestrator first.")
  irf <- model$irf
  if (is.null(horizon))  horizon  <- irf$settings$horizon
  if (is.null(ci_level)) ci_level <- irf$settings$ci_level
  
  get_series <- function(sm, imp, resp) {
    imp_i  <- match(imp, irf$settings$impulses)
    resp_i <- match(resp, irf$settings$responses)
    if (is.na(imp_i) || is.na(resp_i)) stop("Impulse/response not found in IRF settings.")
    M <- sm[, , resp_i, imp_i, drop = FALSE]  # q x h x 1 x 1
    data.frame(
      h = 0:horizon,
      q_lo = M[1, , 1, 1],
      q_med = M[2, , 1, 1],
      q_hi = M[3, , 1, 1]
    )
  }
  
  df_low  <- get_series(irf$regimes$low$summary,  impulse, response)  |> dplyr::mutate(regime = "low")
  df_high <- get_series(irf$regimes$high$summary, impulse, response)  |> dplyr::mutate(regime = "high")
  df <- dplyr::bind_rows(df_low, df_high)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = h, y = q_med, ymin = q_lo, ymax = q_hi)) +
    ggplot2::geom_ribbon(alpha = 0.2) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::facet_wrap(~ regime, nrow = 1) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      title = glue::glue("IRFs: {impulse} → {response}"),
      subtitle = glue::glue("{round(ci_level*100)}% bands, horizon = {horizon}"),
      x = "Horizon",
      y = "Response"
    ) +
    ggplot2::theme_minimal(base_size = 12)
  
  if (!is.null(save_path)) {
    fs::dir_create(dirname(save_path))
    ggplot2::ggsave(save_path, p, width = 10, height = 4, dpi = 300)
    if (isTRUE(verbose)) message("Saved IRF regime plot to: ", save_path)
  }
  
  return(p)
}