#' Plot Regime-Specific Time Series Paths with Enhancements (robust)
#'
#' Visualizes actual vs fitted values of a selected variable from a TVAR model
#' with shaded regime areas and optional residuals. Robust to indexing/shape issues.
#'
#' @param model A fitted TVAR model object with $regimes$low/$regimes$high lists.
#' @param variable The endogenous variable to plot (character, must exist in Y).
#' @param time_index NULL (auto), a vector of length n_low+n_high, or a list(list(low=...,high=...)).
#' @param show_fitted Logical; overlay fitted values.
#' @param show_residuals Logical; add residuals column to data (not plotted by default).
#' @param save_path Optional path to save the figure.
#' @param title Optional plot title.
#' @param subtitle Optional subtitle.
#' @param verbose Logical; print extras.
#' @return Invisibly returns a ggplot object (and saves to disk if save_path given).
#' @export
plot_tvar_regime_paths <- function(
    model,
    variable,
    time_index = NULL,
    show_fitted = TRUE,
    show_residuals = FALSE,
    save_path = NULL,
    title = NULL,
    subtitle = NULL,
    verbose = FALSE
) {
  `%||%` <- function(a,b) if (is.null(a)) b else a
  stopifnot("regimes" %in% names(model))
  stopifnot(!is.null(model$regimes$low$Y), !is.null(model$regimes$high$Y))
  stopifnot(variable %in% colnames(as.data.frame(model$regimes$low$Y)))
  
  # -- helpers
  as_num_col <- function(obj, var, n_expected) {
    if (is.null(obj)) return(rep(NA_real_, n_expected))
    df <- as.data.frame(obj)
    if (!var %in% colnames(df)) return(rep(NA_real_, n_expected))
    v <- df[[var]]
    v <- as.numeric(v)
    len <- length(v)
    if (len < n_expected) {
      v <- c(v, rep(NA_real_, n_expected - len))
    } else if (len > n_expected) {
      v <- v[seq_len(n_expected)]
    }
    v
  }
  
  # -- extract actuals
  Y_low  <- as.data.frame(model$regimes$low$Y)
  Y_high <- as.data.frame(model$regimes$high$Y)
  actual_low  <- as.numeric(Y_low[[variable]])
  actual_high <- as.numeric(Y_high[[variable]])
  n_low  <- length(actual_low)
  n_high <- length(actual_high)
  total_n <- n_low + n_high
  
  # -- extract fitted & residuals safely
  fitted_low   <- as_num_col(model$regimes$low$fitted,   variable, n_low)
  fitted_high  <- as_num_col(model$regimes$high$fitted,  variable, n_high)
  resid_low    <- as_num_col(model$regimes$low$residuals,  variable, n_low)
  resid_high   <- as_num_col(model$regimes$high$residuals, variable, n_high)
  
  # -- time index build (robust)
  idx <- NULL
  if (is.null(time_index)) {
    idx <- seq_len(total_n)
  } else if (is.list(time_index) && !is.null(time_index$low) && !is.null(time_index$high)) {
    if (length(time_index$low) == n_low && length(time_index$high) == n_high) {
      idx <- c(time_index$low, time_index$high)
    } else {
      warning("[plot_tvar_regime_paths] time_index list lengths mismatch; using sequential index.")
      idx <- seq_len(total_n)
    }
  } else if (length(time_index) == total_n) {
    idx <- time_index
  } else {
    warning("[plot_tvar_regime_paths] time_index provided but length != n_low+n_high; using sequential index.")
    idx <- seq_len(total_n)
  }
  
  df <- dplyr::tibble(
    time   = idx,
    actual = c(actual_low, actual_high),
    fitted = c(fitted_low, fitted_high),
    regime = factor(c(rep("low", n_low), rep("high", n_high)), levels = c("low","high"))
  )
  if (show_residuals) {
    df$residual <- c(resid_low, resid_high)
  }
  
  # -- plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = time)) +
    ggplot2::geom_rect(
      data = df,
      ggplot2::aes(xmin = time - 0.5, xmax = time + 0.5, ymin = -Inf, ymax = Inf, fill = regime),
      alpha = 0.06,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_line(ggplot2::aes(y = actual, color = "Actual"), linewidth = 1.1) +
    { if (show_fitted) ggplot2::geom_line(ggplot2::aes(y = fitted, color = "Fitted"), linewidth = 0.9, linetype = "dashed") } +
    ggplot2::scale_color_manual(values = c("Actual" = "black", "Fitted" = "darkgreen")) +
    ggplot2::scale_fill_manual(values = c("low" = "steelblue", "high" = "tomato")) +
    ggplot2::labs(
      title    = title %||% glue::glue("TVAR Regime Paths for '{variable}'"),
      subtitle = subtitle %||% "Shaded by Regime (Low/High)",
      x = "Time", y = variable, color = "Series"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  
  if (verbose) message("[plot_tvar_regime_paths] n_low=", n_low, " n_high=", n_high, " total=", total_n)
  
  if (!is.null(save_path)) {
    dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
    ggplot2::ggsave(save_path, p, width = 9, height = 5.5)
    message(glue::glue("Saved regime plot to: {save_path}"))
  } else {
    print(p)
  }
  invisible(p)
}