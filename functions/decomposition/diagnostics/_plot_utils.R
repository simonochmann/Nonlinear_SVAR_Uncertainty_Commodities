# ---- Shared plotting utilities for Nonlinear SVAR ----
auto_units <- function(x, mode = c("auto","raw","percent","bps","ppm")) {
  mode <- match.arg(mode)
  rng  <- suppressWarnings(max(abs(x), na.rm = TRUE))
  if (mode == "raw")     return(list(scale = 1,    suffix = "",    lab = function(z) scales::label_number(accuracy = 0.001)(z)))
  if (mode == "percent") return(list(scale = 100,  suffix = "%",   lab = function(z) scales::label_number(accuracy = 0.01)(z)))
  if (mode == "bps")     return(list(scale = 1e4,  suffix = "bps", lab = function(z) scales::label_number(accuracy = 0.01)(z)))
  if (mode == "ppm")     return(list(scale = 1e6,  suffix = "ppm", lab = function(z) scales::label_number(accuracy = 0.01)(z)))
  if (is.finite(rng) && rng < 1e-4) return(list(scale = 1e6, suffix = "ppm", lab = function(z) scales::label_number(accuracy = 0.01)(z)))
  if (is.finite(rng) && rng < 1e-2) return(list(scale = 1e4, suffix = "bps", lab = function(z) scales::label_number(accuracy = 0.01)(z)))
  if (is.finite(rng) && rng < 1)    return(list(scale = 100, suffix = "%",  lab = function(z) scales::label_number(accuracy = 0.01)(z)))
  list(scale = 1, suffix = "", lab = function(z) scales::label_number(accuracy = 0.001)(z))
}

wrap_lab <- function(x, width = 16) stringr::str_wrap(as.character(x), width)

theme_nlsvar <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(linewidth = 0.25, colour = "grey90"),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.35, colour = "grey85"),
      plot.title.position  = "plot",
      plot.caption.position= "plot",
      plot.title = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_rect(fill = "grey95", colour = NA),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 8))
    )
}

save_png <- function(plot, path, width = 9, height = 5.5, dpi = 400, bg = "white") {
  fs::dir_create(dirname(path))
  ggplot2::ggsave(filename = path, plot = plot, width = width, height = height, dpi = dpi, bg = bg)
  invisible(path)
}
