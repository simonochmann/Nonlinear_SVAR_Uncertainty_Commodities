#' Collect CSV/figures/logs into a release directory, plus ENVIRONMENT.md & MANIFEST.json
#' Returns a list with paths; always CPU-light.
collect_artifacts <- function(
    release_dir,
    data_dir,
    fig_decdir,
    fig_scndir,
    log_scndir,
    log_decdir,
    renv_lock = NULL
) {
  requireNamespace("fs", quietly = TRUE)
  requireNamespace("digest", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)
  requireNamespace("jsonlite", quietly = TRUE)
  
  fs::dir_create(release_dir)
  
  safe_list <- function(path, glob = NULL, regexp = NULL) {
    if (!fs::dir_exists(path)) return(character(0))
    fs::dir_ls(path, glob = glob, regexp = regexp, type = "file", recurse = FALSE)
  }
  safe_copy_into <- function(files, dest_subdir) {
    files <- files[file.exists(files)]
    if (!length(files)) return(character(0))
    dest <- fs::path(release_dir, dest_subdir)
    fs::dir_create(dest)
    purrr::walk(files, ~fs::file_copy(.x, fs::path(dest, basename(.x)), overwrite = TRUE))
    fs::path(dest, basename(files))
  }
  write_text <- function(txt, path) {
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    cat(txt, file = path, sep = "")
    path
  }
  
  # Collect typical artifacts from 07/08
  csv_candidates <- c(
    fs::path(data_dir, "scenario_results_tidy_fast.csv"),
    fs::path(data_dir, "forecast_decomposition_delta.csv"),
    fs::path(data_dir, "forecast_decomposition_summary.csv"),
    fs::path(data_dir, "decomposition_delta_tidy.csv"),
    fs::path(data_dir, "decomposition_contributions_tidy.csv"),
    fs::path(data_dir, "decomposition_contributions.csv") # packager-friendly
  )
  
  png_decomp <- safe_list(fig_decdir, glob = "*.png")
  png_scens  <- safe_list(fig_scndir,  glob = "*.png")
  
  log_md_json <- c(
    safe_list(log_scndir, regexp = "\\.(md|json)$"),
    safe_list(log_decdir, regexp = "\\.(md|json)$")
  )
  
  rel_csv   <- safe_copy_into(csv_candidates, fs::path("artifacts","data"))
  rel_png_d <- safe_copy_into(png_decomp,    fs::path("artifacts","figures","decomposition"))
  rel_png_s <- safe_copy_into(png_scens,     fs::path("artifacts","figures","scenarios"))
  rel_logs  <- safe_copy_into(log_md_json,   fs::path("artifacts","logs"))
  
  # Copy renv.lock if present
  rel_renv <- character(0)
  if (!is.null(renv_lock) && file.exists(renv_lock)) {
    rel_renv <- safe_copy_into(renv_lock, "config")
  }
  
  # ENVIRONMENT.md
  env_txt <- paste0(
    "# Environment\n\n",
    "*Timestamp:* ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n\n",
    "```r\n", paste(capture.output(utils::sessionInfo()), collapse = "\n"), "\n```\n"
  )
  env_md <- fs::path(release_dir, "ENVIRONMENT.md")
  write_text(env_txt, env_md)
  
  # MANIFEST.json (relative paths + sha256)
  rel_files <- fs::dir_ls(release_dir, recurse = TRUE, type = "file")
  manifest <- purrr::map_df(rel_files, function(f) {
    tibble::tibble(
      file   = fs::path_rel(f, start = release_dir),
      bytes  = fs::file_info(f)$size,
      sha256 = digest::digest(file = f, algo = "sha256")
    )
  })
  man_path <- fs::path(release_dir, "MANIFEST.json")
  jsonlite::write_json(manifest, man_path, pretty = TRUE, auto_unbox = TRUE)
  
  list(
    rel_csv   = rel_csv,
    rel_png_d = rel_png_d,
    rel_png_s = rel_png_s,
    rel_logs  = rel_logs,
    rel_renv  = rel_renv,
    env_md    = env_md,
    manifest  = man_path
  )
}