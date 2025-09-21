# scripts/09_package_export.R
# Packages artifacts from 07/08 into output/releases/, writes manifest & README,
# renders a Quarto appendix, merges .bib, then zips.

suppressPackageStartupMessages({
  library(here); library(fs); library(glue); library(jsonlite); library(digest); library(tibble); library(purrr)
})

# Load helpers 
source(here::here("functions","export","collect_artifacts.R"))
source(here::here("functions","export","render_appendix_qmd.R"))
source(here::here("functions","export","synthesize_bibliography.R"))
source(here::here("functions","export","write_release_readme.R"))

`%||%` <- function(x,y) if (is.null(x)) y else x

# Config 
CFG <- list(
  # identity
  ROOT          = here::here(),
  PATHS_YML     = here::here("config","paths.yml"),
  ACTIVE_FALLBK = "vix",
  
  # inputs to collect
  DATA_DIR      = here::here("data","scenarios"),
  FIG_DEC_DIR   = here::here("figures","decomposition"),
  FIG_SCN_DIR   = here::here("figures","scenarios"),
  LOG_SCN_DIR   = here::here("logs","scenarios"),
  LOG_DEC_DIR   = here::here("logs","decomposition"),
  RENV_LOCK     = here::here("renv.lock"),
  
  # appendix / docs
  APPENDIX_QMD  = here::here("paper","appendix.qmd"),   # change if needed
  MERGE_BIB_DIR = here::here("paper"),                  # search for *.bib here
  RENDER_APPENDIX = TRUE,
  MERGE_BIB       = TRUE,
  
  # release output root
  OUT_REL_ROOT  = here::here("output","releases")
)

# Read active uncertainty index 
get_active_ix <- function(paths_yml, fallback = "vix") {
  if (!file.exists(paths_yml)) return(fallback)
  if (!requireNamespace("yaml", quietly = TRUE)) return(fallback)
  y <- tryCatch(yaml::read_yaml(paths_yml), error = function(e) NULL)
  tolower(y$uncertainty$active %||% fallback)
}

active_ix <- get_active_ix(CFG$PATHS_YML, CFG$ACTIVE_FALLBK)
stamp     <- format(Sys.time(), "%Y%m%d_%H%M%S")
rel_name  <- glue("release_{active_ix}_{stamp}")
rel_dir   <- fs::path(CFG$OUT_REL_ROOT, rel_name)
fs::dir_create(rel_dir)

# Collect artifacts 
art <- collect_artifacts(
  release_dir   = rel_dir,
  data_dir      = CFG$DATA_DIR,
  fig_decdir    = CFG$FIG_DEC_DIR,
  fig_scndir    = CFG$FIG_SCN_DIR,
  log_scndir    = CFG$LOG_SCN_DIR,
  log_decdir    = CFG$LOG_DEC_DIR,
  renv_lock     = CFG$RENV_LOCK
)

# render appendix (.qmd/.Rmd)
app_out <- NULL
if (CFG$RENDER_APPENDIX && file.exists(CFG$APPENDIX_QMD)) {
  app_out <- render_appendix_qmd(
    qmd_path    = CFG$APPENDIX_QMD,
    release_dir = rel_dir
  )
}

# synthesize bibliography 
bib_out <- NULL
if (CFG$MERGE_BIB && fs::dir_exists(CFG$MERGE_BIB_DIR)) {
  bib_out <- synthesize_bibliography(
    search_dir  = CFG$MERGE_BIB_DIR,
    release_dir = rel_dir
  )
}

# README_RELEASE.md 
readme_path <- write_release_readme(
  release_dir   = rel_dir,
  active_ix     = active_ix,
  stamp         = stamp,
  art           = art,
  appendix_info = app_out,
  bib_info      = bib_out
)

# Zip / tarball 
safe_zip <- function(zip_path, dir_to_zip) {
  old <- getwd(); on.exit(setwd(old), add = TRUE)
  setwd(fs::path_dir(dir_to_zip))
  base <- fs::path_file(dir_to_zip)
  files <- fs::dir_ls(base, recurse = TRUE, type = "file")
  utils::zip(zipfile = zip_path, files = files)
  zip_path
}
zip_path <- fs::path(CFG$OUT_REL_ROOT, paste0(rel_name, ".zip"))
zip_path <- safe_zip(zip_path, rel_dir)

# tar creation across OS/ shells
safe_tar <- function(tar_path, dir_to_tar) {
    parent <- fs::path_abs(fs::path_dir(dir_to_tar))
    base   <- fs::path_file(dir_to_tar)
    if (!fs::dir_exists(dir_to_tar)) {
        stop("safe_tar: directory not found: ", dir_to_tar)
      }
    tar_bin <- Sys.which("tar")
    if (nzchar(tar_bin)) {
          args <- c("-C", shQuote(parent), "-czf", shQuote(tar_path), shQuote(base))
          status <- suppressWarnings(system2(tar_bin, args = args))
          if (is.na(status) || status != 0L) {
              warning("system tar failed (status=", status, "); falling back to utils::tar().")
              old <- getwd(); on.exit(setwd(old), add = TRUE)
              setwd(parent)
              utils::tar(tarfile = tar_path, files = base, compression = "gzip")
            }
        } else {
            old <- getwd(); on.exit(setwd(old), add = TRUE)
            setwd(parent)
            utils::tar(tarfile = tar_path, files = base, compression = "gzip")
          }
    tar_path
  }
tar_path <- fs::path(CFG$OUT_REL_ROOT, paste0(rel_name, ".tar.gz"))
tar_path <- safe_tar(tar_path, rel_dir)

message("Release ready: ", fs::path_rel(rel_dir))
message("Zip:         ", fs::path_rel(zip_path))
message("Tarball:     ", fs::path_rel(tar_path))

# after tarball creation + messages
checksums_path <- fs::path(CFG$OUT_REL_ROOT, paste0(rel_name, "_CHECKSUMS.txt"))
writeLines(c(
  paste("SHA256", digest::digest(file = zip_path, algo = "sha256"), fs::path_file(zip_path)),
  paste("SHA256", digest::digest(file = tar_path, algo = "sha256"), fs::path_file(tar_path))
), con = checksums_path)
message("Checksums:   ", fs::path_rel(checksums_path))

