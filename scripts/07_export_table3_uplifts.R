# scripts/07_export_table3_uplifts.R
suppressPackageStartupMessages({
  library(here); library(fs); library(readr); library(dplyr); library(tidyr); library(tibble); library(stringr)
})

MODELS_DIR <- here::here("models","tvar")
DATA_DIR   <- here::here("data","tvar")
OUT_DIR    <- fs::path(MODELS_DIR, "artifacts","tables")
fs::dir_create(OUT_DIR)

`%||%` <- function(a,b) if (!is.null(a)) a else b

# fractional, envelope-based half-life at 60% of peak
half_life_peak_env_frac <- function(x, frac = 0.6) {
  x <- abs(as.numeric(x))
  if (!any(is.finite(x))) return(NA_real_)
  hpk <- which.max(x)
  if (!length(hpk) || !is.finite(x[hpk])) return(NA_real_)
  # monotone envelope from the peak onward (ignores small rebounds)
  if (hpk < length(x)) for (h in (hpk+1L):length(x)) x[h] <- min(x[h-1L], x[h])
  thr <- frac * x[hpk]
  if (x[hpk] <= thr) return(0)
  for (h in hpk:(length(x)-1L)) {
    if (x[h+1L] <= thr) {
      y1 <- x[h]; y2 <- x[h+1L]
      alpha <- if (y1 == y2) 1 else (y1 - thr)/(y1 - y2)   # α ∈ (0,1]
      return((h + alpha) - hpk)    # fractional months after the peak
    }
  }
  NA_real_  # never crossed within horizon
}

# ---- load newest model for tag ("vix","vxo","jln") ----
load_latest <- function(tag) {
  tag <- tolower(tag)
  files <- fs::dir_ls(MODELS_DIR, type = "file", recurse = FALSE)
  pats  <- c(sprintf("^%s_tvar_model_latest\\.rds$", tag),
             sprintf("^%s_tvar_model_.*\\.rds$",   tag))
  hits <- files[Reduce(`|`, lapply(pats, function(p) grepl(p, fs::path_file(files), ignore.case=TRUE)))]
  if (!length(hits)) return(NULL)
  hit <- hits[order(fs::file_info(hits)$modification_time, decreasing=TRUE)][1]
  readRDS(hit)
}

# ---- helpers: groups, MA/GIRF, half-life (peak-based), robust A/Σ ----
map_group <- function(vars){
  v <- tolower(vars)
  grp <- ifelse(grepl("crude|wti|ngas|gas", v), "Energy",
                ifelse(grepl("gold|silver|platinum", v), "Precious",
                       ifelse(grepl("aluminium|aluminum|copper|lead|nickel|tin|zinc", v), "Industrial",
                              ifelse(grepl("cocoa|coffee|cotton|maize|soybean|soybeans|sugar|wheat", v), "Agriculture","Other"))))
  factor(grp, levels = c("Energy","Industrial","Precious","Agriculture","Other"))
}

ma_coefs <- function(A_list, H){
  k <- nrow(A_list[[1]]); p <- length(A_list)
  Phi <- array(0, dim = c(k,k,H+1)); diag(Phi[,,1]) <- 1
  for (h in 2:(H+1)) {
    acc <- matrix(0,k,k)
    for (m in 1:min(p, h-1)) acc <- acc + Phi[,,h-m] %*% A_list[[m]]
    Phi[,,h] <- acc
  }
  Phi
}
girf_cube <- function(A_list, Sigma, H){
  k <- nrow(A_list[[1]]); Phi <- ma_coefs(A_list, H)
  Dinv <- diag(1/sqrt(diag(Sigma)), k, k)   # generalized normalization
  V    <- Sigma %*% Dinv
  arr  <- array(0, dim = c(k,k,H+1))
  for (h in 1:(H+1)) arr[,,h] <- Phi[,,h] %*% V
  arr
}
half_life_peak <- function(x, frac=0.5){
  x <- as.numeric(x); if (!any(is.finite(x))) return(NA_real_)
  hpk <- which.max(abs(x)); if (!length(hpk) || !is.finite(x[hpk])) return(NA_real_)
  thr <- frac*abs(x[hpk]); idx <- seq.int(hpk, length(x))
  hit <- idx[ which(abs(x[idx]) <= thr)[1] ]
  if (length(hit)) hit - hpk else NA_real_
}

normalize_A_list <- function(A, K) {
  if (is.null(A)) return(NULL)
  if (is.list(A)) return(A)
  dims <- dim(A)
  if (length(dims)==3) return(lapply(seq_len(dims[3]), function(j) A[,,j,drop=FALSE]))
  if (length(dims)==2) {
    nr <- dims[1]; nc <- dims[2]
    if (nr==K && (nc %% K)==0) {
      p <- nc %/% K; return(lapply(seq_len(p), function(j) A[,((j-1)*K+1):(j*K),drop=FALSE]))
    }
    if ((nr %% K)==0 && nc==K) {
      p <- nr %/% K; return(lapply(seq_len(p), function(j) A[((j-1)*K+1):(j*K),,drop=FALSE]))
    }
  }
  NULL
}
pluck2 <- function(x, ...) {
  out <- tryCatch({ for (nm in list(...)) x <- x[[nm]]; x }, error = function(e) NULL)
  if (is.null(out)) NULL else out
}
extract_A_Sigma <- function(est, regime=c("low","high")){
  regime <- match.arg(regime)
  K <- length(est$variables)
  A_obj <- NULL
  A_obj <- A_obj %||% pluck2(est, "regimes", regime, "Ak")
  A_obj <- A_obj %||% pluck2(est, "Ak", if (regime=="low") "L" else "H")
  A_obj <- A_obj %||% pluck2(est, "regimes", regime, "A")   # your object
  A_list <- normalize_A_list(A_obj, K)
  Sigma  <- NULL
  Sigma  <- Sigma %||% pluck2(est, "regimes", regime, "Sigma")
  Sigma  <- Sigma %||% pluck2(est, "Sigma", if (regime=="low") "L" else "H")
  if (is.null(Sigma)) {
    E <- pluck2(est, "regimes", regime, "residuals") %||% pluck2(est, "residuals", if (regime=="low") "L" else "H")
    if (!is.null(E)) Sigma <- stats::cov(as.matrix(E), use="pairwise.complete.obs")
  }
  list(A_list=A_list, Sigma=Sigma)
}

# ---- pooled linear VAR (OLS) on a prepared Y matrix ----
estimate_var_ols <- function(Y, p){
  Y <- as.matrix(Y); Tn <- nrow(Y); K <- ncol(Y)
  if ((Tn - p) <= max(50, K*5)) stop("Not enough obs for VAR")
  # build lagged Z
  Z <- NULL
  for (lag in 1:p) Z <- cbind(Z, Y[(p-lag+1):(Tn-lag), , drop=FALSE])
  Z <- cbind(1, Z)                               # intercept
  Yt <- Y[(p+1):Tn, , drop=FALSE]
  B  <- solve(crossprod(Z), crossprod(Z, Yt))    # (1+Kp) x K
  E  <- Yt - Z %*% B
  Sigma <- crossprod(E) / (nrow(E) - (1+K*p))
  # split B into A_k lists (KxK)
  A_list <- lapply(seq_len(p), function(k) t(B[(2 + (k-1)*K):(1 + k*K), , drop=FALSE]))
  list(A_list=A_list, Sigma=Sigma)
}

# ---- prepare Y for a given proxy (same commodity set/sample) ----
JOETS18 <- c("aluminium","cocoa","coffee_arabic","copper","cotton_a_indx","crude_wti",
             "gold","lead","maize","ngas_us","nickel","platinum","silver","soybeans",
             "sugar_wld","tin","wheat_us_hrw","zinc")

prep_Y_from_input <- function(tag){
  in_file <- fs::path(DATA_DIR, sprintf("tvar_input_%s.csv", tolower(tag)))
  if (!fs::file_exists(in_file)) return(NULL)
  df <- readr::read_csv(in_file, show_col_types = FALSE)
  if (!inherits(df$date, "Date")) suppressWarnings(df$date <- as.Date(df$date))
  idx_col <- if (toupper(tag) %in% names(df)) toupper(tag) else tolower(tag)
  if (!idx_col %in% names(df)) return(NULL)
  commod_all <- unique(df$commodity_id)
  commod_sel <- intersect(JOETS18, commod_all)
  # long -> wide returns
  Ywide <- df %>%
    dplyr::filter(commodity_id %in% commod_sel) %>%
    dplyr::select(date, commodity_id, ret) %>%
    tidyr::pivot_wider(names_from = commodity_id, values_from = ret) %>%
    dplyr::arrange(date)
  # require index to match TVAR sample (complete cases incl. index)
  joined <- dplyr::left_join(Ywide, dplyr::distinct(df, date, !!idx_col), by="date")
  keep <- stats::complete.cases(joined)
  Y <- as.matrix(joined[keep, commod_sel, drop=FALSE])
  # standardize to z-scores (as in 04)
  Y[] <- scale(Y)
  colnames(Y) <- make.names(commod_sel, unique=TRUE)
  Y
}

# ---- compute uplifts for one proxy tag ("vix"/"vxo"/"jln") ----
uplifts_for_proxy <- function(tag){
  est <- if (tolower(tag) %in% c("vix","vxo")) load_latest("vix") %||% load_latest("vxo") else load_latest("jln")
  if (is.null(est)) return(tibble())
  p   <- as.integer(est$spec$p %||% 2L)
  Y   <- prep_Y_from_input(tag); if (is.null(Y)) return(tibble())
  # pooled linear VAR on this sample
  var_lin <- estimate_var_ols(Y, p)
  # TVAR high-regime pieces
  tvar_hi <- extract_A_Sigma(est, "high")
  if (is.null(tvar_hi$A_list) || is.null(tvar_hi$Sigma)) return(tibble())
  # GIRFs
  Hmax <- 36L
  GI_lin <- girf_cube(var_lin$A_list, var_lin$Sigma, Hmax)
  GI_hi  <- girf_cube(tvar_hi$A_list, tvar_hi$Sigma, Hmax)
  # metrics by group
  vars <- est$variables
  groups <- map_group(vars)
  K <- length(vars)
  groups_levels <- c("Energy","Industrial","Precious","Agriculture")
  out <- lapply(groups_levels, function(g){
    idx_l <- which(groups == g); if (!length(idx_l)) return(NULL)
    # median over (ℓ∈g, i) of peak |IRF|
    peaks_hi  <- as.vector(sapply(idx_l, function(ell) sapply(1:K, function(i) max(abs(GI_hi[ell,i,])) )))
    peaks_lin <- as.vector(sapply(idx_l, function(ell) sapply(1:K, function(i) max(abs(GI_lin[ell,i,])) )))
    # half-life (peak-based) medians
    hls_hi  <- as.vector(sapply(idx_l, function(ell) sapply(1:K, function(i) half_life_peak_env_frac(GI_hi[ell,i,],  frac=0.6))))
    hls_lin <- as.vector(sapply(idx_l, function(ell) sapply(1:K, function(i) half_life_peak_env_frac(GI_lin[ell,i,], frac=0.6))))
    peak_ratio <- stats::median(peaks_hi,  na.rm=TRUE) / stats::median(peaks_lin, na.rm=TRUE)
    d_hlife    <- stats::median(hls_hi,   na.rm=TRUE) - stats::median(hls_lin,  na.rm=TRUE)
    tibble(proxy = if (tolower(tag) %in% c("vix","vxo")) "Volatility (VXO→VIX)" else "JLN (macro uncertainty)",
           group = g,
           peak_ratio = as.numeric(peak_ratio),
           d_half_life = as.numeric(d_hlife))
  })
  dplyr::bind_rows(out)
}

# ---- run for both proxies and write CSV ----
U_vol <- uplifts_for_proxy("vix") %||% uplifts_for_proxy("vxo")
U_jln <- uplifts_for_proxy("jln")
U_all <- dplyr::bind_rows(U_vol, U_jln)

readr::write_csv(U_all, fs::path(OUT_DIR, "linear_vs_tvar_uplifts.csv"))
message("✓ Wrote: ", fs::path(OUT_DIR, "linear_vs_tvar_uplifts.csv"))