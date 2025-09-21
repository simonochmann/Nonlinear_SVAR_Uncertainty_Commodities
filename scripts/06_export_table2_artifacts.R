# scripts/06_export_table2_artifacts.R
suppressPackageStartupMessages({
  library(here); library(fs); library(readr); library(dplyr); library(stringr); library(tibble)
})

MODELS_DIR <- here::here("models","tvar")
OUT_DIR    <- fs::path(MODELS_DIR, "artifacts","tables")
fs::dir_create(OUT_DIR)

# ---------- helpers (robust extraction) ----------
`%||%` <- function(a,b) if (!is.null(a)) a else b

# Load newest model RDS for a tag ("vix", "vxo", "jln")
load_latest <- function(tag) {
  tag <- tolower(tag)
  files <- fs::dir_ls(MODELS_DIR, type = "file", recurse = FALSE)
  # accept both '..._latest.rds' and timestamped '..._YYYYMMDD....rds'
  pats <- c(
    sprintf("^%s_tvar_model_latest\\.rds$", tag),
    sprintf("^%s_tvar_model_.*\\.rds$",   tag)
  )
  hits <- files[Reduce(`|`, lapply(pats, function(p)
    grepl(p, fs::path_file(files), ignore.case = TRUE)))]
  if (!length(hits)) return(NULL)
  hit <- hits[order(fs::file_info(hits)$modification_time, decreasing = TRUE)][1]
  readRDS(hit)
}

# Split any reasonable A container into a {A_k}_{k=1..p} list of KxK
normalize_A_list <- function(A, K, p_hint = NULL) {
  if (is.null(A)) return(NULL)
  if (is.list(A)) return(A)
  dims <- dim(A)
  if (length(dims) == 3) {
    p <- dims[3]
    return(lapply(seq_len(p), function(j) A[,,j, drop = FALSE]))
  }
  if (length(dims) == 2) {
    nr <- dims[1]; nc <- dims[2]
    # Case 1: K x (K*p) — blocks across columns
    if (nr == K && (nc %% K) == 0) {
      p <- nc %/% K
      return(lapply(seq_len(p), function(j) A[, ((j-1)*K+1):(j*K), drop = FALSE]))
    }
    # Case 2: (K*p) x K — blocks down rows
    if ((nr %% K) == 0 && nc == K) {
      p <- nr %/% K
      return(lapply(seq_len(p), function(j) A[((j-1)*K+1):(j*K), , drop = FALSE]))
    }
  }
  NULL
}

# Persistence index across horizon H: PI_{i->ℓ} = sum_{h=0}^H |GIRF_{i->ℓ}(h)|
# Group summary: for each response ℓ compute median_i PI_{i->ℓ}, then median across ℓ in the group.
persistence_by <- function(est, proxy_label, Hpi = 24L) {
  vars <- est$variables
  grp  <- map_group(vars); k <- length(vars)
  pieces <- extract_A_Sigma(est, "high")
  A_list <- pieces$A_list; Sigma <- pieces$Sigma
  if (is.null(A_list) || is.null(Sigma)) return(tibble::tibble())
  GI <- girf_cube(A_list, Sigma, Hpi)  # k (resp) × k (shock) × (H+1)
  # PI per response (median over shocks)
  pi_l <- rep(NA_real_, k)
  for (ell in seq_len(k)) {
    pis_i <- sapply(seq_len(k), function(i) sum(abs(GI[ell, i, ]), na.rm = TRUE))
    pi_l[ell] <- stats::median(pis_i, na.rm = TRUE)
  }
  tibble::tibble(proxy = proxy_label,
                 response = vars,
                 group    = as.character(grp),
                 regime   = "H",
                 H        = Hpi,
                 PI       = pi_l) |>
    dplyr::group_by(proxy, group, regime, H) |>
    dplyr::summarise(PI = stats::median(PI, na.rm = TRUE), .groups = "drop")
}

# Rebuild A_k from stacked B (rows: [1] intercept, then K rows per lag)
reconstruct_A_from_B <- function(B, K, p) {
  if (is.null(B) || is.null(p) || is.na(p)) return(NULL)
  A_list <- vector("list", p)
  for (k in seq_len(p)) {
    r1 <- 1 + (k-1)*K + 1
    r2 <- 1 + k*K
    blk <- B[r1:r2, , drop = FALSE]
    A_list[[k]] <- t(blk)
  }
  A_list
}

pluck2 <- function(x, ...) {
  out <- tryCatch({ for (nm in list(...)) x <- x[[nm]]; x }, error = function(e) NULL)
  if (is.null(out)) NULL else out
}

extract_A_Sigma <- function(est, regime = c("low","high")) {
  regime <- match.arg(regime)
  K <- length(est$variables)
  p <- as.integer(est$spec$p %||% est$metadata$p %||% NA_integer_)
  
  # Try A list/array/matrix under common paths
  A_obj <- NULL
  A_obj <- A_obj %||% pluck2(est, "regimes", regime, "Ak")
  A_obj <- A_obj %||% pluck2(est, "Ak", if (regime=="low") "L" else "H")
  A_obj <- A_obj %||% pluck2(est, "regimes", regime, "A")  # <— your object has this
  
  A_list <- normalize_A_list(A_obj, K, p)
  
  # Fallback: stacked B → per-lag blocks
  if (is.null(A_list)) {
    B <- pluck2(est, "regimes", regime, "B") %||% pluck2(est, "B", if (regime=="low") "L" else "H")
    if (!is.null(B)) {
      # If B is vector, try to reshape to (1+K*p) x K using p hint
      if (is.null(dim(B)) && !is.na(p)) {
        if (length(B) == K) {
          B <- cbind(B)               # intercept only (rare); cannot reconstruct
        } else if ((length(B) %% K) == 0) {
          nrow <- length(B) / K
          B <- matrix(B, nrow = nrow, ncol = K)
        }
      }
      if (!is.null(dim(B))) A_list <- reconstruct_A_from_B(B, K, p)
    }
  }
  
  # Sigma from stored Sigma or residuals
  Sigma <- NULL
  Sigma <- Sigma %||% pluck2(est, "regimes", regime, "Sigma")
  Sigma <- Sigma %||% pluck2(est, "Sigma", if (regime=="low") "L" else "H")
  if (is.null(Sigma)) {
    E <- pluck2(est, "regimes", regime, "residuals") %||% pluck2(est, "residuals", if (regime=="low") "L" else "H")
    if (!is.null(E)) Sigma <- stats::cov(as.matrix(E), use = "pairwise.complete.obs")
  }
  
  list(A_list = A_list, Sigma = Sigma, K = K, p = p)
}

# MA / GIRF / half-life (unchanged)
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
  Dinv <- diag(1/sqrt(diag(Sigma)), k, k)
  V    <- Sigma %*% Dinv
  arr  <- array(0, dim = c(k,k,H+1))
  for (h in 1:(H+1)) arr[,,h] <- Phi[,,h] %*% V
  arr
}
half_life <- function(x){
  M <- max(abs(x), na.rm=TRUE)
  if (!is.finite(M) || M == 0) return(NA_real_)
  idx <- which(abs(x) <= 0.5*M)
  if (length(idx)) min(idx) - 1 else NA_real_
}
map_group <- function(vars){
  v <- tolower(vars)
  grp <- ifelse(grepl("crude|wti|ngas|gas", v), "Energy",
                ifelse(grepl("gold|silver|platinum", v), "Precious",
                       ifelse(grepl("aluminium|aluminum|copper|lead|nickel|tin|zinc", v), "Industrial",
                              ifelse(grepl("cocoa|coffee|cotton|maize|soybean|soybeans|sugar|wheat", v), "Agriculture", "Other"))))
  factor(grp, levels = c("Energy","Industrial","Precious","Agriculture","Other"))
}

# Peak-based half-life with monotone envelope after the peak
# Peak-based fractional half-life with monotone envelope
half_life_peak_env_frac <- function(x, frac = 0.6) {
  x <- abs(as.numeric(x))
  if (!any(is.finite(x))) return(NA_real_)
  hpk <- which.max(x)
  if (!length(hpk) || !is.finite(x[hpk])) return(NA_real_)
  # non-increasing envelope from the peak onward
  if (hpk < length(x)) for (h in (hpk+1L):length(x)) x[h] <- min(x[h-1L], x[h])
  thr <- frac * x[hpk]
  if (x[hpk] <= thr) return(0)
  # find first crossing and linearly interpolate between h and h+1
  for (h in hpk:(length(x)-1L)) {
    if (x[h+1L] <= thr) {
      y1 <- x[h]; y2 <- x[h+1L]
      frac_h <- if (y1 == y2) 1 else (y1 - thr) / (y1 - y2)  # in (0,1]
      return((h + frac_h) - hpk)
    }
  }
  NA_real_  # never reached threshold within horizon
}

# Pull regime pieces from est object robustly
get_regime_piece <- function(est, which = c("low","high"), slot = c("Ak","Sigma","residuals")){
  which <- match.arg(which); slot <- match.arg(slot)
  # Try nested first, then legacy
  x <- try(est$regimes[[which]][[slot]], silent=TRUE)
  if (!inherits(x,"try-error") && !is.null(x)) return(x)
  legacy <- if (slot=="Ak") list(L="L",H="H") else if (slot=="Sigma") list(L="L",H="H") else list(L="L",H="H")
  key <- if (which=="low") "L" else "H"
  alt <- try(est[[slot]][[ legacy[[key]] ]], silent=TRUE)
  if (inherits(alt,"try-error")) NULL else alt
}

# ---------- load models ----------
vol <- load_latest("vix"); if (is.null(vol)) vol <- load_latest("vxo")
jln <- load_latest("jln")

stopifnot(!is.null(vol), !is.null(jln))

# Dates & threshold series to reconstruct regime flags
mk_flags <- function(est){
  dates <- est$metadata$dates
  th    <- est$metadata$threshold_series
  dsel  <- est$spec$delay %||% 1L
  cstar <- if (is.list(est$threshold_value)) as.numeric(est$threshold_value[[1]]) else as.numeric(est$threshold_value)
  # Apply delay (lag) to threshold driver to match s_t definition
  th_lag <- c(rep(NA, dsel), head(th, -dsel))
  tibble(date = as.Date(dates), x = as.numeric(th_lag), high = as.integer(x > cstar))
}

`%||%` <- function(a,b) if (!is.null(a)) a else b

flags_vol <- mk_flags(vol) %>% mutate(proxy = "vxo_vix")
flags_jln <- mk_flags(jln) %>% mutate(proxy = "jln")

# Align and write regime flags
REG <- dplyr::bind_rows(
  dplyr::select(flags_vol, date, proxy, high),
  dplyr::select(flags_jln, date, proxy, high)
) |> dplyr::arrange(date, proxy)

readr::write_csv(REG, fs::path(OUT_DIR, "regime_flags.csv"))

# ---------- FEVD (state-contingent linearized within regime) ----------
Hlist <- c(12L, 24L)
groups_levels <- c("Energy","Industrial","Precious","Agriculture")

fevd_by <- function(est, proxy_label){
  vars <- est$variables; grp <- map_group(vars); k <- length(vars)
  out  <- list()
  for (reg in c("low","high")){
    pieces <- extract_A_Sigma(est, reg)
    A_list <- pieces$A_list; Sigma <- pieces$Sigma
    if (is.null(A_list) || is.null(Sigma)) next
    for (H in Hlist){
      GI <- girf_cube(A_list, Sigma, H)          # k×k×(H+1)
      num   <- apply(GI^2, c(1,2), sum)          # k×k
      denom <- rowSums(num)                      # k
      shares_lg <- sapply(groups_levels, function(g){
        idx_g <- which(grp == g)
        if (!length(idx_g)) return(rep(NA_real_, k))
        rowSums(num[, idx_g, drop=FALSE]) / denom
      })
      share_g <- colMeans(shares_lg, na.rm = TRUE)
      out[[length(out)+1]] <- tibble::tibble(
        proxy  = proxy_label,
        regime = toupper(substr(reg,1,1)),
        group  = groups_levels,
        horizon= H,
        share  = as.numeric(share_g)
      )
    }
  }
  if (length(out)) dplyr::bind_rows(out) else tibble::tibble()
}

FE_vol <- fevd_by(vol, "vxo_vix")
FE_jln <- fevd_by(jln, "jln")
FEVD   <- dplyr::bind_rows(FE_vol, FE_jln)
readr::write_csv(FEVD, fs::path(OUT_DIR, "fevd_regime_shares.csv"))

# ---------- Half-life (high state; own-shock; median by group) ----------
half_by <- function(est, proxy_label, Hmax = 36L) {
  vars <- est$variables
  grp  <- map_group(vars)
  k    <- length(vars)
  
  pieces <- extract_A_Sigma(est, "high")
  A_list <- pieces$A_list; Sigma <- pieces$Sigma
  if (is.null(A_list) || is.null(Sigma)) return(tibble::tibble())
  
  GI <- girf_cube(A_list, Sigma, Hmax)   # k (resp) × k (shock) × (H+1)
  
  # For each response ℓ, take the median half-life across ALL shocks i (using the envelope metric)
  hl_l <- rep(NA_real_, k)
  for (ell in seq_len(k)) {
    # median across shocks, then median across responses in the group
    hls_i <- sapply(seq_len(k), function(i) half_life_peak_env_frac(GI[ell, i, ], frac = 0.6))
    hl_l[ell] <- stats::median(hls_i, na.rm = TRUE)
  }
  
  tibble::tibble(proxy   = proxy_label,
                 response = vars,
                 group    = as.character(grp),
                 regime   = "H",
                 half_life = hl_l) |>
    dplyr::group_by(proxy, group, regime) |>
    dplyr::summarise(half_life = stats::median(half_life, na.rm = TRUE), .groups = "drop")
}

HL <- dplyr::bind_rows(
  half_by(vol, "vxo_vix"),
  half_by(jln, "jln")
)
readr::write_csv(HL, fs::path(OUT_DIR, "half_life_summary.csv"))

# ---------- Persistence index (high state; median over shocks then responses) ----------
PERS <- dplyr::bind_rows(
  persistence_by(vol, "vxo_vix", Hpi = 24L),
  persistence_by(jln, "jln",     Hpi = 24L)
)
readr::write_csv(PERS, fs::path(OUT_DIR, "persistence_index_summary.csv"))

message("Wrote: ",
        fs::path(OUT_DIR, "regime_flags.csv"), " | ",
        fs::path(OUT_DIR, "fevd_regime_shares.csv"), " | ",
        fs::path(OUT_DIR, "half_life_summary.csv"), " | ",
        fs::path(OUT_DIR, "persistence_index_summary.csv"))