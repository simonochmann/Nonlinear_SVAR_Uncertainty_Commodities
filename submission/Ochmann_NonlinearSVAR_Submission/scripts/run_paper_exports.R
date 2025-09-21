# run_paper_exports.R — regenerate paper/artifacts/* from data/*

suppressPackageStartupMessages({
  library(fs); library(here); library(readr)
  library(dplyr); library(tidyr)
  library(ggplot2); library(scales); library(vars); library(stats); library(lubridate)
})

options(dplyr.summarise.inform = FALSE)
if ("package:plyr" %in% search()) detach("package:plyr", unload = TRUE, force = TRUE)
set.seed(123)

# -------------------- Tunables --------------------
TRIM       <- 0.15     # threshold grid trimming
M          <- 3L       # MA smoother length for driver
DLY        <- 1L       # lag for driver
HMAX       <- 36L      # max horizon
LAGS       <- 2:5      # modest to avoid singularities
GRID_N     <- 61L      # threshold grid resolution
MIN_OBS    <- 60L      # min finite obs per series before selection
MIN_ROWS   <- 120L     # min complete rows after pruning
MIN_COLS   <- 6L       # min variables for VAR
MAX_SERIES <- 18L      # cap VAR dimension
PLOT_WINDOW <- "Extension (1986–2025)"  # which window to use for delta_lines.png
# --------------------------------------------------

# ---- Paths ----
paperdir <- here::here("submission","paper")
art_dir  <- fs::path(paperdir,"artifacts")
tab_dir  <- fs::path(art_dir, "tables")
fig_dir  <- fs::path(art_dir, "figures","decomposition")
dat_dir  <- here::here("submission","data")

fs::dir_create(tab_dir, recurse = TRUE)
fs::dir_create(fig_dir, recurse = TRUE)

# ---- Load inputs ----
path_ret <- fs::path(dat_dir, "returns_wide.csv")
path_prx <- fs::path(dat_dir, "proxies.csv")
path_grp <- fs::path(dat_dir, "series_groups.csv")
stopifnot(fs::file_exists(path_ret), fs::file_exists(path_prx), fs::file_exists(path_grp))

RET <- readr::read_csv(path_ret, show_col_types = FALSE)
PRX <- readr::read_csv(path_prx, show_col_types = FALSE)
GRP <- readr::read_csv(path_grp, show_col_types = FALSE)

names(RET)[1] <- "date"; RET$date <- as.Date(RET$date)
names(PRX)[1] <- "date"; PRX$date <- as.Date(PRX$date)

# Align by month
RET <- RET %>% mutate(date = floor_date(date, "month"))
PRX <- PRX %>% mutate(date = floor_date(date, "month"))

DF  <- RET %>% inner_join(PRX, by = "date") %>% arrange(date)
if (nrow(DF) == 0L) stop("RET × PRX monthly join produced 0 rows.")

message("Joined rows: ", nrow(DF), "  |  Date range: ",
        as.character(min(DF$date)), " .. ", as.character(max(DF$date)))

# ---- Drivers (volatility + JLN) ----
col_has <- function(nm) nm %in% names(DF)

vol_raw <- if (col_has("vol_proxy")) DF$vol_proxy else rep(NA_real_, nrow(DF))
if (col_has("VIXCLS")) vol_raw <- ifelse(is.na(vol_raw), DF$VIXCLS, vol_raw)
if (col_has("VXOCLS")) vol_raw <- ifelse(is.na(vol_raw), DF$VXOCLS, vol_raw)

jln_raw <- if (col_has("jln_proxy")) DF$jln_proxy else rep(NA_real_, nrow(DF))
if (col_has("JLNUM1M")) jln_raw <- ifelse(is.na(jln_raw), DF$JLNUM1M, jln_raw)

if (sum(is.finite(vol_raw)) < 36L) stop("Volatility proxy missing: need vol_proxy or VIXCLS/VXOCLS in proxies.csv.")
if (sum(is.finite(jln_raw)) < 36L) stop("JLN proxy missing: need jln_proxy or JLNUM1M in proxies.csv.")

std <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
sm3 <- function(x) stats::filter(x, rep(1/M, M), sides = 1)

prep_driver <- function(raw, label) {
  x <- std(raw)
  xs <- as.numeric(sm3(x))
  if (DLY > 0) xs <- dplyr::lag(xs, DLY)
  if (sum(is.finite(xs)) == 0L) {
    message("NOTE: driver '", label, "' fully NA after MA(3)+lag(", DLY, "); falling back to std+lag.")
    xs <- std(raw)
    if (DLY > 0) xs <- dplyr::lag(xs, DLY)
  }
  xs
}
DF$x_vol <- prep_driver(vol_raw, "vol")
DF$x_jln <- prep_driver(jln_raw, "jln")

# ---- Series mapping & pool ----
GRP <- GRP %>% filter(series %in% names(RET))
stopifnot(nrow(GRP) >= MIN_COLS)
map_group <- setNames(GRP$group, GRP$series)
keep_cols <- setdiff(intersect(names(RET), names(map_group)), "date")
if (length(keep_cols) < MIN_COLS) stop("Not enough mapped series; need >=", MIN_COLS, ", got ", length(keep_cols))
Y_full <- DF[, keep_cols, drop = FALSE]

# ---- Windows ----
win_rep <- DF$date >= as.Date("1986-01-01") & DF$date <= as.Date("2015-04-30")
win_ext <- DF$date >= as.Date("1986-01-01") & DF$date <= max(DF$date, na.rm = TRUE)

message("Driver 'vol (rep)' finite in window: ", sum(is.finite(DF$x_vol[win_rep])), " of ", sum(win_rep))
message("Driver 'jln (rep)' finite in window: ", sum(is.finite(DF$x_jln[win_rep])), " of ", sum(win_rep))

# ---- Helpers: choose series & prune ----
pick_wide_Y <- function(Y, window_flag, min_obs = MIN_OBS, max_series = MAX_SERIES) {
  Yw <- Y[window_flag, , drop = FALSE]
  cc <- colSums(is.finite(as.matrix(Yw)))
  ok <- names(cc)[cc >= min_obs]
  if (length(ok) < MIN_COLS) stop("Coverage filter retained only ", length(ok), " series.")
  ord <- order(cc[ok], decreasing = TRUE)
  sel <- ok[ord][seq_len(min(max_series, length(ord)))]
  Yw[, sel, drop = FALSE]
}

# Return also the indices in DF of the kept rows
prune_to_complete <- function(Yw, xvec, window_flag, min_rows = MIN_ROWS, min_cols = MIN_COLS) {
  window_idx <- which(window_flag)
  mask_x     <- is.finite(xvec)
  Y1         <- Yw[mask_x, , drop = FALSE]
  x1         <- xvec[mask_x]
  if (nrow(Y1) == 0L) stop("All rows dropped by x NA mask.")
  cols <- colnames(Y1)
  repeat {
    rows_ok <- stats::complete.cases(Y1[, cols, drop = FALSE])
    if (sum(rows_ok) >= min_rows || length(cols) <= min_cols) break
    na_counts <- colSums(!is.finite(as.matrix(Y1[, cols, drop = FALSE])))
    dropcol <- cols[which.max(na_counts)]
    cols <- setdiff(cols, dropcol)
  }
  rows_ok <- stats::complete.cases(Y1[, cols, drop = FALSE])
  Y2      <- Y1[rows_ok, cols, drop = FALSE]
  x2      <- x1[rows_ok]
  idx_win <- window_idx[mask_x][rows_ok]
  if (ncol(Y2) < min_cols) stop("Not enough columns after pruning: ", ncol(Y2))
  if (nrow(Y2) < max(LAGS) + 10) stop("Too few complete rows after pruning: ", nrow(Y2))
  list(Y = Y2, x = x2, idx_win = idx_win)
}

grid_c <- function(x) {
  x_ok <- x[is.finite(x)]
  rng  <- stats::quantile(x_ok, probs = c(TRIM, 1-TRIM), na.rm = TRUE)
  as.numeric(seq(rng[1], rng[2], length.out = GRID_N))
}

# ---- Stability & BIC ----
spectral_radius <- function(fit) {
  r <- try(vars::roots(fit, modulus = TRUE), silent = TRUE)
  if (!inherits(r, "try-error")) {
    r <- as.numeric(r); r <- r[is.finite(r) & r > 0]
    if (length(r)) return(1 / min(r))
  }
  Ac <- try(vars::Acoef(fit), silent = TRUE)
  if (!inherits(Ac, "try-error") && length(Ac)) {
    K <- nrow(Ac[[1]]); p <- length(Ac)
    if (p == 1L) {
      return(max(Mod(eigen(Ac[[1]], only.values = TRUE)$values)))
    } else {
      Ctop <- do.call(cbind, Ac)
      Cbot <- cbind(diag(K * (p - 1)), matrix(0, nrow = K * (p - 1), ncol = K))
      Comp <- rbind(Ctop, Cbot)
      return(max(Mod(eigen(Comp, only.values = TRUE)$values)))
    }
  }
  NA_real_
}

lb_min_p <- function(resid_mat, lag = 12) {
  ps <- apply(resid_mat, 2, function(z)
    tryCatch(stats::Box.test(z, lag = lag, type = "Ljung-Box")$p.value, error = function(e) NA_real_))
  suppressWarnings(min(ps, na.rm = TRUE))
}

bic_for_split <- function(Y, sflag, p) {
  Ttot <- nrow(Y); n <- ncol(Y)
  kcount <- function(p, n) n*(n*p + 1)
  out <- list(BIC = Inf, T_L = 0L, T_H = 0L, fitL = NULL, fitH = NULL)
  TL <- sum(sflag == 0, na.rm = TRUE); TH <- sum(sflag == 1, na.rm = TRUE)
  if (TL < (n*p + 5) || TH < (n*p + 5)) return(out)
  YL <- Y[sflag == 0, , drop = FALSE]
  YH <- Y[sflag == 1, , drop = FALSE]
  fitL <- tryCatch(vars::VAR(YL, p = p, type = "const"), error = function(e) NULL)
  fitH <- tryCatch(vars::VAR(YH, p = p, type = "const"), error = function(e) NULL)
  if (is.null(fitL) || is.null(fitH)) return(out)
  eL <- stats::residuals(fitL); eH <- stats::residuals(fitH)
  if (any(!is.finite(eL)) || any(!is.finite(eH))) return(out)
  SigL <- crossprod(eL) / nrow(eL)
  SigH <- crossprod(eH) / nrow(eH)
  deL  <- tryCatch(det(SigL), error = function(e) NA_real_)
  deH  <- tryCatch(det(SigH), error = function(e) NA_real_)
  if (!is.finite(deL) || !is.finite(deH) || deL <= 0 || deH <= 0) return(out)
  ll   <- nrow(YL) * log(deL) + nrow(YH) * log(deH)
  kpar <- (kcount(p, n) + kcount(p, n))
  BIC  <- ll + kpar * log(Ttot)
  list(BIC = BIC, T_L = nrow(YL), T_H = nrow(YH), fitL = fitL, fitH = fitH)
}

pick_pd_c <- function(Y, x) {
  cs   <- grid_c(x)
  best <- list(BIC = Inf, p = NA_integer_, c = NA_real_, fitL = NULL, fitH = NULL, T_L = 0L, T_H = 0L)
  for (p in LAGS) {
    for (c in cs) {
      sflag <- as.integer(x > c)
      tmp   <- bic_for_split(Y, sflag, p)
      if (is.finite(tmp$BIC) && tmp$BIC < best$BIC) best <- c(tmp, list(p = p, c = c))
    }
  }
  best
}

# ---- Safe IRF: try vars::irf(boot=FALSE); fallback to manual VAR→VMA recursion ----
manual_irf <- function(fit, H = HMAX) {
  A <- try(vars::Acoef(fit), silent = TRUE)
  if (inherits(A, "try-error") || !length(A)) stop("Acoef(fit) failed in manual_irf().")
  K <- nrow(A[[1]]); p <- length(A)
  Theta <- vector("list", H + 1L)
  Theta[[1L]] <- diag(K)
  for (h in 2:(H + 1L)) {
    m <- matrix(0, K, K)
    for (j in 1:min(p, h - 1L)) m <- m + A[[j]] %*% Theta[[h - j]]
    Theta[[h]] <- m
  }
  sds <- sqrt(diag(fit$covres))
  cn  <- colnames(fit$y)
  out <- vector("list", K); names(out) <- cn
  for (i in seq_len(K)) {
    shock <- rep(0, K); shock[i] <- sds[i]
    M <- do.call(rbind, lapply(Theta, function(Th) as.numeric(Th %*% shock)))
    colnames(M) <- cn
    out[[i]] <- M
  }
  list(irf = out)
}

safe_irf <- function(fit, H = HMAX) {
  out <- try(vars::irf(fit,
                       impulse  = colnames(fit$y),
                       response = colnames(fit$y),
                       n.ahead  = H,
                       ortho    = FALSE,
                       boot     = FALSE),
             silent = TRUE)
  if (!inherits(out, "try-error")) return(out)
  manual_irf(fit, H = H)
}

# ---- Main worker ----
do_all_for_proxy <- function(proxy_name, xdriver, window_flag, label_window) {
  Yw <- pick_wide_Y(Y_full, window_flag, min_obs = MIN_OBS, max_series = MAX_SERIES)
  message("Using ", ncol(Yw), " series in ", label_window, " after coverage filter.")
  
  xall <- xdriver[window_flag]
  pc   <- prune_to_complete(Yw, xall, window_flag, min_rows = MIN_ROWS, min_cols = MIN_COLS)
  X    <- scale(as.matrix(pc$Y), center = TRUE, scale = TRUE)
  x    <- pc$x
  idx  <- pc$idx_win
  message("Complete block for ", label_window, ": ", nrow(X), " rows × ", ncol(X), " series.")
  
  sel   <- pick_pd_c(X, x)
  p     <- sel$p; cstar <- sel$c
  if (!is.finite(p) || !is.finite(cstar)) stop("Model selection failed for ", label_window, " / ", proxy_name)
  sflag <- as.integer(x > cstar)
  fitL  <- sel$fitL; fitH <- sel$fitH
  
  lamL <- spectral_radius(fitL); lamH <- spectral_radius(fitH)
  lbL  <- lb_min_p(stats::residuals(fitL)); lbH <- lb_min_p(stats::residuals(fitH))
  sH   <- mean(sflag == 1)
  
  row1 <- tibble::tibble(
    window = label_window,
    proxy  = if (proxy_name == "vol") "Volatility (VXO->VIX)" else "JLN (macro uncertainty)",
    p = p, d = DLY, c_star = round(cstar, 3),
    T_L = sel$T_L, T_H = sel$T_H, s_H = sH,
    lambda_max_L = lamL, lambda_max_H = lamH,
    lb_p_L = lbL, lb_p_H = lbH,
    supLM_stat = NA_real_, supLM_p = NA_real_
  )
  
  fit_lin <- vars::VAR(X, p = p, type = "const")
  irL <- safe_irf(fitL); irH <- safe_irf(fitH); irV <- safe_irf(fit_lin)
  extract_ir <- function(irf_obj) irf_obj$irf
  midL <- extract_ir(irL); midH <- extract_ir(irH); midV <- extract_ir(irV)
  
  pairs <- expand.grid(imp = names(midH), resp = colnames(midH[[1]]), stringsAsFactors = FALSE)
  half_life <- function(z) { v <- abs(z); pk <- max(v); if (pk<=0) return(NA_real_); idx <- which(v <= pk/2)[1]; if (is.na(idx)) NA_real_ else idx-1 }
  get_series <- function(M, imp, resp) as.numeric(M[[imp]][, resp])
  
  pk_ratio <- dh <- numeric(nrow(pairs)); grp_resp <- character(nrow(pairs))
  for (i in seq_len(nrow(pairs))) {
    imp  <- pairs$imp[i]; resp <- pairs$resp[i]
    grp_resp[i] <- map_group[[resp]]
    sHh  <- get_series(midH, imp, resp)
    sV   <- get_series(midV, imp, resp)
    pkH  <- max(abs(sHh)); pkV <- max(abs(sV))
    pk_ratio[i] <- if (pkV > 0) pkH / pkV else NA_real_
    dh[i]       <- half_life(sHh) - half_life(sV)
  }
  
  U <- tibble::tibble(
    window = label_window,
    proxy  = if (proxy_name == "vol") "Volatility" else "JLN",
    group  = grp_resp, peak_ratio = pk_ratio, d_half_life = dh
  ) %>% filter(is.finite(peak_ratio) | is.finite(d_half_life))
  
  # FEVD via squared generalized IRFs (state‑conditional)
  girf_to_fevd <- function(irf_list) {
    res <- vector("list", length(irf_list) * ncol(irf_list[[1]])); k <- 0L
    for (imp in names(irf_list)) {
      M <- irf_list[[imp]]
      for (resp in colnames(M)) {
        k <- k + 1L
        v <- as.numeric(M[, resp])
        res[[k]] <- tibble::tibble(impulse=imp, response=resp, horizon=seq_along(v)-1L, contrib=v^2)
      }
    }
    DFfe <- dplyr::bind_rows(res)
    DFfe <- DFfe %>% dplyr::group_by(response, horizon) %>% dplyr::mutate(share = contrib/sum(contrib)) %>% dplyr::ungroup()
    DFfe
  }
  FE_L <- girf_to_fevd(midL); FE_H <- girf_to_fevd(midH)
  agg_group <- function(FE) {
    FE %>% dplyr::mutate(group = unname(map_group[response])) %>%
      dplyr::group_by(horizon, group) %>%
      dplyr::summarise(share = mean(share, na.rm = TRUE), .groups = "drop")
  }
  FE_Lg <- agg_group(FE_L) %>% mutate(regime="L")
  FE_Hg <- agg_group(FE_H) %>% mutate(regime="H")
  FE_all <- dplyr::bind_rows(FE_Lg, FE_Hg) %>%
    dplyr::filter(horizon <= HMAX) %>%
    dplyr::mutate(proxy = if (proxy_name=="vol") "Volatility" else "JLN",
                  window = label_window) %>%
    dplyr::select(window, proxy, regime, horizon, group, share)
  
  list(sel=row1, upl=U, fevd=FE_all, sflag=sflag, idx=idx,
       p=p, fitL=fitL, fitH=fitH, fit_lin=fit_lin)
}

# === Run both proxies on both windows ===
outV_rep <- do_all_for_proxy("vol", DF$x_vol, win_rep, "Replication (1986–2015)")
outJ_rep <- do_all_for_proxy("jln", DF$x_jln, win_rep, "Replication (1986–2015)")
outV_ext <- do_all_for_proxy("vol", DF$x_vol, win_ext, "Extension (1986–2025)")
outJ_ext <- do_all_for_proxy("jln", DF$x_jln, win_ext, "Extension (1986–2025)")

SEL  <- bind_rows(outV_rep$sel,  outJ_rep$sel,  outV_ext$sel,  outJ_ext$sel)
UPL  <- bind_rows(outV_rep$upl,  outJ_rep$upl,  outV_ext$upl,  outJ_ext$upl)
FEVD <- bind_rows(outV_rep$fevd, outJ_rep$fevd, outV_ext$fevd, outJ_ext$fevd)

# === Regime flags aligned to kept rows (Extension window only, for shares plot) ===
mk_flags <- function(idx_fullDF, sflag_vec, proxy_label, win_label) {
  tibble::tibble(
    date  = DF$date[idx_fullDF],
    proxy = proxy_label,
    high  = as.integer(sflag_vec),
    window= win_label
  )
}
REG <- dplyr::bind_rows(
  mk_flags(outV_ext$idx, outV_ext$sflag, "Volatility", "Extension (1986–2025)"),
  mk_flags(outJ_ext$idx, outJ_ext$sflag, "JLN",        "Extension (1986–2025)")
)

# === Half-life & persistence (H=24) from high-regime fits (Extension window) ===
Hsel <- 24L
HL_sum <- function(irf_obj, proxy_label, group_map, regime_label) {
  ir <- irf_obj$irf; res <- list(); k <- 0L
  for (imp in names(ir)) {
    M <- ir[[imp]]
    for (resp in colnames(M)) {
      v <- as.numeric(M[1:(Hsel+1), resp])
      group <- unname(group_map[[resp]]); pk <- max(abs(v)); hl <- NA_real_
      if (pk > 0) { idx <- which(abs(v) <= pk/2)[1]; if (!is.na(idx)) hl <- idx-1 }
      PI <- sum(abs(v), na.rm = TRUE)
      k <- k + 1L
      res[[k]] <- tibble::tibble(proxy=proxy_label, group=group, regime=regime_label,
                                 response=resp, half_life=hl, H=Hsel, PI=PI)
    }
  }
  dplyr::bind_rows(res)
}
HLJ_H <- HL_sum(safe_irf(outJ_ext$fitH, H = Hsel), "JLN",        map_group, "H")
HLV_H <- HL_sum(safe_irf(outV_ext$fitH, H = Hsel), "Volatility", map_group, "H")
HL_SUM <- dplyr::bind_rows(HLV_H, HLJ_H)

# === Export artifacts ===
readr::write_csv(SEL,  fs::path(tab_dir, "model_selection_summary.csv"))
readr::write_csv(UPL,  fs::path(tab_dir, "linear_vs_tvar_uplifts.csv"))
readr::write_csv(FEVD, fs::path(tab_dir, "fevd_regime_shares.csv"))
readr::write_csv(REG,  fs::path(tab_dir, "regime_flags.csv"))

HL_SUM_out1 <- HL_SUM[, c("proxy","group","regime","half_life","H"), drop = FALSE]
HL_SUM_out2 <- HL_SUM[, c("proxy","group","regime","H","PI"), drop = FALSE]
readr::write_csv(HL_SUM_out1, fs::path(tab_dir, "half_life_summary.csv"))
readr::write_csv(HL_SUM_out2, fs::path(tab_dir, "persistence_index_summary.csv"))

# === ΔΔ–GFEVD figure (Extension window only) ===
make_dd_plot <- function(FEVD, which_window = PLOT_WINDOW) {
  FE <- FEVD %>%
    dplyr::filter(window == which_window) %>%
    dplyr::group_by(window, proxy, regime, horizon, group) %>%
    dplyr::summarise(share = mean(share, na.rm = TRUE), .groups = "drop")
  pickH <- function(FE, H) {
    hs <- sort(unique(FE$horizon))
    FE %>% dplyr::filter(horizon == hs[which.min(abs(hs - H))])
  }
  dd_for <- function(H) {
    W <- FE %>% pickH(H) %>%
      tidyr::pivot_wider(names_from = c(proxy, regime), values_from = share,
                         values_fn = mean)  # ensure numeric, no list-cols
    for (nm in c("Volatility_H","Volatility_L","JLN_H","JLN_L"))
      if (!nm %in% names(W)) W[[nm]] <- NA_real_
    W$dd <- (W$Volatility_H - W$Volatility_L) - (W$JLN_H - W$JLN_L)
    W$H  <- paste0("H=", H)
    W[, c("group","dd","H"), drop = FALSE]
  }
  DD <- dplyr::bind_rows(dd_for(12), dd_for(24))
  ggplot2::ggplot(DD, ggplot2::aes(x = group, y = dd)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = .3, colour = "grey60") +
    ggplot2::geom_col(width = .6) +
    ggplot2::facet_wrap(~H, nrow = 1) +
    ggplot2::scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    ggplot2::labs(x = NULL, y = "ΔΔ–GFEVD (share points)",
                  caption = "Notes: ΔΔ ≡ (Volatility HL − JLN HL) at each H.\nSource: own calculations based on World Bank Pink Sheet; VIX/VXO and JLN (FRED).") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   plot.caption = ggplot2::element_text(hjust = 0, size = 8))
}
g <- make_dd_plot(FEVD, which_window = PLOT_WINDOW)
ggsave(filename = fs::path(fig_dir, "delta_lines.png"), plot = g, width = 7, height = 3.4, dpi = 300)

message("✅ Artifacts written to: ", art_dir)