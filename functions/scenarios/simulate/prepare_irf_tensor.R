# functions/scenarios/simulate/prepare_irf_tensor.R
# Robust tensor builder for multiple IRF layouts:
# - model$irf$regimes[[regime]]$summary  : 4D [stat, h, resp, imp] (dimnames)
# - model$irf$regimes[[regime]]$draws    : 4D [h, resp, imp, draw] (dimnames)
# - model$irf$regimes[[regime]]          : 3D array [h, resp, imp]
# - model$irf[[regime]] (3D) or response->impulse nested lists
# Picks requested regime with fallbacks and returns a compact IRF tensor.

prepare_irf_tensor <- function(model, regime = "combined",
                               responses_keep = NULL, impulses_keep = NULL,
                               H = 12L, verbose = TRUE) {
  stopifnot(is.list(model), !is.null(model$variables), !is.null(model$irf))
  `%||%` <- function(x, y) if (is.null(x)) y else x
  vars_all <- as.character(model$variables)
  irf0     <- model$irf
  
  # ---------------- helpers ----------------
  strip_meta <- function(x) {
    if (!is.list(x)) return(x)
    drops <- intersect(names(x) %||% character(),
                       c("settings","var_names","created_at","metadata","meta","info"))
    if (length(drops)) x[drops] <- NULL
    x
  }
  has_any_varname <- function(nm) length(intersect(nm %||% character(), vars_all)) > 0
  
  # picks a usable regime object (array or list)
  pick_regime_obj <- function(irf_root, requested) {
    # A) regimes layer
    if (is.list(irf_root$regimes)) {
      reg_layer <- strip_meta(irf_root$regimes)
      prefs <- unique(c(requested, "combined", "low", "high", names(reg_layer)))
      for (rk in prefs) {
        if (is.null(rk)) next
        cand <- reg_layer[[rk]]
        if (!is.null(cand)) return(list(obj = cand, used = rk))
      }
    }
    # B) top-level regimes
    top <- strip_meta(irf_root)
    if (length(names(top))) {
      prefs <- unique(c(requested, "combined", "low", "high", names(top)))
      for (rk in prefs) {
        if (is.null(rk)) next
        cand <- top[[rk]]
        if (!is.null(cand)) return(list(obj = cand, used = rk))
      }
    }
    # C) already response->impulse map or array
    list(obj = strip_meta(irf_root), used = "combined")
  }
  
  dimn <- function(arr) dimnames(arr)
  # Identify indices of (resp, imp, horizon, draw, stat) in arrays by dimnames
  find_dims_3 <- function(arr) {
    dn <- dimn(arr)
    idx_resp <- which.max(vapply(dn, has_any_varname, 0L))
    rest <- setdiff(1:3, idx_resp)
    # second var-dim among remaining (if any)
    scores <- vapply(dn[rest], has_any_varname, 0L)
    idx_imp <- rest[which.max(scores)]
    if (length(idx_imp) == 0) idx_imp <- rest[1]
    idx_h   <- setdiff(1:3, c(idx_resp, idx_imp))[1]
    list(h = idx_h, resp = idx_resp, imp = idx_imp)
  }
  find_dims_4_draws <- function(arr) {
    dn <- dimn(arr)
    # two dims should match variable names
    scores <- vapply(dn, has_any_varname, 0L)
    var_dims <- which(scores > 0)
    if (length(var_dims) < 2) {
      # fallback: assume dims 2 and 3 are vars
      var_dims <- c(2,3)[1:min(2, length(dim(arr)))]
    }
    idx_resp <- var_dims[1]; idx_imp <- var_dims[2]
    rest <- setdiff(1:4, c(idx_resp, idx_imp))
    # horizon likely the one whose names look like "1","2",... or largest <= 64
    looks_h <- function(nm, len) {
      if (len <= 64) return(TRUE)
      if (is.null(nm)) return(FALSE)
      all(grepl("^[0-9]+$", nm))
    }
    cand_h <- rest[which.max(vapply(rest, function(i) looks_h(dn[[i]], dim(arr)[i]), 0L))]
    if (length(cand_h) == 0) cand_h <- rest[1]
    idx_h <- cand_h
    idx_draw <- setdiff(rest, idx_h)[1]
    list(h = idx_h, resp = idx_resp, imp = idx_imp, draw = idx_draw)
  }
  find_dims_4_summary <- function(arr) {
    dn <- dimnames(arr)
    
    is_stat_dim <- function(nm_vec) {
      nm_vec <- tolower(nm_vec %||% character())
      any(nm_vec %in% c("median","mean","p50","point","central","lower","upper","p05","p95")) ||
        any(grepl("^\\d+%$", nm_vec))  # <-- handles "5%", "50%", "95%"
    }
    
    cand <- which(vapply(dn, is_stat_dim, logical(1)))
    stat_dim <- if (length(cand)) cand[1] else 1
    
    # two var-name-rich dims (response & impulse)
    var_scores <- vapply(dn, function(n) sum((n %||% character()) %in% vars_all), integer(1))
    var_dims <- setdiff(which(var_scores > 0), stat_dim)
    if (length(var_dims) < 2) {
      rest <- setdiff(seq_along(dn), stat_dim)
      var_dims <- tail(rest, 2)
    }
    idx_resp <- var_dims[1]
    idx_imp  <- var_dims[2]
    
    # remaining is horizon
    idx_h <- setdiff(seq_along(dn), c(stat_dim, idx_resp, idx_imp))[1]
    list(stat = stat_dim, h = idx_h, resp = idx_resp, imp = idx_imp)
  }
  
  aperm_get <- function(arr, order) aperm(arr, order, resize = FALSE)
  
  slice_3d <- function(arr, resp, imp, H, dims) {
    dn <- dimn(arr)
    pr <- match(resp, dn[[dims$resp]]); pi <- match(imp, dn[[dims$imp]])
    if (is.na(pr) || is.na(pi)) return(numeric(0))
    ord <- c(dims$h, dims$resp, dims$imp)
    x <- aperm_get(arr, ord)
    out <- x[, pr, pi]
    out <- unname(as.numeric(out))
    if (length(out) > H) out <- out[seq_len(H)]
    out
  }
  slice_4d_draws <- function(arr, resp, imp, H, dims) {
    dn <- dimn(arr)
    pr <- match(resp, dn[[dims$resp]]); pi <- match(imp, dn[[dims$imp]])
    if (is.na(pr) || is.na(pi)) return(numeric(0))
    ord <- c(dims$h, dims$resp, dims$imp, dims$draw)  # -> (h, resp, imp, draw)
    x <- aperm_get(arr, ord)
    mat <- x[, pr, pi, drop = FALSE]
    # average across draws (last dim), avoid NA
    vec <- rowMeans(drop(mat), na.rm = TRUE)
    if (length(vec) > H) vec <- vec[seq_len(H)]
    unname(as.numeric(vec))
  }
  slice_4d_summary <- function(arr, resp, imp, H, dims) {
    dn <- dimnames(arr)
    
    # choose a “median-like” stat: prefer 50%, then p50/median/mean
    stat_names <- tolower(dn[[dims$stat]] %||% character())
    
    # 1) direct hits
    pref <- c("50%","p50","median","mean","point","central")
    hit  <- match(pref, stat_names, nomatch = 0)
    if (any(hit > 0)) {
      lab <- dn[[dims$stat]][ hit[which(hit > 0)[1]] ]
    } else {
      # 2) pick % label closest to 50
      pct <- suppressWarnings(as.numeric(gsub("%","", stat_names)))
      if (any(!is.na(pct))) {
        lab <- dn[[dims$stat]][ which.min(abs(pct - 50)) ]
      } else {
        lab <- dn[[dims$stat]][1]
      }
    }
    
    # IMPORTANT: index *by names* in the original order [stat, h, response, impulse]
    k <- arr[ lab, , resp, imp, drop = TRUE ]  # vector over h
    k <- unname(as.numeric(k))
    
    # keep the first H entries (this includes h=0 as the first element)
    if (length(k) > H) k <- k[seq_len(H)]
    k
  }
  
  get_kernel <- function(reg_obj, resp, imp, H) {
    # 4D SUMMARY
    if (is.list(reg_obj) && !is.null(reg_obj$summary) &&
        is.array(reg_obj$summary) && length(dim(reg_obj$summary)) == 4) {
      dims <- find_dims_4_summary(reg_obj$summary)
      k <- slice_4d_summary(reg_obj$summary, resp, imp, H, dims)
      if (length(k)) return(k)
    }
    # 4D DRAWS
    if (is.list(reg_obj) && !is.null(reg_obj$draws) &&
        is.array(reg_obj$draws) && length(dim(reg_obj$draws)) == 4) {
      dims <- find_dims_4_draws(reg_obj$draws)
      k <- slice_4d_draws(reg_obj$draws, resp, imp, H, dims)
      if (length(k)) return(k)
    }
    # 3D array
    if (is.array(reg_obj) && length(dim(reg_obj)) == 3) {
      dims <- find_dims_3(reg_obj)
      k <- slice_3d(reg_obj, resp, imp, H, dims)
      if (length(k)) return(k)
    }
    # nested lists (resp->imp or imp->resp), try to pull numeric
    if (is.list(reg_obj)) {
      num_from <- function(x) {
        if (is.numeric(x)) return(unname(as.numeric(x)))
        if (is.list(x)) {
          for (nm in c("median","mean","irf","point","p50","P50")) {
            if (!is.null(x[[nm]]) && is.numeric(x[[nm]])) return(unname(as.numeric(x[[nm]])))
          }
          flat <- unlist(x, recursive = TRUE, use.names = FALSE)
          if (is.numeric(flat) && length(flat)) return(unname(as.numeric(flat)))
        }
        NULL
      }
      if (!is.null(reg_obj[[resp]])) {
        k <- num_from(reg_obj[[resp]][[imp]]); if (!is.null(k)) return(head(k, H))
      }
      if (!is.null(reg_obj[[imp]])) {
        k <- num_from(reg_obj[[imp]][[resp]]); if (!is.null(k)) return(head(k, H))
      }
    }
    numeric(0)
  }
  
  # --------------- pick regime ---------------
  pick_for_regime <- function(irf_root, rk) {
    # returns object or NULL
    if (is.list(irf_root$regimes) && !is.null(irf_root$regimes[[rk]])) return(irf_root$regimes[[rk]])
    if (!is.null(irf_root[[rk]])) return(irf_root[[rk]])
    NULL
  }
  
  if (identical(regime, "combined")) {
    low_obj  <- pick_for_regime(irf0, "low")
    high_obj <- pick_for_regime(irf0, "high")
    if (!is.null(low_obj) && !is.null(high_obj)) {
      get_cube <- function(ro) {
        IRF_tmp <- array(0, dim = c(length(vars_all), length(vars_all), H),
                         dimnames = list(resp = vars_all, imp = vars_all, h = seq_len(H)))
        for (resp in vars_all) for (imp in vars_all) {
          k <- get_kernel(ro, resp, imp, H)
          if (length(k)) IRF_tmp[resp, imp, seq_len(min(H, length(k)))] <- k[seq_len(min(H, length(k)))]
        }
        IRF_tmp
      }
      IRF_low  <- get_cube(low_obj)
      IRF_high <- get_cube(high_obj)
      IRF_comb <- (IRF_low + IRF_high) / 2
      reg_obj <- IRF_comb
      used_regime <- "combined"
    } else {
      # fall back to whichever exists
      reg_obj <- low_obj %||% high_obj %||% strip_meta(irf0)
      used_regime <- if (!is.null(low_obj)) "low" else if (!is.null(high_obj)) "high" else "combined"
    }
  } else {
    picked <- pick_regime_obj(irf0, regime)
    reg_obj <- picked$obj
    used_regime <- picked$used
  }
  
  # defaults (fast)
  if (is.null(responses_keep)) responses_keep <- head(vars_all, min(3L, length(vars_all)))
  if (is.null(impulses_keep))  impulses_keep  <- head(vars_all, min(2L, length(vars_all)))
  responses_keep <- intersect(responses_keep, vars_all)
  impulses_keep  <- intersect(impulses_keep,  vars_all)
  if (!length(responses_keep) || !length(impulses_keep))
    stop("responses_keep or impulses_keep empty after intersect with model variables: ",
         paste(vars_all, collapse = ", "))
  
  H <- as.integer(H)
  IRF <- array(0, dim = c(length(responses_keep), length(impulses_keep), H),
               dimnames = list(resp = responses_keep, imp = impulses_keep, h = seq_len(H)))
  
  for (resp in responses_keep) {
    for (imp in impulses_keep) {
      k <- get_kernel(reg_obj, resp, imp, H)
      if (length(k)) {
        L <- min(H, length(k))
        IRF[resp, imp, seq_len(L)] <- k[seq_len(L)]
      }
    }
  }
  
  # ... inside prepare_irf_tensor(), after your normal assembly logic but before stopping:
  
  # Fallback: robust parser for per-impulse lists or unusual shapes
  if (!exists("build_irf_cube_any")) {
    source(here::here("functions","tvar","irf","build_irf_cube_any.R"))
  }
  IRF_fb <- NULL
  try({
    if (!is.null(model$irf)) {
      if (!is.null(model$irf[[regime]])) {
        IRF_fb <- build_irf_cube_any(model$irf[[regime]], vars = responses_keep, H = H)
      } else if (identical(tolower(regime), "combined") && !is.null(model$irf$low) && !is.null(model$irf$high)) {
        IRF_low  <- build_irf_cube_any(model$irf$low,  vars = responses_keep, H = H)
        IRF_high <- build_irf_cube_any(model$irf$high, vars = responses_keep, H = H)
        IRF_fb   <- (IRF_low + IRF_high) / 2
      }
    }
  }, silent = TRUE)
  #-------------------------------------------
  if (is.array(IRF_fb) && any(IRF_fb != 0, na.rm = TRUE)) {
    IRF <- IRF_fb
    # optionally attempt bands if present
    IRF_lo <- IRF_hi <- NULL
    if (!is.null(model$irf$envelope)) {
      IRF_lo <- build_irf_cube_any(model$irf$envelope$lo[[regime]], vars = responses_keep, H = H)
      IRF_hi <- build_irf_cube_any(model$irf$envelope$hi[[regime]], vars = responses_keep, H = H)
    }
    return(list(IRF = IRF, IRF_lo = IRF_lo, IRF_hi = IRF_hi,
                H = H, regime = regime,
                responses = dimnames(IRF)$resp, impulses = dimnames(IRF)$imp))
  }
  
  stop("[prepare_irf_tensor] All kernels are zero for regime '", regime,
       "'. Recompute IRFs in 05 (check identification & H, and ensure names match model$variables).")
  
  
  if (all(IRF == 0)) {
    stop("[prepare_irf_tensor] All kernels are zero for regime '", used_regime,
         "'. Recompute IRFs in 05 (check identification & H, and ensure names match model$variables).")
  }
   #-------------------------------------------
  if (verbose) message("[07] IRF tensor built using regime '", used_regime,
                       "' (H=", H, ", resp=", length(responses_keep),
                       ", imp=", length(impulses_keep), ").")
  
  list(IRF = IRF,
       responses = responses_keep,
       impulses  = impulses_keep,
       H         = H,
       regime    = used_regime)
}