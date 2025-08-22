# functions/scenarios/simulate/simulate_structural_shock_fast.R
# FAST simulator for GIRF: deterministic IRF convolution, returns list(Y=...)
# Expected by compute_tvar_girf(): simulate_structural_shock_fast(model, H, B, A, eps0, draws, r0, hysteresis, regime)
simulate_structural_shock_fast <- function(model, H, B, A = NULL, eps0 = NULL,
                                           draws = NULL, r0 = NULL, hysteresis = NULL,
                                           regime = "combined") {
  `%||%` <- function(x,y) if (is.null(x)) y else x
  stopifnot(is.list(model), !is.null(model$variables))
  
  vars <- as.character(model$variables); k <- length(vars)
  H <- as.integer(H); if (is.na(H) || H < 1L) H <- 12L
  B <- as.integer(B); if (is.na(B) || B < 1L) B <- 100L
  
  # ---------- generic zero cube ----------
  zero_cube <- function(H) array(0, dim = c(k, k, H),
                                 dimnames = list(resp = vars, imp = vars, h = seq_len(H)))
  
  # ---------- robust: try build_irf_cube_any() if available ----------
  build_irf_robust <- function(obj, H) {
    if (exists("build_irf_cube_any", mode = "function")) {
      # external robust parser you added earlier
      return(build_irf_cube_any(obj, vars = vars, H = H))
    }
    # internal conservative builder (kept as a fallback)
    IRF <- zero_cube(H)
    if (is.list(obj) && !is.array(obj)) {
      # resp -> imp
      if (length(names(obj)) && all(names(obj) %in% vars)) {
        for (resp in intersect(names(obj), vars)) {
          sub <- obj[[resp]]
          if (is.list(sub)) for (imp in intersect(names(sub), vars)) {
            v <- sub[[imp]]; kvec <- NULL
            if (is.numeric(v)) kvec <- as.numeric(v)
            if (is.null(kvec) && is.list(v)) {
              for (nm in c("50%","p50","median","mean","point","central")) {
                if (!is.null(v[[nm]]) && is.numeric(v[[nm]])) { kvec <- as.numeric(v[[nm]]); break }
              }
            }
            if (!is.null(kvec) && length(kvec)) {
              L <- min(H, length(kvec)); IRF[resp, imp, seq_len(L)] <- kvec[seq_len(L)]
            }
          }
        }
      }
      # 4D summary [stat, h, resp, imp]
      if (all(IRF == 0) && !is.null(obj$summary) && is.array(obj$summary) && length(dim(obj$summary)) == 4) {
        dn <- dimnames(obj$summary); stat_names <- tolower(dn[[1]] %||% character())
        pref <- c("50%","p50","median","mean","point","central")
        pick <- which(stat_names %in% tolower(pref)); sidx <- if (length(pick)) pick[1] else 1
        for (resp in vars) for (imp in vars) {
          vec <- obj$summary[sidx, , resp, imp, drop = TRUE]
          if (is.numeric(vec) && length(vec)) {
            L <- min(H, length(vec)); IRF[resp, imp, seq_len(L)] <- as.numeric(vec[seq_len(L)])
          }
        }
      }
    }
    # 3D array in unknown order → align
    if (is.array(obj) && length(dim(obj)) == 3 && all(IRF == 0)) {
      dn <- dimnames(obj)
      score <- vapply(dn, function(n) sum((n %||% character()) %in% vars), integer(1))
      var_dims <- which(score > 0)
      if (length(var_dims) >= 2) {
        resp_dim <- var_dims[1]; imp_dim <- var_dims[2]; h_dim <- setdiff(1:3, c(resp_dim, imp_dim))[1]
        x <- aperm(obj, c(resp_dim, imp_dim, h_dim))
        x <- x[vars, vars, , drop = FALSE]
        L <- min(H, dim(x)[3]); IRF <- zero_cube(H); IRF[, , seq_len(L)] <- x[, , seq_len(L), drop = FALSE]
      }
    }
    IRF
  }
  
  # ---------- assemble IRF for requested regime ----------
  IRF <- zero_cube(H)
  root <- model$irf
  
  # 1) Combined: average low/high if both exist
  if (identical(regime, "combined") && is.list(root) && !is.null(root$low) && !is.null(root$high)) {
    IRF_low  <- build_irf_robust(root$low,  H)
    IRF_high <- build_irf_robust(root$high, H)
    if (any(IRF_low != 0) || any(IRF_high != 0)) {
      IRF <- (IRF_low + IRF_high) / 2
      message("[sim-fast] IRF built via robust parser (combined=avg low/high).")
    }
  }
  
  # 2) Single regime object if still zero
  if (all(IRF == 0)) {
    reg_obj <- root
    if (is.list(root$regimes) && !is.null(root$regimes[[regime]])) reg_obj <- root$regimes[[regime]]
    if (!is.null(root[[regime]])) reg_obj <- root[[regime]]
    IRF_try <- build_irf_robust(reg_obj, H)
    if (any(IRF_try != 0)) {
      IRF <- IRF_try
      message("[sim-fast] IRF built via robust parser (regime='", regime, "').")
    }
  }
  
  # 3) Fallback: prepare_irf_tensor() if still zero
  if (all(IRF == 0) && exists("prepare_irf_tensor", mode = "function")) {
    ten <- try(prepare_irf_tensor(
      model          = model,
      regime         = regime,
      responses_keep = vars,
      impulses_keep  = vars,
      H              = H,
      verbose        = FALSE
    ), silent = TRUE)
    if (!inherits(ten, "try-error") && !is.null(ten$IRF) && is.array(ten$IRF) && any(ten$IRF != 0)) {
      IRF <- ten$IRF
      message("[sim-fast] IRF built via prepare_irf_tensor().")
    }
  }
  
  if (all(IRF == 0)) {
    nms <- if (!is.null(model$irf)) names(model$irf) else "(none)"
    stop(paste0(
      "[simulate_structural_shock_fast] IRF cube is all zeros.\n",
      "- regime requested: '", regime, "'\n",
      "- top-level names(model$irf): ", paste(nms, collapse = ", "), "\n",
      "- hint: re-run 05 to compute/save IRFs or ensure robust parser is sourced."
    ))
  }
  
  # -------- Structural impact vector --------
  if (is.null(eps0)) eps0 <- rep(0, k)
  if (is.null(names(eps0))) names(eps0) <- vars
  
  # -------- Deterministic convolution into Y [B x H x k] --------
  Y <- array(0, dim = c(B, H, k), dimnames = list(draw = NULL, h = seq_len(H), var = vars))
  nz_imp <- names(eps0)[abs(as.numeric(eps0)) > 0]
  if (length(nz_imp)) {
    for (imp in nz_imp) {
      s <- as.numeric(eps0[[imp]])
      Kslice <- IRF[, imp, , drop = TRUE]  # ✅ 2D [k x H]
      for (resp_idx in seq_len(k)) {
        Y[, , resp_idx] <- sweep(Y[, , resp_idx, drop = FALSE], 2,
                                 s * Kslice[resp_idx, ], `+`)
      }
    }
  }
  
  list(Y = Y, regime = regime, IRF = IRF)
}