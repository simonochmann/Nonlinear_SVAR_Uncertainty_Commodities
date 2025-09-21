# scripts/07_shock_scenario_simulations.R
# Orchestrates scenario simulations using precomputed TVAR IRFs and/or GIRFs.

suppressPackageStartupMessages({
  library(here); library(fs); library(yaml); library(readr)
  library(dplyr); library(purrr); library(glue); library(tibble); library(digest); library(jsonlite)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# Runtime switches 
FAST_MODE      <- FALSE        
MAX_SCENARIOS  <- 1L          
HARD_H         <- 8L          
MAKE_PLOTS     <- TRUE       
COMPUTE_BANDS  <- TRUE       

# Setup & config
setup_path <- here::here("scripts", "setup.R")
if (fs::file_exists(setup_path)) source(setup_path)

cfg <- yaml::read_yaml(here::here("config/paths.yml"))

scen_dir <- cfg$scenarios$dir     %||% here::here("config","scenarios")
out_dir  <- cfg$scenarios$out_dir %||% here::here("data","scenarios")
fig_dir  <- cfg$scenarios$fig_dir %||% here::here("figures","scenarios")
log_dir  <- cfg$scenarios$log_dir %||% here::here("logs","scenarios")
fs::dir_create(c(out_dir, fig_dir, log_dir))

SEED_MASTER <- as.integer(cfg$seeds$master %||% 20240817)
set.seed(SEED_MASTER)

# Sources 
source(here::here("functions/tvar/irf/compute_tvar_girf.R"))

source(here::here("functions/scenarios/config/load_scenario_yaml.R"))
source(here::here("functions/scenarios/config/validate_scenario_spec.R"))

source(here::here("functions/scenarios/simulate/simulate_structural_shock_fast.R"))
source(here::here("functions/scenarios/simulate/prepare_irf_tensor.R"))

safe_source <- function(path) if (fs::file_exists(path)) source(path)
safe_source(here::here("functions","tvar","identify","build_contemporaneous_A.R"))
safe_source(here::here("functions","tvar","identify","canonicalize_ordering.R"))
safe_source(here::here("functions","tvar","utils","build_shock_vector.R"))
safe_source(here::here("functions","tvar","utils","scale_shock_by_sigma.R"))
safe_source(here::here("functions","tvar","utils","get_resid_cov.R"))
safe_source(here::here("functions","tvar","sim","generate_counterfactual_baseline.R"))
safe_source(here::here("functions","tvar","sim","choose_initial_regimes.R"))
safe_source(here::here("functions","tvar","stats","attach_confidence_bands.R"))

if (!FAST_MODE && MAKE_PLOTS) {
  safe_source(here::here("functions/scenarios/diagnostics/plot_scenario_paths.R"))
  safe_source(here::here("functions/scenarios/diagnostics/plot_counterfactual_vs_baseline.R"))
  safe_source(here::here("functions/scenarios/diagnostics/plot_scenario_fan.R"))
  safe_source(here::here("functions/scenarios/diagnostics/plot_irf_envelope_vs_scenario.R"))
}

# Model resolver
get_active_uncertainties <- function(cfg) {
  act <- cfg$uncertainty$active %||% "vix"
  if (identical(tolower(act), "all")) return(names(cfg$uncertainty$sources))
  if (is.character(act)) return(as.character(unlist(act)))
  stop("Unrecognized cfg$uncertainty$active")
}

# Prefer analysis_ready model, fallback to latest if needed
get_active_model <- function(active) {
  base <- cfg$models$dir %||% here::here("models","tvar")
  active <- tolower(active)
  cands <- c(
    fs::path(base, sprintf("%s_tvar_model_analysis_ready_latest.rds", active)),
    fs::path(base, sprintf("%s_tvar_model_latest.rds",               active))
  )
  mp <- cands[fs::file_exists(cands)][1]
  if (is.na(mp)) stop("[07] No model found for '", active, "' under ", base)
  message("[07] Using model: ", fs::path_rel(mp, start = here::here()))
  mdl <- readr::read_rds(mp)
  if (is.null(mdl$variables) || is.null(mdl$irf)) {
    stop("[07] Model '", active, "' missing $variables or $irf. Run step 05 to enrich and save again.")
  }
  # refuse if empty/zero
  has_nonzero_irf <- FALSE
  try({
    # quick scan across common placements
    scan_one <- function(obj) {
      if (is.null(obj)) return(FALSE)
      if (is.list(obj) && !is.null(obj$summary) && is.array(obj$summary)) return(any(obj$summary != 0, na.rm = TRUE))
      if (is.array(obj)) return(any(obj != 0, na.rm = TRUE))
      if (is.list(obj))  return(any(vapply(unlist(obj, recursive = TRUE, use.names = FALSE), is.numeric, FALSE)))
      FALSE
    }
    root <- mdl$irf
    has_nonzero_irf <- scan_one(root) || scan_one(root$combined) || scan_one(root$low) || scan_one(root$high)
  }, silent = TRUE)
  if (!has_nonzero_irf) {
    stop("[07] $irf present but all zeros or unreadable. Use the *analysis_ready* model from step 05.")
  }
  list(model_path = mp, model = mdl)
}

active_uncerts <- get_active_uncertainties(cfg)

#  Helpers  
hash_file <- function(path) if (fs::file_exists(path)) digest::digest(file = path, algo = "sha256") else NA_character_

# collect impulses mentioned in a spec (path or girf)
safe_validate_impulses <- function(spec, model_vars) {
  ty <- tolower(spec$type %||% "path")
  if (ty == "girf") {
    imps <- character(0)
    if (!is.null(spec$impulse)) imps <- c(imps, spec$impulse)
    if (!is.null(spec$scenarios)) imps <- c(imps, vapply(spec$scenarios, function(x) x$impulse %||% NA_character_, ""))
    imps <- unique(na.omit(imps))
  } else {
    sch_imps   <- unique(vapply(spec$schedule %||% list(), function(s) s$impulse %||% s$shock %||% NA_character_, character(1)))
    shock_imps <- names(spec$shocks %||% list())
    imps <- unique(na.omit(c(sch_imps, shock_imps)))
  }
  missing <- setdiff(imps, model_vars)
  if (length(missing)) stop(glue("Scenario '{spec$name %||% 'scenario'}' uses impulses not in model: {toString(missing)}"))
  invisible(TRUE)
}

# simple causal convolution (path-mode)
convolve_causal <- function(u, k) {
  H <- length(u); L <- length(k); y <- numeric(H)
  for (t in 1:H) { lim <- min(L - 1L, t - 1L); if (lim >= 0L) for (h in 0:lim) y[t] <- y[t] + u[t - h] * k[h + 1L] }
  y
}

# PATH-MODE
simulate_with_tensor <- function(scenario, tensor, drop_h0 = FALSE, include_bands = FALSE) {
  H <- tensor$H
  U <- matrix(0, nrow = H, ncol = length(tensor$impulses),
              dimnames = list(t = 1:H, imp = tensor$impulses))
  if (!is.null(scenario$schedule) && length(scenario$schedule)) {
    for (s in scenario$schedule) {
      imp <- s$impulse %||% s$shock
      if (is.null(imp) || !imp %in% tensor$impulses) next
      s0  <- max(1L, as.integer(s$t_start %||% 1L))
      s1  <- min(H,   as.integer(s$t_end   %||% s0))
      amp <- as.numeric(s$size %||% s$amplitude %||% 1)
      if (s0 <= s1) U[s0:s1, imp] <- U[s0:s1, imp] + amp
    }
  }
  if (!is.null(scenario$shocks)) {
    for (nm in names(scenario$shocks)) if (nm %in% tensor$impulses) U[1, nm] <- U[1, nm] + as.numeric(scenario$shocks[[nm]])
  }
  IRF_core <- tensor$IRF
  if (isTRUE(drop_h0) && dim(IRF_core)[3] >= 2) IRF_core <- IRF_core[, , -1, drop = FALSE]
  Yd <- matrix(0, nrow = tensor$H, ncol = length(tensor$responses),
               dimnames = list(t = 1:tensor$H, variable = tensor$responses))
  for (imp in tensor$impulses) {
    u <- U[, imp]; if (all(u == 0)) next
    for (resp in tensor$responses) {
      kern <- as.numeric(IRF_core[resp, imp, ])
      if (!all(kern == 0)) Yd[, resp] <- Yd[, resp] + convolve_causal(u, kern)
    }
  }
  tb <- tibble(
    t           = rep(seq_len(tensor$H), times = length(tensor$responses)),
    variable    = rep(tensor$responses, each = tensor$H),
    value       = as.vector(Yd),
    scenario    = scenario$name %||% "scenario",
    is_baseline = FALSE,
    regime      = tensor$regime
  )
  if (include_bands && !is.null(tensor$IRF_lo) && !is.null(tensor$IRF_hi)) {
    accumulate_band <- function(IRF_band) {
      Yb <- matrix(0, nrow = tensor$H, ncol = length(tensor$responses),
                   dimnames = list(t = 1:tensor$H, variable = tensor$responses))
      for (imp in tensor$impulses) {
        u <- U[, imp]; if (all(u == 0)) next
        for (resp in tensor$responses) {
          kern <- as.numeric(IRF_band[resp, imp, ])
          if (!all(kern == 0)) Yb[, resp] <- Yb[, resp] + convolve_causal(u, kern)
        }
      }
      as.vector(Yb)
    }
    tb$value_lo <- accumulate_band(tensor$IRF_lo)
    tb$value_hi <- accumulate_band(tensor$IRF_hi)
  }
  tb
}

# IRF helpers (for GIRF fallback) 

# Fuzzy translator: map arbitrary names onto model$variables using edit distance
.make_name_translator <- function(vars) {
  vars <- as.character(vars)
  function(nm) {
    if (is.null(nm)) return(character(0))
    nm <- as.character(nm)
    out <- character(length(nm))
    for (i in seq_along(nm)) {
      x <- nm[i]
      if (is.na(x) || !nzchar(x)) { out[i] <- NA_character_; next }
      if (x %in% vars)           { out[i] <- x;          next }
      d  <- utils::adist(tolower(x), tolower(vars))
      j  <- which.min(d)
      out[i] <- if (length(j) && d[j] <= 2) vars[j] else NA_character_
    }
    out
  }
}

# Build a k×k×H IRF cube from "anything".
build_irf_cube_any <- function(obj, vars, H, .depth = 0L) {
  `%||%` <- function(x,y) if (is.null(x)) y else x
  k <- length(vars)
  as_cube <- function() array(0, dim = c(k, k, H),
                              dimnames = list(resp = vars, imp = vars, h = seq_len(H)))
  IRF <- as_cube()
  if (is.null(obj)) return(IRF)
  
  translate <- .make_name_translator(vars)
  
  # LONG DATA-FRAME / TIBBLE 
  if (is.data.frame(obj) && nrow(obj)) {
    nm <- tolower(names(obj))
    col_resp <- which(nm %in% c("response","resp","variable","var","y"))
    col_imp  <- which(nm %in% c("impulse","imp","shock","x"))
    col_h    <- which(nm %in% c("h","horizon","lag","step"))
    col_val  <- which(nm %in% c("value","irf","median","mean","p50","point","central"))
    col_stat <- which(nm %in% c("stat","quantile","percentile","q","band"))
    if (length(col_resp) && length(col_imp) && length(col_h) && length(col_val)) {
      df <- obj
      if (length(col_stat)) {
        s <- tolower(df[[col_stat[1]]])
        pick <- which(s %in% c("50%","p50","median","mean","point","central"))
        if (length(pick)) df <- df[pick, , drop = FALSE]
      }
      rmap <- translate(df[[col_resp[1]]]); imap <- translate(df[[col_imp[1]]])
      keep <- !is.na(rmap) & !is.na(imap)
      if (any(keep)) {
        df <- df[keep, , drop = FALSE]; rmap <- rmap[keep]; imap <- imap[keep]
        hidx <- pmax(1L, pmin(H, as.integer(df[[col_h[1]]])))
        for (j in seq_len(nrow(df))) {
          IRF[rmap[j], imap[j], hidx[j]] <- as.numeric(df[[col_val[1]]][j])
        }
        return(IRF)
      }
    }
  }
  
  pull_num <- function(x) {
    if (is.numeric(x)) return(as.numeric(x))
    if (is.list(x)) {
      for (nm in c("50%","p50","median","mean","point","central","irf","IRF","value","y")) {
        if (!is.null(x[[nm]]) && is.numeric(x[[nm]])) return(as.numeric(x[[nm]]))
      }
    }
    NULL
  }
  
  # 4D SUMMARY [stat, h, resp, imp]
  if (is.array(obj) && length(dim(obj)) == 4) {
    dn <- dimnames(obj)
    if (!is.null(dn)) {
      stat_names <- tolower(dn[[1]] %||% character())
      pref <- c("50%","p50","median","mean","point","central")
      sidx <- if (length(stat_names)) { hit <- which(stat_names %in% pref); if (length(hit)) hit[1] else 1 } else 1
      rnames <- translate(dn[[3]] %||% character())
      inames <- translate(dn[[4]] %||% character())
      ok_r <- which(!is.na(rnames)); ok_i <- which(!is.na(inames))
      if (length(ok_r) && length(ok_i)) {
        for (ri in ok_r) for (ii in ok_i) {
          vec <- obj[sidx, , dn[[3]][ri], dn[[4]][ii], drop = TRUE]
          if (is.numeric(vec) && length(vec)) {
            L <- min(H, length(vec)); IRF[rnames[ri], inames[ii], seq_len(L)] <- as.numeric(vec[seq_len(L)])
          }
        }
        if (any(IRF != 0)) return(IRF)
      }
    }
    # Unnamed: assume dims like [stat, h, k, k]
    d <- dim(obj); k_dims <- which(d == k)
    if (length(k_dims) >= 2) {
      stat_dim <- setdiff(1:4, k_dims)[1]; h_dim <- setdiff(1:4, c(k_dims[1], k_dims[2], stat_dim))[1]
      x <- aperm(obj, c(stat_dim, h_dim, k_dims[1], k_dims[2])) # [stat, h, k, k]
      sidx <- 1
      for (r in seq_len(k)) for (i in seq_len(k)) {
        vec <- x[sidx, , r, i, drop = TRUE]
        if (is.numeric(vec) && length(vec)) {
          L <- min(H, length(vec)); IRF[vars[r], vars[i], seq_len(L)] <- as.numeric(vec[seq_len(L)])
        }
      }
      if (any(IRF != 0)) return(IRF)
    }
  }
  
  # 4D DRAWS [h, resp, imp, draw] 
  if (is.array(obj) && length(dim(obj)) == 4) {
    dn <- dimnames(obj); d <- dim(obj)
    if (!is.null(dn)) {
      rnames <- translate(dn[[2]] %||% character())
      inames <- translate(dn[[3]] %||% character())
      ok_r <- which(!is.na(rnames)); ok_i <- which(!is.na(inames))
      if (length(ok_r) && length(ok_i)) {
        x <- obj
        m <- apply(x, c(1,2,3), mean, na.rm = TRUE) # avg draws
        for (ri in ok_r) for (ii in ok_i) {
          vec <- m[, dn[[2]][ri], dn[[3]][ii], drop = TRUE]
          if (is.numeric(vec) && length(vec)) {
            L <- min(H, length(vec)); IRF[rnames[ri], inames[ii], seq_len(L)] <- as.numeric(vec[seq_len(L)])
          }
        }
        if (any(IRF != 0)) return(IRF)
      }
    }
    # Unnamed
    k_dims <- which(d == k)
    if (length(k_dims) >= 2) {
      rest <- setdiff(1:4, k_dims)
      h_dim <- rest[which.min(pmax(1, d[rest] > 64))]
      draw_dim <- setdiff(rest, h_dim)[1]
      x <- aperm(obj, c(h_dim, k_dims[1], k_dims[2], draw_dim)) # [h, k, k, draw]
      m <- apply(x, c(1,2,3), mean, na.rm = TRUE)               # [h, k, k]
      L <- min(H, dim(m)[1]); IRF[, , seq_len(L)] <- aperm(m[seq_len(L), , , drop = FALSE], c(2,3,1))
      dimnames(IRF) <- list(resp = vars, imp = vars, h = seq_len(H))
      if (any(IRF != 0)) return(IRF)
    }
  }
  
  # 3D array
  if (is.array(obj) && length(dim(obj)) == 3) {
    dn <- dimnames(obj); d <- dim(obj)
    if (!is.null(dn)) {
      rnames <- translate(dn[[1]] %||% character())
      inames <- translate(dn[[2]] %||% character())
      ok_r <- which(!is.na(rnames)); ok_i <- which(!is.na(inames))
      if (length(ok_r) && length(ok_i)) {
        for (ri in ok_r) for (ii in ok_i) {
          vec <- obj[dn[[1]][ri], dn[[2]][ii], , drop = TRUE]
          if (is.numeric(vec) && length(vec)) {
            L <- min(H, length(vec)); IRF[rnames[ri], inames[ii], seq_len(L)] <- as.numeric(vec[seq_len(L)])
          }
        }
        if (any(IRF != 0)) return(IRF)
      }
    }
    # Unnamed & two k-dims assume order == model$variables
    k_dims <- which(d == k)
    if (length(k_dims) >= 2) {
      h_dim   <- setdiff(1:3, k_dims)[1]
      respdim <- k_dims[1]; impdim <- k_dims[2]
      x <- aperm(obj, c(respdim, impdim, h_dim))                
      L <- min(H, dim(x)[3])
      for (r in seq_len(k)) for (i in seq_len(k)) {
        vec <- x[r, i, seq_len(L), drop = TRUE]
        if (is.numeric(vec) && length(vec)) IRF[vars[r], vars[i], seq_len(L)] <- as.numeric(vec)
      }
      if (any(IRF != 0)) return(IRF)
    }
  }
  
  # Per‑impulse lists
  if (is.list(obj) && length(names(obj))) {
    irf_nodes <- grep("^irf_", names(obj), value = TRUE)
    if (length(irf_nodes)) {
      get_impulse <- function(node_name) {
        m <- regexec("^irf_(?:low|high|combined|[A-Za-z0-9]+)_([A-Za-z0-9_]+)$", node_name)
        mm <- regmatches(node_name, m)[[1]]
        if (length(mm) >= 2) mm[2] else sub("^irf_","", node_name)
      }
      for (node in irf_nodes) {
        imp_raw <- get_impulse(node)
        imp <- translate(imp_raw)
        if (is.na(imp)) next
        x <- obj[[node]]
        # A: named numeric vector 
        if (is.numeric(x) && length(names(x))) {
          nms <- names(x)
          for (j in seq_along(x)) {
            nmj <- nms[j]
            m <- regexec("^(.+?)[_\\- ]?(\\d+)$", nmj)
            mm <- regmatches(nmj, m)[[1]]
            if (length(mm) >= 3) {
              resp <- translate(mm[2]); h <- as.integer(mm[3])
              if (!is.na(resp) && h >= 1L && h <= H) IRF[resp, imp, h] <- as.numeric(x[j])
            }
          }
        }
        # B: list of named numeric vectors
        if (is.list(x)) {
          for (el_name in names(x)) {
            v <- x[[el_name]]
            if (is.numeric(v) && length(names(v))) {
              nms <- names(v)
              for (j in seq_along(v)) {
                nmj <- nms[j]
                m <- regexec("^(.+?)[_\\- ]?(\\d+)$", nmj)
                mm <- regmatches(nmj, m)[[1]]
                if (length(mm) >= 3) {
                  resp <- translate(mm[2]); h <- as.integer(mm[3])
                  if (!is.na(resp) && h >= 1L && h <= H) IRF[resp, imp, h] <- as.numeric(v[j])
                }
              }
            } else {
              # If it’s a plain numeric vector with length H and el_name is a response
              resp <- translate(el_name)
              if (!is.na(resp) && is.numeric(v)) {
                L <- min(H, length(v)); IRF[resp, imp, seq_len(L)] <- as.numeric(v[seq_len(L)])
              }
            }
          }
        }
      }
      if (any(IRF != 0)) return(IRF)
    }
    
    rmap <- translate(names(obj))
    if (any(!is.na(rmap))) {
      out <- as_cube()
      for (idx_r in which(!is.na(rmap))) {
        sub <- obj[[ names(obj)[idx_r] ]]
        if (is.list(sub)) {
          imap <- translate(names(sub))
          for (idx_i in which(!is.na(imap))) {
            v <- pull_num(sub[[ names(sub)[idx_i] ]])
            if (!is.null(v) && length(v)) {
              L <- min(H, length(v)); out[rmap[idx_r], imap[idx_i], seq_len(L)] <- v[seq_len(L)]
            }
          }
        }
      }
      if (any(out != 0)) return(out)
    }
    imap <- translate(names(obj))
    if (any(!is.na(imap))) {
      out <- as_cube()
      for (idx_i in which(!is.na(imap))) {
        sub <- obj[[ names(obj)[idx_i] ]]
        if (is.list(sub)) {
          rmap <- translate(names(sub))
          for (idx_r in which(!is.na(rmap))) {
            v <- pull_num(sub[[ names(sub)[idx_r] ]])
            if (!is.null(v) && length(v)) {
              L <- min(H, length(v)); out[rmap[idx_r], imap[idx_i], seq_len(L)] <- v[seq_len(L)]
            }
          }
        }
      }
      if (any(out != 0)) return(out)
    }
    
    kids <- intersect(names(obj) %||% character(), c(
      "regimes","combined","low","high","summary","draws",
      "median","mean","p50","point","central","irf","IRF","data","df","tbl","envelope","central_tendency"
    ))
    for (kk in kids) {
      got <- build_irf_cube_any(obj[[kk]], vars, H, .depth + 1L)
      if (any(got != 0)) return(got)
    }
  }
  
  IRF
}

# a tensor for a named regime
irf_tensor_for_regime <- function(model, regime, H) {
  vars <- model$variables
  make_ten <- function(IRF, regime_used) {
    list(IRF = IRF, H = H, regime = regime_used,
         responses = dimnames(IRF)$resp, impulses = dimnames(IRF)$imp)
  }
  root <- model$irf
  
  if (identical(tolower(regime), "combined") &&
      is.list(root) && !is.null(root$low) && !is.null(root$high)) {
    IRF_low  <- build_irf_cube_any(root$low,  vars, H)
    IRF_high <- build_irf_cube_any(root$high, vars, H)
    IRF <- (IRF_low + IRF_high) / 2
    if (any(IRF != 0)) return(make_ten(IRF, "combined"))
  }
  
  reg_obj <- if (is.list(root) && !is.null(root$regimes)) root$regimes[[regime]] else root[[regime]]
  if (is.null(reg_obj)) reg_obj <- root
  IRF <- build_irf_cube_any(reg_obj, vars, H)
  if (any(IRF != 0)) return(make_ten(IRF, regime))
  
  if (exists("prepare_irf_tensor")) {
    ten <- try(prepare_irf_tensor(model = model, regime = regime,
                                  responses_keep = vars, impulses_keep = vars,
                                  H = H, verbose = FALSE), silent = TRUE)
    if (!inherits(ten, "try-error") && is.array(ten$IRF) && any(ten$IRF != 0)) return(ten)
  }
  NULL
}

girf_from_irf <- function(model, impulse, H, regime, shock_size_sigma = 1) {
  ten <- irf_tensor_for_regime(model, regime, H)
  if (is.null(ten) || !is.array(ten$IRF) || !any(ten$IRF != 0))
    stop("[07] IRF fallback failed; no usable IRF tensor.")
  if (!(impulse %in% dimnames(ten$IRF)$imp))
    stop("[07] Impulse '", impulse, "' not found in IRF tensor.")
  out <- matrix(0, nrow = H, ncol = length(model$variables),
                dimnames = list(h = seq_len(H), var = model$variables))
  for (resp in model$variables) {
    out[, resp] <- shock_size_sigma * as.numeric(ten$IRF[resp, impulse, ])
  }
  out
}

# GIRF-MODE helpers 
get_sigma_for_impulse <- function(model, impulse) {
  if (!is.null(model$shock_sigma) && !is.null(model$shock_sigma[impulse])) return(as.numeric(model$shock_sigma[impulse]))
  if (!is.null(model$residuals) && impulse %in% colnames(model$residuals)) return(stats::sd(model$residuals[, impulse], na.rm = TRUE))
  1.0
}

run_girf_once <- function(model, sp, impulse, H, regime_label) {
  # identification: accept method or type, default to chol
  id <- sp$identification %||% list()
  if (!is.null(id$type) && is.null(id$method)) id$method <- id$type
  id$method   <- id$method   %||% "chol"
  id$ordering <- canonicalize_ordering(id$ordering, model$variables)
  
  start_state <- switch(tolower(regime_label),
                        "low"  = "low",
                        "high" = "high",
                        "combined" = "mix",
                        "mix")
  
  # shock size handling 
  raw_size <- (if (!is.null(sp$shocks) && !is.null(sp$shocks[[impulse]])) sp$shocks[[impulse]] else sp$size) %||% 1
  units <- sp$units %||% (if (!is.null(sp$scale) && isTRUE(sp$scale$by_sigma)) "sigma" else NULL)
  if (!is.null(units) && identical(tolower(units), "abs")) {
    sig <- get_sigma_for_impulse(model, impulse)
    shock_size <- as.numeric(raw_size) / ifelse(sig > 0, sig, 1)
  } else {
    shock_size <- as.numeric(raw_size)
  }
  
  B <- as.integer((sp$bands %||% list())$draws %||% 100L)
  
  tryCatch(
    {
      compute_tvar_girf(
        model          = model,
        impulse        = impulse,
        shock_size     = shock_size,
        H              = as.integer(H),
        B              = B,
        identification = id,
        start_state    = start_state,
        hysteresis     = sp$hysteresis %||% NULL
      )
    },
    error = function(e) {
      message("[07] GIRF Monte-Carlo failed (", e$message, "). Falling back to IRF convolution.")
      g <- girf_from_irf(model, impulse = impulse, H = as.integer(H),
                         regime = regime_label, shock_size_sigma = shock_size)
      list(girf = g, ci = NULL, dpaths = NULL,
           impulse = impulse, shock_size = shock_size,
           start_state = start_state, A = NULL, identification = id)
    }
  )
}

# Takes a GIRF spec and returns a tidy tibble 
run_girf_scenario <- function(model, sp, H, regime_label) {
  subs <- sp$scenarios %||% list(list(impulse = sp$impulse))
  out  <- vector("list", length(subs))
  for (i in seq_along(subs)) {
    sub <- subs[[i]]
    imp <- sub$impulse %||% sp$impulse
    if (is.null(imp)) stop("GIRF scenario must specify 'impulse'.")
    # allow sub-scenario to override size/units
    sp_i <- sp
    if (!is.null(sub$size))  sp_i$size  <- sub$size
    if (!is.null(sub$units)) sp_i$units <- sub$units
    res <- run_girf_once(model, sp_i, impulse = imp, H = H, regime_label = regime_label)
    g   <- res$girf        # [H x k] difference paths
    tb  <- tibble(
      t           = rep(seq_len(H), times = ncol(g)),
      variable    = rep(colnames(g) %||% model$variables, each = H),
      value       = as.vector(g),
      scenario    = (sub$name %||% sp$name %||% "GIRF scenario"),
      is_baseline = FALSE,
      regime      = regime_label
    )
    out[[i]] <- tb
  }
  dplyr::bind_rows(out)
}

# Load scenarios 
ymls  <- fs::dir_ls(scen_dir, regexp = "\\.(yml|yaml)$", type = "file")
if (!length(ymls)) stop("No scenario YAMLs found in ", scen_dir)
if (FAST_MODE && length(ymls) > MAX_SCENARIOS) ymls <- ymls[seq_len(MAX_SCENARIOS)]
specs <- purrr::map(ymls, load_scenario_yaml)

# Horizon strategy
H_cfg <- as.integer(cfg$irf$horizon %||% 48L)
if (FAST_MODE) {
  specs <- purrr::map(specs, function(sp) { sp$horizon <- min(as.integer(sp$horizon %||% HARD_H), HARD_H); sp })
} else {
  specs <- purrr::map(specs, function(sp) { sp$horizon <- min(as.integer(sp$horizon %||% H_cfg), H_cfg); sp })
}

# Prepare & run 
t_start <- Sys.time()
tidy_list   <- list()
meta_events <- list()

for (unc in active_uncerts) {
  mdl <- get_active_model(unc)
  model_path <- mdl$model_path
  tvar_model <- mdl$model

  # reload YAMLs 
  ymls  <- fs::dir_ls(scen_dir, regexp = "\\.(yml|yaml)$", type = "file")
  if (!length(ymls)) stop("No scenario YAMLs found in ", scen_dir)
  if (FAST_MODE && length(ymls) > MAX_SCENARIOS) ymls <- ymls[seq_len(MAX_SCENARIOS)]
  specs <- purrr::map(ymls, load_scenario_yaml)
  H_cfg <- as.integer(cfg$irf$horizon %||% 48L)
  specs <- if (FAST_MODE) {
    purrr::map(specs, function(sp){ sp$horizon <- min(as.integer(sp$horizon %||% HARD_H), HARD_H); sp })
  } else {
    purrr::map(specs, function(sp){ sp$horizon <- min(as.integer(sp$horizon %||% H_cfg), H_cfg); sp })
  }

  get_regimes_for_spec <- function(sp) {
    rg <- tolower(sp$regime %||% "combined")
    if (rg %in% c("all","*")) unique(cfg$irf$regimes %||% c("combined","low","high")) else rg
  }
  tensor_cache <- new.env(parent = emptyenv())
  
  .all_zero <- function(x) is.array(x) && length(x) > 0 && all(is.na(x) | x == 0)
  
  get_tensor <- function(regime, H) {
    key <- paste(regime, H, sep="__")
    if (exists(key, envir = tensor_cache)) return(tensor_cache[[key]])
    
    vars <- tvar_model$variables
    
    ten <- try(
      prepare_irf_tensor(
        model          = tvar_model,
        regime         = regime,
        responses_keep = vars,
        impulses_keep  = vars,
        H              = H,
        verbose        = FALSE
      ),
      silent = TRUE
    )
    
    need_fallback <- inherits(ten, "try-error") || is.null(ten$IRF) || .all_zero(ten$IRF)
    if (need_fallback) {
      fb <- irf_tensor_for_regime(tvar_model, regime, H)  # uses build_irf_cube_any + avg(low,high)
      if (is.null(fb) || .all_zero(fb$IRF)) {
        stop(paste0(
          "[07/path] Could not assemble a non-zero IRF tensor.\n",
          "- regime requested: '", regime, "'\n",
          "- top-level names(model$irf): ", paste(names(tvar_model$irf), collapse = ", "), "\n",
          "- re-run 05 to compute/save IRFs, or ensure names match model$variables."
        ))
      }
      ten <- fb
      message("[07/path] Using robust IRF fallback for regime '", fb$regime, "'.")
    }
    
    if (is.null(ten$responses)) ten$responses <- dimnames(ten$IRF)$resp %||% vars
    if (is.null(ten$impulses))  ten$impulses  <- dimnames(ten$IRF)$imp  %||% vars
    ten$H      <- as.integer(H)
    ten$regime <- regime
    
    tensor_cache[[key]] <- ten
    ten
  }

  for (i in seq_along(specs)) {
    sp <- specs[[i]]
    sp$type <- tolower(sp$type %||% "path")

    validate_scenario_spec(sp, model_vars = tvar_model$variables)
    safe_validate_impulses(sp, model_vars = tvar_model$variables)

    regimes <- get_regimes_for_spec(sp)

    for (rg in regimes) {
      Hrun <- as.integer(sp$horizon)
      message(sprintf("[07] %s | %d/%d — %s | type=%s | regime=%s | H=%d | vars=%d",
                      toupper(unc), i, length(specs), sp$name %||% basename(ymls[i]),
                      sp$type, rg, Hrun, length(tvar_model$variables)))

      if (sp$type == "girf") {
        td <- run_girf_scenario(
          model         = tvar_model,
          sp            = sp,
          H             = Hrun,
          regime_label  = rg
        )
      } else {
        tensor <- get_tensor(rg, Hrun)
        td <- simulate_with_tensor(
          scenario      = sp,
          tensor        = tensor,
          drop_h0       = FALSE,
          include_bands = (!FAST_MODE && isTRUE(COMPUTE_BANDS))
        )
      }

      td$uncertainty <- unc
      base <- dplyr::mutate(td, value = 0, value_lo = NA_real_, value_hi = NA_real_, is_baseline = TRUE)
      tidy_list[[length(tidy_list)+1L]] <- dplyr::bind_rows(base, td)

      meta_events[[length(meta_events)+1L]] <- list(
        uncertainty = unc,
        scenario    = sp$name %||% basename(ymls[i]),
        yaml_path   = fs::path_rel(ymls[i]),
        yaml_hash   = hash_file(ymls[i]),
        regime      = rg,
        horizon     = Hrun,
        type        = sp$type,
        impulses    = if (sp$type == "girf")
          unique(c(sp$impulse %||% character(0),
                   vapply(sp$scenarios %||% list(), function(x) x$impulse %||% NA_character_, character(1))))
        else
          intersect(names(sp$shocks %||% list()), tvar_model$variables),
        scheduled   = length(sp$schedule %||% list()) > 0,
        n_variables = length(tvar_model$variables),
        B           = sp$B %||% (sp$bands %||% list())$draws %||% NA_integer_,
        ci_level    = (sp$bands %||% list())$ci_level %||% NA_real_
      )
    }
  }
}

if (!length(tidy_list)) stop("[07] No scenario produced output.")
tidy_all <- dplyr::bind_rows(tidy_list)

# Output & logs 
run_id   <- format(Sys.time(), "%Y%m%d_%H%M%S")

fs::dir_create(out_dir, recurse = TRUE)
fs::dir_create(log_dir, recurse = TRUE)
fs::dir_create(fig_dir, recurse = TRUE)

ymls_final <- fs::dir_ls(scen_dir, regexp = "\\.(yml|yaml)$", type = "file")
bundle_h <- digest::digest(paste(sort(vapply(ymls_final, hash_file, "")), collapse = "|"), algo = "sha256")

out_csv <- fs::path(out_dir, glue("scenario_results_tidy_{run_id}.csv"))

# Write the csv
tryCatch({
  readr::write_csv(tidy_all, out_csv)
  message(sprintf("[07] Wrote: %s (%d rows)", fs::path_rel(out_csv), nrow(tidy_all)))
}, error = function(e) {
  stop("[07] Failed to write scenario CSV: ", conditionMessage(e))
})

# Stable alias 
alias_csv <- fs::path(out_dir, "scenario_results_tidy_latest.csv")
if (fs::file_exists(out_csv)) {
  ok <- try(fs::file_copy(out_csv, alias_csv, overwrite = TRUE), silent = TRUE)
  if (inherits(ok, "try-error")) {
    warning("[07] Skipped alias copy: ", conditionMessage(attr(ok, "condition")))
  } else {
    message(sprintf("[07] Alias updated: %s", fs::path_rel(alias_csv)))
  }
} else {
  warning("[07] Skipped alias copy: source missing: ", fs::path_rel(out_csv))
}

if (isTRUE(FAST_MODE)) {
  out_fast <- fs::path(out_dir, "scenario_results_tidy_fast.csv")
  if (fs::file_exists(out_csv)) {
    try(fs::file_copy(out_csv, out_fast, overwrite = TRUE), silent = TRUE)
  }
}

# Meta & logs
meta <- list(
  run_id         = run_id,
  started_at     = as.character(t_start),
  finished_at    = as.character(Sys.time()),
  duration_secs  = as.numeric(difftime(Sys.time(), t_start, units = "secs")),
  fast_mode      = FAST_MODE,
  compute_bands  = COMPUTE_BANDS && !FAST_MODE,
  make_plots     = MAKE_PLOTS && !FAST_MODE,
  seed_master    = SEED_MASTER,
  yaml_dir       = fs::path_rel(scen_dir),
  yaml_files     = lapply(as.list(ymls_final), function(p) list(path = fs::path_rel(p), sha256 = hash_file(p))),
  yaml_bundle_sha= bundle_h,
  n_rows         = nrow(tidy_all),
  events         = meta_events
)

if (exists("model_path")) {
  meta$model_path <- fs::path_rel(model_path)
  meta$model_hash <- hash_file(model_path)
}

meta_json <- fs::path(log_dir, glue("scenario_run_meta_{run_id}.json"))
writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE), meta_json)
message(sprintf("[07] Meta: %s", fs::path_rel(meta_json)))

# plots 
if (!FAST_MODE && MAKE_PLOTS) {
  if (exists("plot_scenario_paths")) {
    try(plot_scenario_paths(tidy_all, out_dir = fig_dir), silent = TRUE)
  }
  if (exists("plot_counterfactual_vs_baseline")) {
    try(plot_counterfactual_vs_baseline(tidy_all, out_dir = fig_dir), silent = TRUE)
  }
  if (exists("plot_scenario_fan")) {
    try(plot_scenario_fan(tidy_all, out_dir = fig_dir), silent = TRUE)
  }
  if (exists("plot_irf_envelope_vs_scenario")) {
    try(plot_irf_envelope_vs_scenario(tidy_all, model = tvar_model, out_dir = fig_dir), silent = TRUE)
  }
  
  # fallback: 2 basic ggplots if custom fns are missing
  if (!exists("plot_scenario_paths") && !exists("plot_scenario_fan")) {
    suppressPackageStartupMessages(require(ggplot2))
    
    p_paths <- tidy_all |>
      dplyr::filter(!is_baseline) |>
      ggplot(aes(t, value, group = interaction(scenario, regime))) +
      geom_line(alpha = 0.6) +
      facet_wrap(~ variable, scales = "free_y") +
      labs(title = "Scenario paths (non-baseline)", x = "horizon", y = "delta") +
      theme_minimal(base_size = 11)
    
    ggplot2::ggsave(
      filename = fs::path(fig_dir, glue::glue("scenario_paths_{run_id}.png")),
      plot = p_paths, width = 14, height = 9, dpi = 150
    )
    
    p_top <- tidy_all |>
      dplyr::filter(!is_baseline) |>
      dplyr::group_by(scenario, variable) |>
      dplyr::summarise(max_abs = max(abs(value), na.rm = TRUE), .groups = "drop") |>
      dplyr::group_by(scenario) |>
      dplyr::slice_max(max_abs, n = 10) |>
      ggplot(aes(reorder(variable, max_abs), max_abs)) +
      geom_col() +
      coord_flip() +
      facet_wrap(~ scenario, scales = "free_y") +
      labs(title = "Top 10 movers by |max| per scenario", x = NULL, y = "|max delta|") +
      theme_minimal(base_size = 11)
    
    ggplot2::ggsave(
      filename = fs::path(fig_dir, glue::glue("scenario_top_movers_{run_id}.png")),
      plot = p_top, width = 12, height = 8, dpi = 150
    )
  }
}

message(sprintf("[07] OK: %s (%d rows)", fs::path_rel(out_csv), nrow(tidy_all)))
message(sprintf("[07] Meta: %s", fs::path_rel(meta_json)))
message("Done.")