#' Build FEVD shares from IRFs for the variables/horizon present in a delta slice
#'
#' @param delta_slice tibble with at least columns: regime, variable, t
#'        (optionally 'uncertainty'). Only used to infer H, variables, and uncertainty.
#' @param H integer horizon (if NULL, inferred as max(delta_slice$t))
#' @param regimes character vector of regimes to compute ("low","high","combined")
#'
#' @return tibble with columns:
#'         uncertainty (opt), regime, response, impulse, t, fevd_share
#'
#' @details
#'  - FEVD(h) := sum_{tau<=h} IRF^2(response<-impulse, tau) / sum_{j} sum_{tau<=h} IRF^2(response<-j, tau)
#'  - Uses prepare_irf_tensor(model, regime, ...), so assumes IRFs are orthogonalized.
#'  - The model is auto-resolved from the 'uncertainty' tag:
#'      models/tvar/{uncertainty}_tvar_model_analysis_ready_latest.rds
#'    with a couple of fallback patterns.
#'
build_fevd_from_irf <- function(delta_slice, H = NULL, regimes = c("combined")) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("fs", quietly = TRUE)
  requireNamespace("glue", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("here", quietly = TRUE)
  `%||%` <- function(x,y) if (is.null(x)) y else x
  
  # ---- ensure helper available
  if (!exists("prepare_irf_tensor", mode = "function")) {
    src <- here::here("functions","scenarios","simulate","prepare_irf_tensor.R")
    if (!fs::file_exists(src)) stop("[build_fevd_from_irf] missing prepare_irf_tensor.R at: ", src)
    sys.source(src, envir = .GlobalEnv)
  }
  
  # ---- infer uncertainty(s) and variables/horizon from the slice
  has_unc <- "uncertainty" %in% names(delta_slice)
  uncertainties <- if (has_unc) sort(unique(delta_slice$uncertainty)) else NA_character_
  H_target <- as.integer(H %||% max(delta_slice$t, na.rm = TRUE))
  if (is.na(H_target) || H_target < 1L) stop("[build_fevd_from_irf] invalid horizon derived.")
  
  # helper: model resolver per uncertainty
  find_model_for_uncertainty <- function(unc) {
    base <- here::here("models","tvar")
    pats <- c(
      glue::glue("{unc}_tvar_model_analysis_ready_latest.rds"),
      glue::glue("{unc}_tvar_model_latest.rds"),
      glue::glue("{unc}_tvar_model.rds")
    )
    for (p in pats) {
      f <- fs::path(base, p)
      if (fs::file_exists(f)) return(f)
    }
    # final regex fallback
    cand <- try(fs::dir_ls(base, regexp = paste0("^", unc, ".*\\.rds$"), type = "file"), silent = TRUE)
    if (!inherits(cand, "try-error") && length(cand) >= 1) return(cand[[1]])
    stop("[build_fevd_from_irf] could not find model for uncertainty='", unc, "'. Looked in: ", base)
  }
  
  out_all <- list()
  
  for (unc in uncertainties) {
    slice <- if (!is.na(unc)) dplyr::filter(delta_slice, .data$uncertainty == unc) else delta_slice
    # Variables present in this slice (ensure original model order later)
    vars_in <- sort(unique(slice$variable))
    
    # load model for this uncertainty
    if (is.na(unc)) {
      stop("[build_fevd_from_irf] 'uncertainty' column not found in delta slice. Run 07/08 with uncertainty tagging.")
    }
    mpath <- find_model_for_uncertainty(unc)
    model <- readr::read_rds(mpath)
    vars_model <- as.character(model$variables)
    if (!all(vars_in %in% vars_model)) {
      warning("[build_fevd_from_irf] Some variables in delta not found in model; will keep intersection only.")
    }
    keep_vars <- intersect(vars_model, vars_in)
    if (length(keep_vars) < 2L) stop("[build_fevd_from_irf] Need >=2 variables to compute FEVD; got: ", paste(keep_vars, collapse = ","))
    
    # compute FEVD for each requested regime
    for (reg in regimes) {
      # pull IRF cube [resp, imp, h]
      ten <- try(prepare_irf_tensor(
        model = model, regime = reg,
        responses_keep = keep_vars, impulses_keep = keep_vars,
        H = H_target, verbose = FALSE
      ), silent = TRUE)
      
      if (inherits(ten, "try-error") || is.null(ten$IRF) || !is.array(ten$IRF) || length(dim(ten$IRF)) != 3) {
        warning("[build_fevd_from_irf] could not build IRF cube for regime='", reg, "' (unc='", unc, "'). Skipping.")
        next
      }
      IRF <- ten$IRF
      if (all(IRF == 0)) {
        warning("[build_fevd_from_irf] IRF cube is all zeros for regime='", reg, "' (unc='", unc, "'). Skipping.")
        next
      }
      
      # Ensure dimnames are present and labeled
      dn <- dimnames(IRF)
      if (is.null(dn) || is.null(dn[[1]]) || is.null(dn[[2]])) {
        dimnames(IRF) <- list(resp = keep_vars, imp = keep_vars, h = seq_len(dim(IRF)[3]))
      } else {
        # enforce order to keep_vars if possible
        IRF <- IRF[keep_vars, keep_vars, , drop = FALSE]
        # label dimnames to avoid "Var1"/"Var2" surprises
        names(dimnames(IRF)) <- c("response","impulse","h")
      }
      
      K  <- dim(IRF)[1]
      HH <- dim(IRF)[3]
      Hh <- min(H_target, HH)
      
      # FEVD by horizon: loop h=1..Hh
      rows <- vector("list", Hh)
      eps  <- .Machine$double.eps
      
      for (h in seq_len(Hh)) {
        # cumulative power through horizon h
        # P[r,i] = sum_{tau<=h} IRF[r,i,tau]^2
        P <- apply(IRF[,,seq_len(h), drop = FALSE], c(1,2), function(x) sum(x^2))
        # denom[r] = sum_i P[r,i]
        denom <- rowSums(P)
        # shares[r,i] = P[r,i] / denom[r]  (guard divide-by-zero)
        denom_safe <- pmax(denom, eps)
        shares <- sweep(P, 1L, denom_safe, "/")
        
        # to long tibble; handle both named and unnamed dimnames robustly
        df <- as.data.frame(as.table(shares))
        cols <- names(df)
        idx_cols <- setdiff(cols, "Freq")
        # normalize names to response/impulse regardless of Var1/Var2/custom
        if (length(idx_cols) != 2L) stop("[build_fevd_from_irf] unexpected as.table() structure.")
        names(df)[match(idx_cols, names(df))] <- c("response","impulse")
        df <- dplyr::transmute(
          df,
          response   = as.character(.data$response),
          impulse    = as.character(.data$impulse),
          t          = h,
          fevd_share = as.numeric(.data$Freq),
          regime     = reg
        )
        if (!is.na(unc)) df$uncertainty <- unc
        rows[[h]] <- df
      }
      
      fevd_tbl_reg <- dplyr::bind_rows(rows)
      # order & columns
      fevd_tbl_reg <- fevd_tbl_reg %>%
        dplyr::select(dplyr::any_of(c("uncertainty")), "regime", "response", "impulse", "t", "fevd_share") %>%
        dplyr::arrange(.data$response, .data$impulse, .data$t)
      
      out_all[[paste(unc, reg, sep = "::")]] <- fevd_tbl_reg
    } # regimes
  } # uncertainties
  
  if (!length(out_all)) stop("[build_fevd_from_irf] produced no FEVD rows.")
  dplyr::bind_rows(out_all)
}
