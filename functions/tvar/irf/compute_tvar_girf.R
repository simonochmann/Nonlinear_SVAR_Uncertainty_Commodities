compute_tvar_girf <- function(model,
                              impulse,        # name in model$variables
                              shock_size = 1, # σ-units unless scale_shock_by_sigma() overrides
                              H = 20L,
                              B = 1000L,
                              identification = list(type = "chol", ordering = model$variables),
                              start_state = c("low","high","mix"),
                              seed = NULL,
                              hysteresis = NULL) {
  
  stopifnot(is.list(model), !is.null(model$variables), impulse %in% model$variables)
  if (!is.null(seed)) set.seed(seed)
  start_state <- match.arg(start_state)
  
  k <- length(model$variables)
  
  # ---- Identification params (method/type are synonyms) ----
  identification <- identification %||% list()
  if (!is.null(identification$type) && is.null(identification$method)) {
    identification$method <- identification$type
  }
  if (is.null(identification$method)) {
    identification$method <- "chol"
  }
  if (is.null(identification$ordering)) {
    identification$ordering <- model$variables
  }
  
  # ---- 1) Build A and structural impact shock on 'impulse' ----
  A   <- build_contemporaneous_A(model, identification = identification)
  e_i <- build_shock_vector(model, impulse = impulse)          # unit vector aligned to 'impulse'
  eps0 <- scale_shock_by_sigma(model, e_i, shock_size = shock_size, A = A)
  
  # ---- 2) Common random draws for baseline and shocked paths ----
  draws_raw <- generate_counterfactual_baseline(model, H = H, B = B, seed = NULL)
  
  # Normalize shape for downstream: always pass a list(U = array)
  draws_arr <- if (is.list(draws_raw) && !is.null(draws_raw$U)) {
    draws_raw$U
  } else if (is.array(draws_raw)) {
    draws_raw
  } else {
    stop("generate_counterfactual_baseline returned unexpected type.")
  }
  draws_box <- list(U = draws_arr)
  
  if (getOption("tvar.debug", FALSE)) {
    message(sprintf("[girf] draws: B=%d, H=%d, k=%d", dim(draws_box$U)[1], dim(draws_box$U)[2], dim(draws_box$U)[3]))
  }
  
  r0 <- choose_initial_regimes(model, B = B, mode = start_state)
  
  base_paths <- simulate_structural_shock_fast(
    model, H = H, B = B, A = A,
    eps0 = rep(0, k),
    draws = draws_box,  # <-- pass list(U = ...)
    r0 = r0,
    hysteresis = hysteresis
  )
  shock_paths <- simulate_structural_shock_fast(
    model, H = H, B = B, A = A,
    eps0 = as.numeric(eps0),
    draws = draws_box,  # <-- pass list(U = ...)
    r0 = r0,
    hysteresis = hysteresis
  )
  
  # ---- 5) GIRF: average difference across B paths ----
  # Expect arrays shaped [B x H x k] with dimnames on the 3rd dim = variables
  dpaths <- shock_paths$Y - base_paths$Y
  girf   <- apply(dpaths, c(2,3), mean, na.rm = TRUE)  # [H x k]
  ci     <- attach_confidence_bands(dpaths)            # list/array with percentiles by [H x k]
  
  dimnames(girf) <- list(h = seq_len(H), var = model$variables)
  
  list(
    girf        = girf,
    ci          = ci,
    dpaths      = dpaths,
    impulse     = impulse,
    shock_size  = shock_size,
    start_state = start_state,
    A           = A,
    identification = identification
  )
}