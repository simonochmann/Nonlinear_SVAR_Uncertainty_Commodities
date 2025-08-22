#' Shapley contributions of shocks to forecast deltas (per scenario × t)
#'
#' Computes (exact or Monte Carlo) Shapley values for non/additive models.
#' For each scenario, we consider permutations of the included shocks and average
#' the marginal contribution f(S ∪ {i}) - f(S) to obtain φ_i.
#'
#' Two generators for f(S):
#'   1) callback mode: supply `path_fun(scenario, subset_df)` returning a tibble
#'      with columns: regime, variable, t, value for the combined subset.
#'      `subset_df` has columns: shock_id, weight.
#'   2) superpose fallback: if `path_fun` is NULL, we approximate
#'      f(S) = baseline + Σ_{j∈S} weight_j * (single_j - baseline).
#'
#' @param baseline tibble: regime, variable, t, value  (common baseline)
#' @param scenario_shock_map tibble: scenario, shock_id, weight (defaults to 1 if missing)
#' @param single_shock_paths optional tibble: shock_id, regime, variable, t, value
#' @param path_fun optional function(scenario, subset_df) -> tibble(regime, variable, t, value)
#' @param scenarios optional character vector to restrict scenarios (NULL = all in map)
#' @param n_perm integer Monte Carlo permutations per scenario if using MC
#' @param exact_if_k_le integer: do exact Shapley if #shocks ≤ this (default 7)
#' @param seed optional integer RNG seed for reproducibility (MC)
#'
#' @return tibble: scenario, shock_id, regime, variable, t, shapley, method, generator, perms_used
#'
#' @examples
#' # shap <- shapley_contributions(baseline, scenario_shock_map, single_shock_paths,
#' #                               path_fun = my_generator, n_perm = 512, exact_if_k_le = 6)
#'
shapley_contributions <- function(
    baseline,
    scenario_shock_map,
    single_shock_paths = NULL,
    path_fun = NULL,
    scenarios = NULL,
    n_perm = 512,
    exact_if_k_le = 7,
    seed = NULL
) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)
  requireNamespace("glue", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  
  # --- validate inputs
  stopifnot(all(c("regime","variable","t","value") %in% names(baseline)))
  stopifnot(all(c("scenario","shock_id") %in% names(scenario_shock_map)))
  if (!"weight" %in% names(scenario_shock_map)) {
    scenario_shock_map$weight <- 1
  }
  if (is.null(path_fun) && is.null(single_shock_paths)) {
    stop("Provide either `path_fun` (preferred) or `single_shock_paths` for superposition fallback.")
  }
  if (!is.null(single_shock_paths)) {
    stopifnot(all(c("shock_id","regime","variable","t","value") %in% names(single_shock_paths)))
  }
  
  # --- restrict scenarios if requested
  scen_list <- sort(unique(scenario_shock_map$scenario))
  if (!is.null(scenarios)) {
    scen_list <- intersect(scen_list, scenarios)
  }
  
  # --- precompute singleton deltas for fallback
  if (is.null(path_fun)) {
    single_delta <- single_shock_paths %>%
      dplyr::left_join(baseline %>% dplyr::rename(baseline = value),
                       by = c("regime","variable","t")) %>%
      dplyr::mutate(delta = .data$value - dplyr::coalesce(.data$baseline, 0)) %>%
      dplyr::select(shock_id, regime, variable, t, delta)
  } else {
    single_delta <- NULL
  }
  
  # --- key builder for subset memoization
  .key_of_subset <- function(sub_df) {
    if (nrow(sub_df) == 0) return("∅")
    # stable order
    ord <- order(sub_df$shock_id)
    paste0(paste(sub_df$shock_id[ord], sub_df$weight[ord], sep = "@", collapse = "|"))
  }
  
  # --- generator of f(S): returns tibble(regime, variable, t, value)
  .gen_path <- function(scenario, subset_df, cache_env) {
    key <- paste0(scenario, "||", .key_of_subset(subset_df))
    if (exists(key, envir = cache_env, inherits = FALSE)) {
      return(get(key, envir = cache_env, inherits = FALSE))
    }
    if (!is.null(path_fun)) {
      path <- path_fun(scenario = scenario, subset_df = subset_df)
      # sanity
      miss <- setdiff(c("regime","variable","t","value"), names(path))
      if (length(miss)) stop(glue::glue("[shapley] path_fun missing columns: {paste(miss, collapse=', ')}"))
      path <- dplyr::arrange(path, .data$variable, .data$regime, .data$t)
      assign(key, path, envir = cache_env)
      return(path)
    }
    # superposition fallback: f(S) = baseline + Σ w_k * (single_k - baseline)
    if (nrow(subset_df) == 0) {
      path <- baseline
    } else {
      dsum <- subset_df %>%
        dplyr::inner_join(single_delta, by = "shock_id") %>%
        dplyr::mutate(weighted = .data$weight * .data$delta) %>%
        dplyr::group_by(.data$regime, .data$variable, .data$t) %>%
        dplyr::summarise(delta = sum(.data$weighted, na.rm = TRUE), .groups = "drop")
      path <- baseline %>%
        dplyr::left_join(dsum, by = c("regime","variable","t")) %>%
        dplyr::mutate(value = .data$value + dplyr::coalesce(.data$delta, 0)) %>%
        dplyr::select(.data$regime, .data$variable, .data$t, .data$value)
    }
    assign(key, path, envir = cache_env)
    path
  }
  
  # --- per-scenario Shapley
  .shapley_one_scenario <- function(scen) {
    submap <- scenario_shock_map %>% dplyr::filter(.data$scenario == scen)
    K <- nrow(submap)
    if (K == 0) return(NULL)
    
    # exact or MC?
    do_exact <- K <= exact_if_k_le
    perms <- if (do_exact) {
      # all permutations of shock_ids
      asplit(gtools::permutations(n = K, r = K, v = seq_len(K)), 1)
    } else {
      if (!is.null(seed)) set.seed(seed)
      # sample permutations as index lists
      purrr::map(seq_len(n_perm), ~ sample.int(K, size = K, replace = FALSE))
    }
    perms_used <- length(perms)
    method <- if (do_exact) "exact" else "mc"
    generator <- if (!is.null(path_fun)) "callback" else "superpose"
    
    # memoization cache for subset paths
    cache_env <- new.env(parent = emptyenv())
    
    # Baseline path and delta target for scenario (full set)
    full_path <- .gen_path(scen, submap[,c("shock_id","weight")], cache_env)
    base_path <- baseline
    delta_target <- full_path %>%
      dplyr::left_join(base_path %>% dplyr::rename(baseline = value),
                       by = c("regime","variable","t")) %>%
      dplyr::mutate(delta = .data$value - dplyr::coalesce(.data$baseline, 0)) %>%
      dplyr::select(.data$regime, .data$variable, .data$t, .data$delta)
    
    # accumulator of marginal contributions per shock
    acc <- vector("list", K)
    for (k in seq_len(K)) acc[[k]] <- NULL
    
    # iterate permutations
    for (p in perms) {
      # growing coalition S
      S_idx <- integer(0)
      S_df  <- tibble::tibble(shock_id = character(0), weight = numeric(0))
      prev_path <- .gen_path(scen, S_df, cache_env)
      
      for (pos in seq_len(K)) {
        idx <- p[[pos]]
        shock_row <- submap[idx, c("shock_id","weight")]
        # S ∪ {i}
        S_df2 <- dplyr::bind_rows(S_df, shock_row)
        new_path <- .gen_path(scen, S_df2, cache_env)
        
        # marginal contribution of i at this permutation step = f(S∪{i}) - f(S)
        mc <- new_path %>%
          dplyr::left_join(prev_path %>% dplyr::rename(prev = value),
                           by = c("regime","variable","t")) %>%
          dplyr::mutate(contrib = .data$value - .data$prev) %>%
          dplyr::select(.data$regime, .data$variable, .data$t, .data$contrib)
        
        mc$shock_id <- shock_row$shock_id
        acc[[idx]] <- dplyr::bind_rows(acc[[idx]], mc)
        
        # advance
        S_df <- S_df2
        prev_path <- new_path
      }
    }
    
    # average marginals per shock across permutations
    out_list <- purrr::imap(acc, function(tbl, idx) {
      if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
      tbl %>%
        dplyr::group_by(.data$shock_id, .data$regime, .data$variable, .data$t) %>%
        dplyr::summarise(shapley = mean(.data$contrib, na.rm = TRUE), .groups = "drop")
    })
    shap <- purrr::list_rbind(out_list)
    
    # attach scenario and metadata
    shap <- shap %>%
      dplyr::mutate(scenario = scen, method = method, generator = generator, perms_used = perms_used) %>%
      dplyr::select(.data$scenario, .data$shock_id, .data$regime, .data$variable, .data$t,
                    .data$shapley, .data$method, .data$generator, .data$perms_used)
    
    # Optional: sanity report (not returned) — how close Σφ to Δ
    # res_chk <- shap %>%
    #   dplyr::group_by(regime, variable, t) %>%
    #   dplyr::summarise(sum_phi = sum(shapley, na.rm = TRUE), .groups = "drop") %>%
    #   dplyr::inner_join(delta_target, by = c("regime","variable","t")) %>%
    #   dplyr::mutate(residual = delta - sum_phi)
    
    shap
  }
  
  # gtools is used only for exact permutations; provide a minimal fallback if absent
  if (!requireNamespace("gtools", quietly = TRUE)) {
    if (max(dplyr::count(scenario_shock_map, scenario)$n, na.rm = TRUE) <= exact_if_k_le) {
      stop("Package 'gtools' is required for exact permutations. Install it or lower 'exact_if_k_le'.")
    }
  }
  
  # run per scenario
  shap_all <- purrr::map(scen_list, .shapley_one_scenario)
  shap_all <- purrr::list_rbind(shap_all)
  
  shap_all
}