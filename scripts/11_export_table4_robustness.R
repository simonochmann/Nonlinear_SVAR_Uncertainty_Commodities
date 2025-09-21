# scripts/11_export_table4_robustness.R
suppressPackageStartupMessages({
  library(here); library(fs); library(readr); library(dplyr); library(tidyr); library(stringr); library(tibble)
})

MODELS_DIR <- here::here("models","tvar")
TAB_DIR    <- fs::path(MODELS_DIR, "artifacts","tables")
fs::dir_create(TAB_DIR)

# helpers 
read_if <- function(p) if (fs::file_exists(p)) readr::read_csv(p, show_col_types = FALSE) else NULL
latest_by <- function(df, key) df %>% arrange(desc(.data[[key]])) %>% slice(1)

# load artifacts 
FEVD  <- read_if(fs::path(TAB_DIR, "fevd_regime_shares.csv"))
HL    <- read_if(fs::path(TAB_DIR, "half_life_summary.csv"))
PERS  <- read_if(fs::path(TAB_DIR, "persistence_index_summary.csv"))
MSUM  <- read_if(fs::path(TAB_DIR, "model_selection_summary.csv"))

groups <- c("Energy","Industrial","Precious","Agriculture")

# ΔΔ-GFEVD at H=12, H=24 (Vol - JLN, High-Low) 
dd_row <- function(Hsel){
  if (is.null(FEVD) || !all(c("proxy","regime","group","horizon","share") %in% names(FEVD)))
    return(tibble(group = groups, value = NA_real_, H = Hsel))
  FE <- FEVD %>%
    mutate(proxy = case_when(tolower(proxy) %in% c("volatility","vxo_vix","vix","vxo") ~ "Volatility",
                             tolower(proxy) == "jln" ~ "JLN", TRUE ~ proxy),
           regime = toupper(regime))
  h <- FE$horizon[which.min(abs(FE$horizon - Hsel))]
  out <- lapply(groups, function(g){
    sub <- FE %>% filter(horizon == h, group == g, regime %in% c("H","L"), proxy %in% c("Volatility","JLN"))
    if (!nrow(sub)) return(tibble(group = g, value = NA_real_, H = h))
    HL_vol <- with(filter(sub, proxy=="Volatility"), share[regime=="H"] - share[regime=="L"])
    HL_jln <- with(filter(sub, proxy=="JLN"),        share[regime=="H"] - share[regime=="L"])
    tibble(group = g, value = as.numeric(HL_vol) - as.numeric(HL_jln), H = h)
  })
  bind_rows(out)
}
DD12 <- dd_row(12L) %>% rename(dd12 = value)
DD24 <- dd_row(24L) %>% rename(dd24 = value)

# Δ half-life (Vol - JLN) in high regime
dHL <- {
  if (is.null(HL) || !all(c("proxy","group","regime","half_life") %in% names(HL)))
    tibble(group = groups, dhl = NA_real_)
  else {
    HH <- HL %>%
      mutate(proxy = case_when(tolower(proxy) %in% c("volatility","vxo_vix","vix","vxo") ~ "Volatility",
                               tolower(proxy) == "jln" ~ "JLN", TRUE ~ proxy),
             regime = toupper(regime)) %>%
      filter(regime == "H")
    bind_rows(lapply(groups, function(g){
      v <- HH %>% filter(group==g, proxy=="Volatility") %>% pull(half_life)
      j <- HH %>% filter(group==g, proxy=="JLN")        %>% pull(half_life)
      tibble(group=g, dhl = if (length(v)&&length(j)) median(v,na.rm=TRUE)-median(j,na.rm=TRUE) else NA_real_)
    }))
  }
}

# Δ PI (Vol - JLN) at H=24 in high regime
dPI <- {
  if (is.null(PERS) || !all(c("proxy","group","regime","H","PI") %in% names(PERS)))
    tibble(group = groups, dpi = NA_real_)
  else {
    PP <- PERS %>%
      mutate(proxy = case_when(tolower(proxy) %in% c("volatility","vxo_vix","vix","vxo") ~ "Volatility",
                               tolower(proxy) == "jln" ~ "JLN", TRUE ~ proxy),
             regime = toupper(regime)) %>%
      filter(regime == "H")
    h <- if ("H" %in% names(PP)) { Hs <- sort(unique(PP$H)); Hs[which.min(abs(Hs-24L))] } else NA
    if (!is.na(h)) PP <- PP %>% filter(H == h)
    bind_rows(lapply(groups, function(g){
      v <- PP %>% filter(group==g, proxy=="Volatility") %>% pull(PI)
      j <- PP %>% filter(group==g, proxy=="JLN")        %>% pull(PI)
      tibble(group=g, dpi = if (length(v)&&length(j)) median(v,na.rm=TRUE)-median(j,na.rm=TRUE) else NA_real_)
    }))
  }
}

# Diagnostics (λ_max, LB p) and splice check
diag_symbol <- function(){
  if (is.null(MSUM)) return("–")
  
  MS <- MSUM
  # keep the latest per (window, proxy)
  if (!"run_at" %in% names(MS)) MS$run_at <- NA_character_
  MS <- MS %>%
    dplyr::arrange(window, proxy, dplyr::desc(run_at)) %>%
    dplyr::distinct(window, proxy, .keep_all = TRUE)
  
  # coerce diagnostics to numeric explicitly
  numify <- function(x) suppressWarnings(as.numeric(x))
  MS <- MS %>%
    dplyr::mutate(
      lambda_max_L = numify(lambda_max_L),
      lambda_max_H = numify(lambda_max_H),
      lb_p_L       = numify(lb_p_L),
      lb_p_H       = numify(lb_p_H)
    )
  
  vol_rows <- MS %>% dplyr::filter(tolower(proxy) %in% c("vxo_vix","vix","vol","vxo"))
  jln_rows <- MS %>% dplyr::filter(tolower(proxy) %in% c("jln"))
  
  assess <- function(rows){
    if (!nrow(rows)) return(list(pass = FALSE, any_stab = FALSE))
    lam <- c(rows$lambda_max_L, rows$lambda_max_H)
    any_stab <- all(is.finite(lam)) && all(lam < 1)
    lbv <- c(rows$lb_p_L, rows$lb_p_H)
    lb_ok <- length(lbv) > 0 && all(is.finite(lbv)) && min(lbv, na.rm = TRUE) >= 0.01
    list(pass = (any_stab && lb_ok), any_stab = any_stab)
  }
  
  av <- assess(vol_rows)
  aj <- assess(jln_rows)
  
  pass  <- av$pass  || aj$pass
  mixed <- (av$any_stab || aj$any_stab) && !pass
  
  if (pass) "✓" else if (mixed) "△" else "✗"
}
splice_symbol <- function(){
  if (is.null(MSUM)) return("–")
  vol <- MSUM %>% filter(proxy %in% c("vxo_vix","vix","VXO_VIX","vol"))
  # find two volatility rows: extension vs 1990-only
  ext <- vol %>% filter(grepl("1986.*2025", window)) %>% arrange(desc(run_at)) %>% slice(1)
  w90 <- vol %>% filter(grepl("1990", window))         %>% arrange(desc(run_at)) %>% slice(1)
  if (!nrow(ext) || !nrow(w90)) return("–")
  diff <- abs(as.numeric(ext$s_H) - as.numeric(w90$s_H))
  if (is.na(diff)) return("–")
  if (diff <= 0.02) "✓" else if (diff <= 0.05) "△" else "✗"
}

DIAG   <- diag_symbol()
SPLICE <- splice_symbol()

# score symbols per group 
score_dd   <- function(d12, d24) {        # sign + size consistency
  if (any(is.na(c(d12,d24)))) return("△")
  if (sign(d12)==sign(d24) && max(abs(d12),abs(d24)) >= 0.03) "✓"
  else if (max(abs(d12),abs(d24)) >= 0.01) "△" else "✗"
}
score_horz <- function(d12,d24){          # horizon stability
  if (any(is.na(c(d12,d24)))) return("△")
  if (sign(d12)==sign(d24)) {
    rel <- abs(d24 - d12) / (max(abs(d12),1e-6))
    if (rel <= 0.50) "✓" else "△"
  } else "✗"
}
score_dpi  <- function(x){ if (is.na(x)) "△" else if (abs(x) >= 0.15) "✓" else if (abs(x) >= 0.05) "△" else "✗" }
score_dhl  <- function(x){ if (is.na(x)) "△" else if (abs(x) >= 0.05) "✓" else if (abs(x) >= 0.02) "△" else "✗" }

ROB <- tibble::tibble(group = groups) %>%
  dplyr::left_join(DD12 %>% dplyr::select(group, dd12), by = "group") %>%
  dplyr::left_join(DD24 %>% dplyr::select(group, dd24), by = "group") %>%
  dplyr::left_join(dPI,  by = "group") %>%
  dplyr::left_join(dHL,  by = "group") %>%
  dplyr::mutate(
    `ΔΔ @12`  = mapply(score_dd,   dd12, dd24),
    Horizon   = mapply(score_horz, dd12, dd24),
    `Δ PI`    = sapply(dpi,  score_dpi),
    `Δ h½ (m)`= sapply(dhl,  score_dhl),
    Diagnostics = DIAG,
    Splice      = SPLICE
  ) %>%
  dplyr::rename(Group = group) %>%                    
  dplyr::select(Group, `ΔΔ @12`, Horizon, `Δ PI`, `Δ h½ (m)`, Diagnostics, Splice)

readr::write_csv(ROB, fs::path(TAB_DIR, "robustness_matrix_table4.csv"))

message("Wrote: ", fs::path(TAB_DIR, "robustness_matrix_table4.csv"))
