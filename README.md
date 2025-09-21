### TEMP-TEST So 21 Sep 2025 23:17:22 CEST

# Nonlinear SVAR: The Effects of Macroeconomic Uncertainty on Commodity Price Volatility

This project revisits and extends the nonlinear macro-financial framework of Joëts et al. (2017) to analyze how uncertainty shocks impact commodity price volatility across different regimes. It was developed as a master's-level research paper for the course *REDACTED* at the REDACTED.

## Research Focus

- **Title**: *The Nonlinear Effects of Macroeconomic Uncertainty on Commodity Price Volatility: A Replication and Extension of Joëts et al. (2017)*
- **Author**: Simon Ochmann  
- **Supervisor**: Prof. Dr. Christian R. Proaño  
- **Course**: MAEES8.1 – Advanced Time Series Methods  
- **University**: Otto-Friedrich-Universität Bamberg

## 📈 Project Summary

This study replicates the original Threshold VAR (TVAR) model from Joëts et al. (2017) and introduces three key extensions:

1. **Sample Extension**: Updates the dataset through April 2025 to incorporate recent macro-financial shocks (e.g., COVID-19, 2022–2024 inflation cycle).
2. **Uncertainty Proxies**: Incorporates modern uncertainty measures:
   - CBOE Volatility Index (VIX)
   - Jurado-Ludvigson-Ng Macroeconomic Uncertainty Index (JLN)
   - ECB Composite Indicator of Systemic Stress (CISS)
3. **Model Enhancements**: Benchmarks the nonlinear TVAR model against linear VAR and SVAR baselines, tests multiple lag/threshold specifications, and simulates regime-contingent impulse responses.

## Repository Structure

Nonlinear_SVAR_Uncertainty_Commodities/
├── data/ # Raw and cleaned commodity & uncertainty data
├── functions/ # Modular R functions for preprocessing, modeling, GIRFs
├── scripts/ # Replication, extension, and validation scripts
├── output/ # Saved figures, tables, and model outputs
├── report/ # Final paper (.Rmd, .pdf)
├── LICENSE # MIT License
└── README.md # This file

## Core Packages

- `tsDyn` – Threshold VAR estimation
- `tvarGIRF` – Generalized impulse response simulation
- `vars`, `svars` – VAR/SVAR estimation and diagnostics
- `ggplot2` – Visualization

## Methodology

- Threshold VAR (TVAR) estimation with regime switching
- Generalized impulse responses (GIRFs) conditional on high/low uncertainty regimes
- Structural VAR (SVAR) with Cholesky identification
- Robustness checks: lag length, threshold trimming, delay parameter, and proxy comparisons

## Validation

Linear and structural models are benchmarked against the nonlinear TVAR to assess:
- Regime asymmetry in volatility transmission
- Tail risk amplification under uncertainty
- Predictive robustness across proxies (VIX, JLN, CISS)

## Reproducibility

All code is fully modularized and implemented in R. The repository is organized for reproducibility, transparency, and future extension by researchers or practitioners. This repository is suitable for:
- PhD macro-finance replication
- IMF/BIS/ECB stress testing toolkits
- Hedge fund macro risk analysis

## Reference

Joëts, M., Mignon, V., & Razafindrabe, T. (2017). *Does the volatility of commodity prices reflect macroeconomic uncertainty?* Energy Economics, 68, 313–326.

---

*Last updated: July 2025*
