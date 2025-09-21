# Nonlinear SVAR — Submission Package

**Paper:** `paper/nonlinear_svar_draft.Rmd`  
**Student:** Simon Ochmann  
**Course:** REDACTED (MAEES 8.1)  
**Supervision:** Prof. Dr. Christian R. Proaño, Maybrit Wäch­ter, MSc

---

## Software environment

- **R**: 4.4.x (paper was built under 4.4.3)
- **LaTeX engine**: XeLaTeX (fallback LuaLaTeX)
- **R packages (CRAN)**: `bookdown`, `rmarkdown`, `knitr`, `fs`, `here`, `readr`, `dplyr`, `tidyr`, `ggplot2`, `kableExtra`, `scales`, `vars`
- Optional: `tinytex` (for TeX)

Install once in R:
```r
pkgs <- c("bookdown","rmarkdown","knitr","fs","here","readr","dplyr","tidyr","ggplot2","kableExtra","scales","vars")
need <- setdiff(pkgs, rownames(installed.packages()))
if (length(need)) install.packages(need)
