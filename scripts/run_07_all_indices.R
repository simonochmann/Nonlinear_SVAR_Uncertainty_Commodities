suppressPackageStartupMessages({ library(yaml); library(fs); library(here) })
cfg_path <- here("config/paths.yml")
orig <- yaml::read_yaml(cfg_path)

for (u in c("vix","vxo","jln")) {
  cfg <- yaml::read_yaml(cfg_path)
  cfg$uncertainty$active <- u
  yaml::write_yaml(cfg, cfg_path)
  
  message("\n=== 07 with active = ", u, " ===")
  status <- system("Rscript scripts/07_shock_scenario_simulations.R")
  if (status != 0) stop("07 failed for active=", u)
  
  # stash outputs to avoid overwrites, then recreate dirs
  safe_move <- function(src, dst){ if (dir.exists(src)) { if (dir.exists(dst)) fs::dir_delete(dst); fs::dir_move(src, dst); dir.create(src, recursive = TRUE) } }
  safe_move("data/scenarios",    sprintf("data/scenarios_%s", u))
  safe_move("figures/scenarios", sprintf("figures/scenarios_%s", u))
  safe_move("logs/scenarios",    sprintf("logs/scenarios_%s", u))
}

yaml::write_yaml(orig, cfg_path)   # restore original active
message("All done.")
