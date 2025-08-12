source("scripts/setup.R")

source(here("functions/tvar/regime/compute_regime_path.R"))
source(here("functions/tvar/regime/compute_regime_durations.R"))

tvar_model <- readRDS(here("models/tvar/vix_tvar_model_20250807_114802.rds"))

compute_regime_path(tvar_model, inject = TRUE, verbose = TRUE)
regime_durations <- compute_regime_durations(model = tvar_model)

model_path <- here("models/tvar/vix_tvar_model_20250807_114802.rds")
tvar_model <- readRDS(model_path)

regime_durations <- compute_regime_durations(
  model = tvar_model,
  verbose = TRUE
)