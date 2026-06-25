## =============================================================================
## run_ensemble_sims.R
## Generate the n=1000 PAIRED randomised-web ensemble (factual vs counterfactual)
## that feeds 03_Scenarios_Through_time.Rmd and 04_Indicators.
##
## This is the EXPENSIVE step (~15 min). Run it deliberately — in RStudio or
## headless (`Rscript run_ensemble_sims.R`) — whenever `params` changes. It writes:
##   Ess_Output/sims_factual.RDS   (seal increase)
##   Ess_Output/sims_counter.RDS   (seals held constant at 1978 level)
## The 03 Rmd then reads those RDS via its cache guard and knits in seconds.
##
## Reproducible: run_paired_sims() is seed-locked (seed = 42), so results are
## bit-identical to an in-Rmd run. Function defs live in Ess_data/Useful_functions.R
## (single source of truth — do NOT redefine them here).
## =============================================================================

suppressPackageStartupMessages({
  library(here); library(mizer); library(mizerExperimental); library(tidyverse)
})
setwd(here::here())
source("Ess_data/Useful_functions.R")   # randomize_interaction_matrix + run_paired_sims

n_sims <- 1000; sd_web <- 0.1

## ── Factual model (Stage 2) ──────────────────────────────────────────────────
params <- readRDS(if (file.exists("Ess_data/params_stage2_AUTO.RDS"))
                    "Ess_data/params_stage2_AUTO.RDS" else
                    "Ess_data/params_stage2_factual.RDS")

## ── Effort: FACTUAL = seal increase 1978->2024; COUNTERFACTUAL = seals constant ─
eff <- read.csv("Ess_data/effort_gear_with_seals.csv")
gear_cols <- c("Bottom_Trawl","Fixed_Gear","Small_Mesh","Pelagic","Seals")
effort_factual <- as.matrix(eff[eff$Year %in% 1978:2024, gear_cols])
rownames(effort_factual) <- eff$Year[eff$Year %in% 1978:2024]

# Counterfactual: seal effort HELD CONSTANT at the 1978 level; fishing identical.
effort_counter <- effort_factual
effort_counter[, "Seals"] <- effort_factual[1, "Seals"]

## ── Run the paired ensemble ──────────────────────────────────────────────────
cat(sprintf("Running %d paired sims (sd_web = %.2g, seed = 42)...\n", n_sims, sd_web))
paired <- run_paired_sims(params, n_sims = n_sims, sd = sd_web,
                          effort_factual = effort_factual,
                          effort_counter = effort_counter, seed = 42)
sims_factual <- paired$factual
sims_counter <- paired$counter

# Keep only COMPLETE pairs (drop index i if either scenario failed) so the two lists
# stay aligned & indexable: factual[[k]] and counter[[k]] are the SAME web.
ok <- which(!vapply(sims_factual, is.null, logical(1)) &
            !vapply(sims_counter, is.null, logical(1)))
sims_factual <- sims_factual[ok]; sims_counter <- sims_counter[ok]
cat(sprintf("Kept %d / %d complete paired ensemble members\n", length(ok), n_sims))

## ── Save (read by 03's cache guard and by 04_Indicators) ─────────────────────
saveRDS(sims_factual, "Ess_Output/sims_factual.RDS")   # seal increase (factual)
saveRDS(sims_counter, "Ess_Output/sims_counter.RDS")   # seals constant (counterfactual)
cat("Saved Ess_Output/sims_factual.RDS + sims_counter.RDS\n")
