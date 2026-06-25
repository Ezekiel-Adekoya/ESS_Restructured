## =============================================================================
## run_sd_sensitivity.R
## Robustness of the cod seal-attribution to the food-web perturbation magnitude.
## Reports the fraction of randomised webs where cod recovers MORE without the seal
## increase (counterfactual > factual 2024), at sd = 0.1 (primary) and sd = 0.2 (stress).
##
## Efficient: sd=0.1 is read from the cached canonical ensemble (seed=42, n=1000);
## only the sd=0.2 ensemble is computed fresh. ~15 min. Writes a small CSV summary
## (the full 0.2 sims are NOT saved — only the headline metric is needed).
## =============================================================================

suppressPackageStartupMessages({
  library(here); library(mizer); library(mizerExperimental); library(tidyverse)
})
setwd(here::here())
source("Ess_data/Useful_functions.R")   # run_paired_sims + randomize_interaction_matrix

params <- readRDS(if (file.exists("Ess_data/params_stage2_AUTO.RDS"))
                    "Ess_data/params_stage2_AUTO.RDS" else
                    "Ess_data/params_stage2_factual.RDS")
eff <- read.csv("Ess_data/effort_gear_with_seals.csv")
gear_cols <- c("Bottom_Trawl","Fixed_Gear","Small_Mesh","Pelagic","Seals")
effort_factual <- as.matrix(eff[eff$Year %in% 1978:2024, gear_cols])
rownames(effort_factual) <- eff$Year[eff$Year %in% 1978:2024]
effort_counter <- effort_factual
effort_counter[, "Seals"] <- effort_factual[1, "Seals"]

cutoff <- species_params(params)$biomass_cutoff; cutoff[is.na(cutoff)] <- 0
sppx   <- species_params(params)$species
jc     <- match("Atlantic Cod", sppx)
term_cod <- function(sim, yr = 2024) {
  n <- sim@n[as.character(yr), , ]
  sum(n[jc, params@w >= cutoff[jc]] * (params@w * params@dw)[params@w >= cutoff[jc]]) / 1e9
}

summarise_pair <- function(fac, cou, sd_val) {
  ok <- which(!vapply(fac, is.null, logical(1)) & !vapply(cou, is.null, logical(1)))
  cf  <- vapply(ok, function(k) term_cod(cou[[k]]), numeric(1))  # counterfactual cod 2024
  ff  <- vapply(ok, function(k) term_cod(fac[[k]]), numeric(1))  # factual cod 2024
  data.frame(sd = sd_val, n_webs = length(ok),
             pct_cod_recovers_more_without_seals = round(100 * mean(cf > ff), 1),
             median_logratio_counter_over_factual = round(median(log(cf / ff)), 3))
}

# sd = 0.1 from the cached canonical ensemble (== run_paired_sims sd=0.1, seed=42, n=1000)
f01 <- readRDS("Ess_Output/sims_factual.RDS")
c01 <- readRDS("Ess_Output/sims_counter.RDS")
r01 <- summarise_pair(f01, c01, 0.1)

# sd = 0.2 computed fresh (stronger structural-uncertainty stress test)
cat("Running sd = 0.2 ensemble (n = 1000, seed = 42)...\n")
pr  <- run_paired_sims(params, n_sims = 1000, sd = 0.2,
                       effort_factual = effort_factual,
                       effort_counter = effort_counter, seed = 42)
r02 <- summarise_pair(pr$factual, pr$counter, 0.2)

res <- rbind(r01, r02)
print(res)
write.csv(res, "Ess_Output/sd_sensitivity_cod.csv", row.names = FALSE)
cat("Saved Ess_Output/sd_sensitivity_cod.csv\n")
