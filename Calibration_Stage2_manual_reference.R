## =============================================================================
## Calibration_Stage2_manual_reference.R
## STAGE 2 of the ESS calibration: time-series tuning → the "factual" model.
##
## Input : params_stage1_steady.RDS  (time-averaged 1978-85 steady state from
##         Stage 1, with the EMPIRICAL seal gear + biomass-weighted cutoffHigh
##         already baked in). Falls back to the current working base if absent.
## Output: params_stage2_factual.RDS + time-series diagnostic plots.
##
## Workflow philosophy (HIMI-style): Stage 1 anchors the reference equilibrium;
## Stage 2 projects through the historical effort and tunes the few processes
## that only reveal themselves dynamically. Cod is NOT re-spun — it is projected
## from the calibrated 1978-85 state so the collapse emerges as a transient.
##
## Mechanism captured: cod COLLAPSE = overfishing; NON-RECOVERY = grey-seal
## predation on juvenile cod + forage-fish predation on cod eggs/larvae
## (cultivation-depensation), both enabled by the forage release that follows
## groundfish overfishing.
## =============================================================================

options(device = function(...) pdf(file = tempfile(fileext = ".pdf"), ...))
suppressPackageStartupMessages({
  library(mizer); library(mizerExperimental); library(tidyverse)
})
source("Ess_data/Useful_functions.R")   # single project functions file
save_dir <- "Ess_Output/"

## ── Load Stage 1 steady state ────────────────────────────────────────────────
stage1_file <- "Ess_data/params_stage1_steady.RDS"
if (!file.exists(stage1_file)) {
  stage1_file <- "Ess_data/new_params_with_seal_interaction_working.RDS"
  message("params_stage1_steady.RDS not found — using working base: ", stage1_file)
}
p0 <- readRDS(stage1_file)
sp <- p0@species_params$species
w  <- p0@w; dw <- p0@dw

gear_cols <- c("Bottom_Trawl","Fixed_Gear","Small_Mesh","Pelagic","Seals")
fish_gears <- c("Bottom_Trawl","Fixed_Gear","Small_Mesh","Pelagic")
eff <- read.csv("Ess_data/effort_gear_with_seals.csv")
effort <- as.matrix(eff[eff$Year %in% 1978:2024, gear_cols])
rownames(effort) <- eff$Year[eff$Year %in% 1978:2024]
biomass_ts <- readRDS("Ess_data/biomass_ts.RDS")

## ── STAGE 2 TUNING TABLE (the only hand-set Stage-2 parameters) ──────────────
## All are multipliers on the Stage-1 calibrated values, with a one-line rationale.

# Fishing catchability multipliers — correct the steady-state under-estimate of
# the real (over)fishing F. Per-species because each stock has its own decline.
fish_mult <- c(
  "Atlantic Cod"        = 4.0,   # late-1980s overfishing → the collapse (FISHING-driven,
                                 # not seals: seals now arrive on the real logistic timing,
                                 # negligible during the 1988-94 collapse window)
  "Haddock"             = 1.8,
  "Pollock"             = 4,     # bistable: ×2.5 explodes (747), ×6 crashes (0) — seek midpoint
  "White Hake"          = 3.5,
  "Silver Hake"         = 0.5,   # base catchability was an artefact (≈50) → ease
  "Monkfish"            = 10,    # Left overshot (~250) ON PURPOSE — STRUCTURAL: Monkfish is the predator
                                 # biomass the model needs to balance herring, and herring (biomass AND
                                 # productivity/size-structure) is load-bearing for cod non-recovery via
                                 # cultivation. Any route to cut Monkfish either overshoots herring or
                                 # starves cod cultivation (cod recovers). Confirmed by experiment 2026-06.
  "Thorny Skate"        = 25,    # bycatch-vulnerable K-strategist
  "Winter Skate"        = 20
)

# Seal catchability multipliers (seal gear). NB: as of 2026-06 the seal gear is
# already SELECTIVE per Bowen (Stage 1 seal_gear_params.csv: Sand Lance & Cod high,
# Herring/Capelin low), so prey-sparing now lives in the gear, not here. The cod x3
# makes cod the binding species under the seal-F cap below, so cod juveniles take the
# full capped F (~non-recovery) while forage gets proportionally less.
seal_mult <- c(
  "Atlantic Cod"        = 3.0,   # predation on JUVENILE cod (w_opt~200g) → non-recovery
  # Demersal release: flatfish/sculpins/monkfish freed by fishing + cod collapse OVERSHOOT
  # because their seal catchability (0.005-0.015) came from the Bowen 1994 high-sand-lance
  # SNAPSHOT that under-represents benthic prey. Grey seals ARE benthic foragers; raise their
  # take of these so the GROWING logistic seal herd crops the release through the 1990s-2000s.
  # Kept below cod's effective level (0.1x3=0.3) so cod stays the binding species (F cap).
  "American Plaice"     = 14,    # 0.015 -> 0.21
  "Yellowtail Flounder" = 14,    # 0.012 -> 0.17
  "Witch Flounder"      = 8,     # 0.005 -> 0.04 (already on obs)
  "Longhorn Sculpin"    = 40,    # 0.005 -> 0.20: small benthic, obs ~1, needs hard cropping
  "Monkfish"            = 15     # 0.010 -> 0.15: DIRECT monkfish control that bypasses herring
)

# Forage -> groundfish interaction (cultivation: forage eat eggs/larvae).
# Per-species strength: COD high (this is the non-recovery lever); Silver Hake &
# Pollock LOW (uniform-strong cultivation crashed them to ~0 — they don't tolerate
# heavy recruit predation); gadoids/flatfish moderate (also damps their overshoots).
forage   <- c("Atlantic Herring","Sand Lance","Capelin")
cult_prey <- c(
  "Atlantic Cod"        = 0.9,   # hold cod down (non-recovery)
  "Haddock"             = 0.8,   # firm up: 0.5 left it at 153 (obs 38)
  "White Hake"          = 0.4,
  "Silver Hake"         = 0.33,  # raised from 0.1 (Pollock cap releases it); brackets obs 63 (0.4→44, 0.28→80)
  "Pollock"             = 0.15,  # stable high branch (×4 fish → 98); bounded by R_max cap below, not cult
  "Redfish Unseparated" = 0.25,
  "American Plaice"     = 0.25,
  "Yellowtail Flounder" = 0.25,
  "Witch Flounder"      = 0.25,
  "Red Hake"            = 0.25,
  "Monkfish"            = 0.25)

forage_Rmax_mult <- 8     # forage productivity (sand lance, capelin) to reach released levels
herring_Rmax_mult <- 8    # herring productivity (kept = forage default; the separate-reduction route
                          # to offset Monkfish removal weakens cod cultivation — see Monkfish note). TUNE.
thorny_Rmax_RDI  <- 1.2   # Thorny Skate finite R_max = RDI x this (K-strategist cap)

# Monkfish predation tuning (roots of its overshoot + herring coupling, 2026-06).
# Base matrix (inherited from Bundy): Monkfish->Herring 0.5 (guild-wide pattern,
# but high for a benthic AMBUSH predator) and Monkfish cannibalism 0.3 (flat
# default — too LOW: monkfish are strongly cannibalistic in reality). Raising
# cannibalism gives density-dependent self-control WITHOUT perturbing herring;
# trimming the herring link removes the food subsidy that inflates monkfish.
monk_cannibal      <- 0.6   # base 0.3; monkfish strongly cannibalistic empirically — raising to
                            # 0.6 lands herring exactly on obs (2220) & trims monkfish 310->250. TUNE.
monk_herring_avail <- 0.5   # base 0.5; KEPT — trimming it overshoots herring (monkfish is herring's
                            # control). Monkfish's residual overshoot is structural (see notes). TUNE.
pollock_Rmax_RDI <- 0.03  # Pollock is BISTABLE (~0 or ~100, no stable middle); a finite
                          # R_max = RDI x this caps the high branch toward obs ~7. TUNE.
redhake_Rmax_RDI <- 0.3   # Red Hake: low-productivity overshooter (68, obs 5) leaning on Monkfish
                          # predation. Finite R_max = RDI x this gives it its OWN control. TUNE.

## ── Apply tuning ─────────────────────────────────────────────────────────────
p <- p0
gp <- gear_params(p)
mult_gear <- function(gp, s, m, gears) {
  ix <- gp$species == s & gp$gear %in% gears
  gp$catchability[ix] <- gp$catchability[ix] * m; gp
}
for (s in names(fish_mult)) gp <- mult_gear(gp, s, fish_mult[s], fish_gears)
for (s in names(seal_mult)) gp <- mult_gear(gp, s, seal_mult[s], "Seals")
gear_params(p) <- gp

## ── Seal catchability → realistic PEAK F (KEEP the logistic abundance shape) ──
## Seals are a GEAR, so F = effort·catchability·sel is LINEAR in effort. The seal
## effort column (effort_gear_with_seals$Seals) is the DIGITISED LOGISTIC abundance
## (00e_digitise_seal_data.r: nls K/(1+exp(-r(t-t0))) scaled 0-1; 0.017 in 1978
## -> 0.998 in 2024). Do NOT clip it flat — that froze seal pressure at the 1985
## level and forced cultivation to carry non-recovery. Instead scale ALL seal
## catchabilities by ONE factor so realized PEAK F (at the 2024 logistic top) =
## F_peak_seal. F then GROWS along the full S-curve (~0.04 -> F_peak_seal), so the
## rising seal herd drives cod-juvenile non-recovery as hypothesised. Bowen
## selectivity ratios and the cod x3 (seal_mult) are preserved. TUNE F_peak_seal.
F_peak_seal  <- 4.0
seal_eff_max <- max(effort[, "Seals"])                       # 2024 logistic top
maxF_per_eff <- max(getFMortGear(p)["Seals", , ]) / initial_effort(p)["Seals"]
seal_q_scale <- as.numeric(F_peak_seal / (maxF_per_eff * seal_eff_max))
gp <- gear_params(p)
gp$catchability[gp$gear == "Seals"] <- gp$catchability[gp$gear == "Seals"] * seal_q_scale
gear_params(p) <- gp
cat(sprintf("Seal catchability x%.4g -> peak F (2024) = %.2f/yr; 1978 F = %.3f/yr (full logistic kept)\n",
            seal_q_scale, F_peak_seal, F_peak_seal * min(effort[, "Seals"]) / seal_eff_max))

inter <- p@interaction
for (s in names(cult_prey)) inter[forage, s] <- cult_prey[s]
inter["Monkfish", "Monkfish"]         <- monk_cannibal       # self-limiting control
inter["Monkfish", "Atlantic Herring"] <- monk_herring_avail  # trim herring subsidy
p <- setInteraction(p, inter)

for (s in forage) { i <- which(p@species_params$species == s)
  m <- if (s == "Atlantic Herring") herring_Rmax_mult else forage_Rmax_mult
  p@species_params$R_max[i] <- p@species_params$R_max[i] * m }
p <- setParams(p)
if (is.infinite(p@species_params$R_max[p@species_params$species == "Thorny Skate"]) ||
    TRUE) {
  p@species_params$R_max[p@species_params$species == "Thorny Skate"] <-
    getRDI(p)["Thorny Skate"] * thorny_Rmax_RDI
  p <- setParams(p)
}
{ # Pollock finite R_max cap (bistable species → bound the high branch to ~obs)
  p@species_params$R_max[p@species_params$species == "Pollock"] <-
    getRDI(p)["Pollock"] * pollock_Rmax_RDI
  p <- setParams(p)
}
{ # Red Hake finite R_max cap (give it its own control vs leaning on Monkfish)
  p@species_params$R_max[p@species_params$species == "Red Hake"] <-
    getRDI(p)["Red Hake"] * redhake_Rmax_RDI
  p <- setParams(p)
}

saveRDS(p, "Ess_data/params_stage2_factual.RDS")
cat("Saved Ess_data/params_stage2_factual.RDS\n\n")

## ── Seal F diagnostic (FULL logistic effort, NO clip) ────────────────────────
## Effort keeps the digitised logistic shape; catchability was scaled above so the
## 2024 top lands at F_peak_seal. Realized F per species GROWS along the S-curve.
peak_seal_F <- apply(getFMortGear(p)["Seals", , ], 1, max) /
               initial_effort(p)["Seals"] * max(effort[, "Seals"])
cat("Peak realized seal F per species (/yr) at 2024 logistic top:\n")
print(round(sort(peak_seal_F, decreasing = TRUE)[1:6], 2))

## ── Hindcast (NO re-spin: project from the calibrated 1978-85 state) ─────────
sim <- project(p, effort = effort, t_max = nrow(effort), t_save = 1,
               progress_bar = FALSE)
cl <- p@species_params$biomass_cutoff; cl[is.na(cl)] <- 0
ch <- p@species_params$biomass_cutoffHigh
yrs <- as.integer(dimnames(sim@n)[[1]]); nT <- length(yrs)

# model biomass: lower cutoff for all; ALSO apply cutoffHigh for Halibut only
# (its model w_max=470cm/1624kg exceeds the survey's max caught size).
bm <- matrix(NA, nT, nrow(p@species_params), dimnames = list(yrs, sp))
for (j in seq_len(nrow(p@species_params))) {
  hi <- if (sp[j] == "Atlantic Halibut" && !is.na(ch[j])) ch[j] else Inf
  idx <- w >= cl[j] & w <= hi
  bm[, j] <- rowSums(sweep(sim@n[, j, idx, drop = FALSE], 3, w[idx]*dw[idx], "*"))
}

## ── Diagnostics ─────────────────────────────────────────────────────────────
cat(sprintf("%-19s %8s %8s | %s\n", "Species", "1978", "2024", "obs2024"))
for (s in sp) {
  o <- biomass_ts$Biomass[biomass_ts$Species == s & biomass_ts$Year == 2024]
  cat(sprintf("%-19s %8.1f %8.1f | %.1f\n", s, bm["1978", s]/1e9,
              bm["2024", s]/1e9, ifelse(length(o), o/1e9, NA)))
}

# Cod mechanism summary
cod <- bm[, "Atlantic Cod"]/1e9
cat(sprintf("\nCod: 1978=%.0f  1994=%.0f  2024=%.0f kt (obs ~470, 64, 21) — collapse + non-recovery\n",
            cod["1978"], cod["1994"], cod["2024"]))

# Time-series plot (log10)
long <- as.data.frame(bm/1e9) |> rownames_to_column("Year") |>
  mutate(Year = as.integer(Year)) |>
  pivot_longer(-Year, names_to = "Species", values_to = "B")
g <- ggplot(long, aes(Year, B)) +
  geom_line(colour = "firebrick", linewidth = 0.7) +
  geom_point(data = filter(biomass_ts, Biomass > 0), aes(Year, Biomass/1e9),
             colour = "black", size = 0.6, inherit.aes = FALSE) +
  facet_wrap(~ Species, scales = "free_y") + scale_y_log10() +
  labs(title = "Stage 2 factual model — time series (log10)",
       subtitle = "black = DFO survey | red = model hindcast",
       y = "Biomass (kt, log10)") + theme_bw(base_size = 9)
ggsave(paste0(save_dir, "stage2_factual_trajectories.jpeg"), g,
       width = 14, height = 10, dpi = 200)
cat("\nSaved Ess_Output/stage2_factual_trajectories.jpeg\n")

# Time-series plot (linear / non-log)
g_lin <- ggplot(long, aes(Year, B)) +
  geom_line(colour = "firebrick", linewidth = 0.7) +
  geom_point(data = filter(biomass_ts, Biomass > 0), aes(Year, Biomass/1e9),
             colour = "black", size = 0.6, inherit.aes = FALSE) +
  facet_wrap(~ Species, scales = "free_y") +
  labs(title = "Stage 2 factual model — time series (linear)",
       subtitle = "black = DFO survey | red = model hindcast",
       y = "Biomass (kt)") + theme_bw(base_size = 9)
ggsave(paste0(save_dir, "stage2_factual_trajectories_linear.jpeg"), g_lin,
       width = 14, height = 10, dpi = 200)
cat("Saved Ess_Output/stage2_factual_trajectories_linear.jpeg\n")

## ── Emergent seal diet vs Bowen & Harrison (1994) — diagnostic check ─────────
ti <- which(yrs == 1992)   # Bowen sampling era
pe <- p; pe@initial_effort[] <- effort[ti, gear_cols]; pe@initial_n <- sim@n[ti,,]
pe <- setParams(pe)
fseal <- getFMortGear(pe)["Seals", , ]
cons  <- sapply(seq_along(sp), function(j) sum(fseal[j,]*sim@n[ti,j,]*w*dw))
diet_pct <- round(100*cons/sum(cons), 1); names(diet_pct) <- sp
bowen <- c("Sand Lance"=80.9,"Atlantic Cod"=11.0,"Silver Hake"=2.6,
           "American Plaice"=1.4,"Redfish Unseparated"=1.3,"Yellowtail Flounder"=1.1)
cat("\nEmergent seal diet 1992 (model% vs Bowen%) — informal check (Bowen is a snapshot):\n")
for (s in names(sort(diet_pct, decreasing = TRUE))[1:8])
  cat(sprintf("  %-19s %5.1f  (Bowen %s)\n", s, diet_pct[s],
              ifelse(s %in% names(bowen), bowen[s], "~0")))
