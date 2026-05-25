setwd("/Users/juliab6/UTAS Research Dropbox/Julia Blanchard/NEW DATA ESS/Untitled")

# Run New_get catches.R first (generates effort_matrix_by_gear.RDS)

# ── References ────────────────────────────────────────────────────────────────
#
# Seal abundance (digitised):
#   Hammill, M.O. & Stenson, G.B. (2007). Estimated prey consumption by harp
#   seals (Pagophilus groenlandicus), hooded seals (Cystophora cristata),
#   grey seals (Halichoerus grypus) and harbour seals (Phoca vitulina) in
#   Atlantic Canada. Journal of Northwest Atlantic Fishery Science, 35, 33-44.
#   [Figure showing Scotian Shelf seal abundance 1960-2021, mean and 95% CI]
#
# Gaussian size selectivity:
#   Bowen, W.D., Lawson, J.W. & Beck, B. (1993). Seasonal and geographic
#   variation in the species composition and size of prey consumed by grey
#   seals (Halichoerus grypus) on the Scotian Shelf. Canadian Journal of
#   Fisheries and Aquatic Sciences, 50(8), 1768-1778.
#   https://doi.org/10.1139/f93-198
#   [Optimal prey mass ~1.5 kg; lognormal selectivity sigma = 0.75]
#
# Vessel power standardisation:
#   Anticamara, J.A. et al. (2011). Global fishing effort (1950-2010): trends,
#   gaps, and implications. Fisheries Research, 107(1-3), 131-136.
#   https://doi.org/10.1016/j.fishres.2010.10.016
#   [Power (kW) = 11.26 * GT^0.71]

# ── Manually digitized seal abundance data ────────────────────────────────────
# Scotian Shelf grey seal abundance (thousands), 1960-2021.
# Mean and 95% CI digitised from Hammill & Stenson (2007).

seal_abundance <- data.frame(
  year = 1960:2021,

  # Scotian Shelf (red)
  scotian_mean = c(
     2,  2,  2,  2,  3,  3,  3,  4,  4,  5,  # 1960-1969
     5,  6,  8, 10, 12, 12, 13, 13, 14, 15,  # 1970-1979
    15, 16, 17, 16, 17, 18, 20, 23, 27, 32,  # 1980-1989
    38, 47, 57, 67, 76, 85, 112, 132, 130, 140,  # 1990-1999
   155, 170, 185, 200, 215, 225, 240, 255, 265, 272,  # 2000-2009
   280, 275, 290, 300, 295, 297, 300, 302, 305, 306, 305, 308  # 2010-2021
  ),
  scotian_upper = c(
     5,  5,  5,  5,  6,  7,  7,  8,  8,  9,
     9, 11, 13, 17, 20, 20, 22, 22, 23, 24,
    24, 25, 26, 25, 26, 27, 30, 35, 40, 48,
    57, 70, 85, 100, 112, 125, 165, 190, 185, 200,
   220, 240, 260, 280, 300, 315, 333, 340, 345, 350,
   335, 330, 340, 350, 345, 352, 355, 357, 360, 360, 360, 360
  ),
  scotian_lower = c(
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  1,  2,  3,  5,  5,  6,  6,  7,  8,
     8,  9, 10,  9, 10, 11, 12, 15, 18, 22,
    25, 32, 40, 48, 55, 62, 82, 100, 100, 108,
   115, 128, 140, 155, 165, 172, 182, 195, 205, 212,
   230, 228, 240, 252, 248, 250, 255, 258, 260, 262, 262, 265
  )
)

# Quick check plot
plot(seal_abundance$year, seal_abundance$scotian_mean, type = "l", col = "red",
     ylim = c(0, 370), xlab = "Year", ylab = "Abundance (thousands)",
     main = "Seal abundance - Scotian shelf")
lines(seal_abundance$year, seal_abundance$scotian_upper, col = "red", lty = 2)
lines(seal_abundance$year, seal_abundance$scotian_lower, col = "red", lty = 2)
legend("topleft", legend = "Scotian shelf", col = "red", lty = 1)

# ── Logistic curve fit and extrapolation to 2025 ─────────────────────────────
# Logistic: N(t) = K / (1 + exp(-r * (t - t0)))

logistic <- function(t, K, r, t0) K / (1 + exp(-r * (t - t0)))

start_vals <- list(K = 320, r = 0.2, t0 = 1995)

fit <- nls(scotian_mean ~ logistic(year, K, r, t0),
           data    = seal_abundance,
           start   = start_vals,
           control = nls.control(maxiter = 500))

summary(fit)

pred_years  <- data.frame(year = 1960:2025)
pred_values <- predict(fit, newdata = pred_years)

scotian_extrap <- data.frame(year = pred_years$year, scotian_predicted = pred_values)

# Plot with logistic fit and extrapolation
plot(seal_abundance$year, seal_abundance$scotian_mean, pch = 16, col = "red",
     xlim = c(1960, 2025), ylim = c(0, 370),
     xlab = "Year", ylab = "Abundance (thousands)",
     main = "Scotian shelf seal abundance - logistic fit")
lines(seal_abundance$year, seal_abundance$scotian_upper, col = "red", lty = 2)
lines(seal_abundance$year, seal_abundance$scotian_lower, col = "red", lty = 2)
lines(scotian_extrap$year, scotian_extrap$scotian_predicted, col = "darkred", lwd = 2)
abline(v = 2021, lty = 3, col = "grey40")
legend("topleft",
       legend = c("Digitised mean", "95% CI", "Logistic fit / extrapolation"),
       col    = c("red", "red", "darkred"),
       lty    = c(NA, 2, 1), pch = c(16, NA, NA))

# Save
write.csv(scotian_extrap, "scotian_shelf_logistic_1960_2025.csv", row.names = FALSE)

# ── Effort matrix: gear-based effort from new_get_catches + seals ────────────
# Load gear-based effort (Bottom_Trawl, Fixed_Gear, Pelagic, Small_Mesh)
# produced by New_get catches.R. Scale each gear 0-1 relative to its own max,
# then append seal abundance scaled to its full-series max (1960-2025).

effort_gear <- readRDS("Ess_data/effort_by_gear.rds")
effort_gear$Year <- as.integer(rownames(effort_gear))
rownames(effort_gear) <- NULL

gear_cols <- setdiff(colnames(effort_gear), "Year")
effort_gear_scaled <- effort_gear
effort_gear_scaled[gear_cols] <- lapply(
  effort_gear[gear_cols],
  function(x) x / max(x, na.rm = TRUE)
)

seal_scaled <- scotian_extrap[, c("year", "scotian_predicted")]
names(seal_scaled) <- c("Year", "Seals")
seal_scaled$Seals <- seal_scaled$Seals / max(seal_scaled$Seals)

effort_with_seals <- merge(effort_gear_scaled, seal_scaled,
                           by = "Year", all.x = TRUE)
effort_with_seals <- effort_with_seals[order(effort_with_seals$Year), ]


# ── Plot each gear and seals individually ─────────────────────────────────────
all_cols <- c(gear_cols, "Seals")
n_cols   <- length(all_cols)
n_row    <- ceiling(n_cols / 3)

par(mfrow = c(n_row, 3), mar = c(3, 3, 2, 1), mgp = c(2, 0.6, 0))
for (col in all_cols) {
  col_colour <- ifelse(col == "Seals", "steelblue", "grey30")
  plot(effort_with_seals$Year, effort_with_seals[[col]],
       type = "l", lwd = 1.5, col = col_colour,
       ylim = c(0, 1.05), xlab = "Year",
       ylab = "Relative effort (0-1)",
       main = gsub("_", " ", col))
}
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

saveRDS(effort_with_seals, "Ess_data/effort_gear_with_seals.RDS")
write.csv(effort_with_seals, "Ess_data/effort_gear_with_seals.csv",
          row.names = FALSE)

# ── Seal gear_params: Gaussian size selectivity (Bowen 1993) ─────────────────
# Seals modelled as a gear with lognormal selectivity by prey weight.
# w_opt = 1500 g (optimal prey mass) and sigma = 0.75 (log-scale width)
# from Bowen et al. (1993) grey seal diet data, Scotian Shelf.
# Catchability reflects species importance in grey seal diet
# (higher for pelagic/schooling prey, lower for large demersal species).
# NOTE: catchability values below are initial estimates based on diet
# composition from Bowen et al. (1993) and must be calibrated against
# observed seal diet data or biomass trends before use in simulations.

gaussian_sizeweight <- function(l, species_params, w_opt, sigma, ...) {
  w <- species_params$a * l ^ species_params$b
  exp(-0.5 * ((log(w) - log(w_opt)) / sigma)^2)
}


# Species names match selected_spp in New_PCA_DFO.R (18 species)
seal_gear_params <- data.frame(
  species = c(
    "Atlantic Cod",
    "Haddock",
    "Redfish Unseparated",
    "American Plaice",
    "Silver Hake",
    "Monkfish",
    "White Hake",
    "Thorny Skate",
    "Winter Skate",
    "Pollock",
    "Yellowtail Flounder",
    "Witch Flounder",
    "Longhorn Sculpin",
    "Red Hake",
    "Atlantic Herring",
    "Sand Lance",
    "Spiny Dogfish",
    "Atlantic Halibut",
    "Capelin"
  ),
  gear         = "Seals",
  sel_func     = "gaussian_sizeweight",
  w_opt        = 1500,  # g, Bowen et al. 1993
  sigma        = 0.75,  # log-scale SD, Bowen et al. 1993
  catchability = c(
    0.15,  # Atlantic Cod          – important demersal prey
    0.10,  # Haddock               – moderate demersal prey
    0.08,  # White Hake            – moderate prey
    0.08,  # Red Hake              – moderate prey
    0.10,  # Silver Hake           – moderate prey
    0.10,  # Pollock               – moderate prey
    0.08,  # Redfish Unseparated   – moderate prey
    0.15,  # American Plaice       – benthic, less accessible
    0.05,  # Yellowtail Flounder   – benthic, less accessible
    0.05,  # Witch Flounder        – benthic, less accessible
    0.45,  # Atlantic Herring      – major pelagic prey
    0.45,  # Capelin               – major pelagic prey
    0.05,  # Thorny Skate          – spiny, less preferred
    0.45,  # Winter Skate          – benthic, less preferred
    0.05,  # Spiny Dogfish         – spines, less preferred
    0.05,  # Monkfish              – large benthic, less accessible
    0.30,  # Sand Lance            – abundant schooling prey
    0.05,  # Longhorn Sculpin      – benthic, less accessible
    0.03   # Atlantic Halibut      – very large, rare prey
  )
)

# ── Plot Gaussian selectivity curves ─────────────────────────────────────────
w_seq <- exp(seq(log(0.1), log(1e6), length.out = 500))
sel   <- exp(-0.5 * ((log(w_seq) - log(1500)) / 0.75)^2)

n_sp  <- nrow(seal_gear_params)
n_row <- ceiling(n_sp / 4)

par(mfrow = c(n_row, 4), mar = c(3, 3, 2, 1), mgp = c(2, 0.6, 0))
for (i in seq_len(n_sp)) {
  plot(w_seq, sel, type = "l", lwd = 1.5, col = "steelblue",
       log = "x", ylim = c(0, 1.05),
       xlab = "Prey weight (g)", ylab = "Selectivity",
       main = seal_gear_params$species[i])
  abline(v = 1500, lty = 2, col = "grey50")
}
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

saveRDS(seal_gear_params, "Ess_data/seal_gear_params.RDS")

# ── Catchability calibration approach ────────────────────────────────────────
# POST-HOC STEP: seal catchabilities should only be calibrated after the
# fishing-gear steady state has been established (species biomasses and
# fishing mortalities already tuned). Once the steady state is fixed, adjust
# catchabilities so that seal consumption at the reference seal abundance
# matches the 12x body mass consumption target below. The scaled seal effort
# time series (effort_gear_with_seals$Seals) then drives how consumption
# changes relative to that calibrated steady state.
#
# Steps:
#   1. Calculate total target consumption (g/yr) at a reference year
#   2. Distribute across species using diet proportions from Bowen (1993)
#   3. Back-calculate species-level catchability from model/survey biomass:
#      divide target species consumption by model biomass and scaled seal
#      effort (0-1) at the reference year
#   4. Run the model, extract getYield() for the Seals gear, compare to
#      target; scale all catchabilities by the ratio until they match
#
# Diet proportions (Bowen et al. 1993, Scotian Shelf, approximate):
diet_prop <- c(
  0.08,  # Atlantic Cod
  0.05,  # Haddock
  0.03,  # White Hake
  0.03,  # Red Hake
  0.05,  # Silver Hake
  0.05,  # Pollock
  0.03,  # Redfish Unseparated
  0.02,  # American Plaice
  0.02,  # Yellowtail Flounder
  0.02,  # Witch Flounder
  0.20,  # Atlantic Herring
  0.13,  # Capelin
  0.01,  # Thorny Skate
  0.01,  # Winter Skate
  0.01,  # Spiny Dogfish
  0.01,  # Monkfish
  0.25,  # Sand Lance
  0.01,  # Longhorn Sculpin
  0.01   # Atlantic Halibut
)
# Grey seal parameters
seal_body_mass_kg   <- 170    # mean adult body mass (kg)
consumption_mult    <- 12     # annual consumption as multiple of body mass
fish_fraction       <- 0.85   # proportion of diet that is fish

# Reference year total consumption (g/yr) — use mean abundance 1990-2010
ref_abundance       <- mean(
  scotian_extrap$scotian_predicted[
    scotian_extrap$year >= 1990 & scotian_extrap$year <= 2010
  ]
) * 1000  # convert thousands to individuals

target_consumption_g <- ref_abundance *
  seal_body_mass_kg * 1000 *  # body mass in grams
  consumption_mult *
  fish_fraction

cat("Target total fish consumption at reference abundance:\n")
cat(format(round(target_consumption_g / 1e9), big.mark = ","),
    "x 10^9 g/yr =",
    round(target_consumption_g / 1e6), "tonnes/yr\n\n")

cat("Target consumption by species (tonnes/yr):\n")
target_by_species <- sort(
  round(diet_prop * target_consumption_g / 1e6), decreasing = TRUE
)
print(target_by_species)

# To use in mizer calibration:
# after running the model to steady state, extract biomass per species,
# then divide target species consumption by biomass and scaled seal effort
# at the reference year (effort_gear_with_seals$Seals).