## Single source of truth for the per-species "consistently-sampled" size window used by ALL
## size-based indicator analyses (mean weight in 04; species/community size spectra in 03).
## A 2-cm length bin is retained for a species if it was caught in >= 75% of survey years;
## the window is the full span [min,max] of retained bins, converted to weight via a/b.
## The 3 species with no bin at 75% fall back to the fixed gear cutoffs [biomass_cutoff,
## biomass_cutoffHigh]. Model and observed are BOTH restricted to this window everywhere.
suppressMessages({library(mizer); library(dplyr)})
params <- readRDS("Ess_data/params_stage2_AUTO.RDS")
sp <- species_params(params)$species
a  <- species_params(params)$a; b <- species_params(params)$b
cl <- species_params(params)$biomass_cutoff;     cl[is.na(cl)] <- 0
ch <- species_params(params)$biomass_cutoffHigh; ch[is.na(ch)] <- Inf
lk <- c("COD(ATLANTIC)"="Atlantic Cod","HADDOCK"="Haddock","WHITE HAKE"="White Hake","SILVER HAKE"="Silver Hake","POLLOCK"="Pollock","REDFISH UNSEPARATED"="Redfish Unseparated","AMERICAN PLAICE"="American Plaice","YELLOWTAIL FLOUNDER"="Yellowtail Flounder","SPINY DOGFISH"="Spiny Dogfish","HALIBUT(ATLANTIC)"="Atlantic Halibut","SQUIRREL OR RED HAKE"="Red Hake","MONKFISH,GOOSEFISH,ANGLER"="Monkfish","NORTHERN SAND LANCE"="Sand Lance","WITCH FLOUNDER"="Witch Flounder","HERRING(ATLANTIC)"="Atlantic Herring","THORNY SKATE"="Thorny Skate","WINTER SKATE"="Winter Skate","LONGHORN SCULPIN"="Longhorn Sculpin","CAPELIN"="Capelin")
ab <- data.frame(Species = sp, a = a, b = b, cl = cl, ch = ch)
occ <- read.csv("Ess_data/LF_Biomass_Abund.csv") |>
  mutate(snum = suppressWarnings(as.numeric(as.character(STRAT)))) |>
  filter(!is.na(snum), snum >= 443, snum <= 466, YEAR >= 1978, YEAR <= 2024, Abundance_Strata > 0) |>
  mutate(Species = lk[COMMON_NAME],
         length_cm = ifelse(COMMON_NAME == "HERRING(ATLANTIC)", FLEN/10, FLEN)) |>
  filter(!is.na(Species)) |> mutate(bin = floor(length_cm/2)*2)
nyr <- dplyr::n_distinct(occ$YEAR)
win <- occ |> group_by(Species, bin) |> summarise(y = n_distinct(YEAR), .groups="drop") |>
  mutate(fr = y/nyr) |> group_by(Species) |>
  summarise(thr_used = if (any(fr >= 0.75)) "75%" else "fixed",
            Lmin = { z <- bin[fr >= 0.75]; if (length(z)) min(z)     else NA_real_ },
            Lmax = { z <- bin[fr >= 0.75]; if (length(z)) max(z) + 2 else NA_real_ },
            .groups="drop")
out <- ab |> left_join(win, by = "Species") |>
  mutate(wlo = ifelse(is.na(Lmin), cl, a * Lmin^b),
         whi = ifelse(is.na(Lmax), ch, a * Lmax^b),
         thr_used = ifelse(is.na(thr_used), "fixed", thr_used)) |>
  transmute(Species, thr_used, Lmin, Lmax, wlo = round(wlo, 2), whi = round(whi, 2),
            nyr_total = nyr, occ_threshold = 0.75)
write.csv(out, "Ess_data/size_windows.csv", row.names = FALSE)
print(out)

## ---- COMMUNITY-level window (used by 03 for the community size spectrum) --------------------
## Per-species windows would chop the community spectrum into jagged per-species segments, so
## for the community-level plot we instead use a SINGLE size range: the half-decade log weight
## classes the survey samples in >= 75% of years POOLED ACROSS ALL SPECIES. This is contiguous
## (the assemblage as a whole is caught across a broad size range every year) and trims only the
## sporadically-caught extremes (mainly the rare largest fish). Model and observed community
## spectra are BOTH restricted to this window, so they are still treated the same way.
breaks <- 10^seq(0, 6, 0.5)
occ_w <- occ |> left_join(ab |> select(Species, a, b), by = "Species") |>
  mutate(w = a * length_cm^b, bi = findInterval(w, breaks))
comm <- occ_w |> filter(bi >= 1, bi < length(breaks)) |>
  group_by(bi) |> summarise(fr = n_distinct(YEAR) / nyr, .groups = "drop")
keep <- comm$bi[comm$fr >= 0.75]
community <- data.frame(wlo = breaks[min(keep)], whi = breaks[max(keep) + 1],
                        occ_threshold = 0.75, nyr_total = nyr)
write.csv(community, "Ess_data/community_size_window.csv", row.names = FALSE)
cat(sprintf("Community size window (>=75%% of years, pooled): [%.1f, %.1f] g\n",
            community$wlo, community$whi))
