# ══════════════════════════════════════════════════════════════════════════════
# MIZER INTERACTION MATRIX — adjusted from Cook & Bundy (2010) Table 4.1.1
# Diet % → rescaled to [0.05, 0.75] interaction values
# 12 species have direct diet data; 7 retain guild heuristics (flagged)
# ══════════════════════════════════════════════════════════════════════════════

sp_order <- species_params(params)$species

inter <- matrix(
  0.2,
  nrow     = length(sp_order),
  ncol     = length(sp_order),
  dimnames = list(sp_order, sp_order))
diag(inter) <- 0.5

# ── Guild vectors (unchanged) ─────────────────────────────────────────────────
pelagics     <- c("Atlantic Herring", "Capelin", "Silver Hake", "Red Hake", "Sand Lance")
mid_water    <- c("White Hake", "Pollock", "Redfish Unseparated")
benthivores  <- c("Haddock", "American Plaice", "Yellowtail Flounder",
                  "Witch Flounder", "Longhorn Sculpin")
piscivores   <- c("Atlantic Cod", "Spiny Dogfish", "White Hake", "Monkfish")
large_elasmo <- c("Thorny Skate", "Winter Skate", "Atlantic Halibut")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION A — values derived directly from Table 4.1.1 (Cook & Bundy 2010)
# Rescaling rule: inter = 0.05 + 0.70 * (pct / max_pct_for_that_predator)
# where max_pct is the highest single fish-family % in that predator's diet
# ══════════════════════════════════════════════════════════════════════════════

# ── Atlantic Cod (max fish prey = Clupeidae 18.31%) ───────────────────────────
# Clupeidae 18.31 → Herring;  Ammodytidae 15.41 → Sand Lance
# Gadidae 3.65 → Haddock/White Hake/Pollock;  Merlucciidae 3.85 → Silver Hake
# Osmeridae 2.6 → Capelin;  Pleuronectidae 2.18 → flatfish
inter["Atlantic Cod", "Atlantic Herring"]    <- 0.75   # Clupeidae 18.31% — dominant
inter["Atlantic Cod", "Sand Lance"]          <- 0.64   # Ammodytidae 15.41%
inter["Atlantic Cod", "Silver Hake"]         <- 0.20   # Merlucciidae 3.85%
inter["Atlantic Cod", "Haddock"]             <- 0.19   # Gadidae 3.65% (shared)
inter["Atlantic Cod", "White Hake"]          <- 0.19   # Gadidae 3.65% (shared)
inter["Atlantic Cod", "Pollock"]             <- 0.19   # Gadidae 3.65% (shared)
inter["Atlantic Cod", "Capelin"]             <- 0.15   # Osmeridae 2.60%
inter["Atlantic Cod", "American Plaice"]     <- 0.13   # Pleuronectidae 2.18%
inter["Atlantic Cod", "Yellowtail Flounder"] <- 0.13   # Pleuronectidae 2.18%
inter["Atlantic Cod", "Witch Flounder"]      <- 0.13   # Pleuronectidae 2.18%
inter["Atlantic Cod", "Redfish Unseparated"] <- 0.10   # Scorpaenidae 1.22%

# ── White Hake (max fish prey = Clupeidae 34.46%) ────────────────────────────
# Clupeidae 34.46 → Herring;  Gadidae 23.47 → Haddock;
# Merlucciidae 9.83 → Silver Hake;  Ammodytidae 0.16 → Sand Lance (very low)
# Pleuronectidae 1.59 → flatfish
inter["White Hake", "Atlantic Herring"]    <- 0.75   # Clupeidae 34.46% — dominant
inter["White Hake", "Haddock"]            <- 0.53   # Gadidae 23.47%
inter["White Hake", "Silver Hake"]        <- 0.25   # Merlucciidae 9.83%
inter["White Hake", "American Plaice"]    <- 0.08   # Pleuronectidae 1.59%
inter["White Hake", "Yellowtail Flounder"]<- 0.08   # Pleuronectidae 1.59%
inter["White Hake", "Sand Lance"]         <- 0.05   # Ammodytidae 0.16% — very low
inter["White Hake", "Redfish Unseparated"]<- 0.10   # stability cap (unchanged)

# ── Silver Hake (max fish prey = Clupeidae 23.40%) ───────────────────────────
# Clupeidae 23.40 → Herring;  Merlucciidae 18.05 → Silver Hake (cannibalism)
# Ammodytidae 7.93 → Sand Lance;  Gadidae 4.69 → gadids
inter["Silver Hake", "Atlantic Herring"]   <- 0.75   # Clupeidae 23.40% — dominant
inter["Silver Hake", "Silver Hake"]        <- 0.59   # Merlucciidae 18.05% (cannibalism)
inter["Silver Hake", "Sand Lance"]         <- 0.29   # Ammodytidae 7.93%
inter["Silver Hake", "Haddock"]            <- 0.19   # Gadidae 4.69%
inter["Silver Hake", "White Hake"]         <- 0.19   # Gadidae 4.69%

# ── Haddock (max fish prey = Ammodytidae 3.44%) ──────────────────────────────
# Mostly invertivorous; Ammodytidae 3.44 → Sand Lance;
# Gadidae 48.49% is Haddock eating gadids — mostly juvenile gadids / fish larvae
# Clupeidae 0.07 → very low herring predation
inter["Haddock", "Sand Lance"]         <- 0.30   # Ammodytidae 3.44% — main fish prey
inter["Haddock", "Atlantic Herring"]   <- 0.07   # Clupeidae 0.07% — minimal
inter["Haddock", "Capelin"]            <- 0.07   # Osmeridae trace

# ── American Plaice (max fish prey = Ammodytidae 24.17%) ─────────────────────
# Ammodytidae 24.17 → Sand Lance dominant;  Osmeridae 7.56 → Capelin
# Merlucciidae 1.41 → Silver/White Hake trace
inter["American Plaice", "Sand Lance"]      <- 0.75   # Ammodytidae 24.17% — dominant
inter["American Plaice", "Capelin"]         <- 0.27   # Osmeridae 7.56%
inter["American Plaice", "Silver Hake"]     <- 0.09   # Merlucciidae 1.41%

# ── Thorny Skate (max fish prey = Pleuronectidae 19.48%) ─────────────────────
# Pleuronectidae 19.48 → flatfish;  Ammodytidae 18.74 → Sand Lance (nearly equal)
# Merlucciidae 0.62 → hake trace
inter["Thorny Skate", "American Plaice"]     <- 0.75   # Pleuronectidae 19.48%
inter["Thorny Skate", "Yellowtail Flounder"] <- 0.75   # Pleuronectidae 19.48%
inter["Thorny Skate", "Witch Flounder"]      <- 0.75   # Pleuronectidae 19.48%
inter["Thorny Skate", "Sand Lance"]          <- 0.73   # Ammodytidae 18.74% (nearly equal)
inter["Thorny Skate", "Silver Hake"]         <- 0.07   # Merlucciidae 0.62%
inter["Thorny Skate", "Winter Skate"]        <- 0.10   # skate-on-skate (unchanged)

# ── Yellowtail Flounder (max fish prey = Ammodytidae 28.50%) ─────────────────
# Ammodytidae 28.50 → Sand Lance dominant; very low other fish
inter["Yellowtail Flounder", "Sand Lance"]      <- 0.75   # Ammodytidae 28.50%
inter["Yellowtail Flounder", "Atlantic Herring"]<- 0.06   # Clupeidae 0.05%
inter["Yellowtail Flounder", "Capelin"]         <- 0.06   # Osmeridae 0.05%

# ── Witch Flounder (max fish prey = Ammodytidae 0.69%) ───────────────────────
# Almost entirely invertivorous (Annelia 25.52% dominant prey)
# Very low fish predation across all species
inter["Witch Flounder", "Sand Lance"]      <- 0.10   # Ammodytidae 0.69%
inter["Witch Flounder", "Capelin"]         <- 0.07   # Osmeridae trace

# ── Atlantic Herring (max fish prey = Fish_larvae <0.01%) ────────────────────
# Almost entirely zooplankton (Euphausiidae 41.39%, Decapoda 26.6%)
# Effectively no fish predation — set all fish interactions very low
inter["Atlantic Herring", pelagics]    <- 0.05
inter["Atlantic Herring", mid_water]   <- 0.05
inter["Atlantic Herring", benthivores] <- 0.05
inter["Atlantic Herring", piscivores]  <- 0.05
inter["Atlantic Herring", large_elasmo]<- 0.05

# ── Capelin (max fish prey = Osmeridae <0.01%) ───────────────────────────────
# Diet: Euphausiidae 26.91%, Decapoda 20.67% — zooplankton dominant
# No meaningful fish predation
inter["Capelin", pelagics]    <- 0.05
inter["Capelin", mid_water]   <- 0.05
inter["Capelin", benthivores] <- 0.05
inter["Capelin", piscivores]  <- 0.05
inter["Capelin", large_elasmo]<- 0.05

# ── Sand Lance (max fish prey = Fish_eggs 0.60%) ─────────────────────────────
# Diet: Copepoda 12.51%, Euphausiidae 24.38%, Decapoda 23.27% — zooplankton
# Minimal fish predation
inter["Sand Lance", pelagics]    <- 0.05
inter["Sand Lance", mid_water]   <- 0.05
inter["Sand Lance", benthivores] <- 0.05
inter["Sand Lance", piscivores]  <- 0.05
inter["Sand Lance", large_elasmo]<- 0.05

# ══════════════════════════════════════════════════════════════════════════════
# SECTION B — guild heuristics for species NOT in Table 4.1.1
# Flagged explicitly: Pollock, Redfish, Monkfish, Spiny Dogfish,
#                     Red Hake, Winter Skate, Atlantic Halibut, Longhorn Sculpin
# ══════════════════════════════════════════════════════════════════════════════

# Pollock — pelagic-leaning gadid, known herring/capelin predator (literature)
inter["Pollock", "Atlantic Herring"] <- 0.55   # heuristic — Pollock diet literature
inter["Pollock", "Capelin"]          <- 0.45   # heuristic
inter["Pollock", "Sand Lance"]       <- 0.35   # heuristic
inter["Pollock", "Silver Hake"]      <- 0.20   # heuristic

# Red Hake — similar ecology to White Hake but smaller/more benthic
inter["Red Hake", "Atlantic Herring"] <- 0.40   # heuristic
inter["Red Hake", "Sand Lance"]       <- 0.25   # heuristic
inter["Red Hake", "Silver Hake"]      <- 0.20   # heuristic

# Redfish Unseparated — deep-water, largely invertivorous, some fish
inter["Redfish Unseparated", "Atlantic Herring"] <- 0.20   # heuristic
inter["Redfish Unseparated", "Capelin"]          <- 0.20   # heuristic
inter["Redfish Unseparated", "Sand Lance"]       <- 0.10   # heuristic

# Monkfish — ambush piscivore, broad diet
inter["Monkfish", "Atlantic Herring"]    <- 0.50   # heuristic
inter["Monkfish", "Sand Lance"]          <- 0.45   # heuristic
inter["Monkfish", "American Plaice"]     <- 0.35   # heuristic
inter["Monkfish", "Haddock"]             <- 0.30   # heuristic
inter["Monkfish", "Atlantic Cod"]        <- 0.20   # heuristic
inter["Monkfish", "Redfish Unseparated"] <- 0.10   # stability cap

# Spiny Dogfish — broad piscivore, herring/sand lance dominant (literature)
inter["Spiny Dogfish", "Atlantic Herring"]    <- 0.55   # heuristic
inter["Spiny Dogfish", "Sand Lance"]          <- 0.45   # heuristic
inter["Spiny Dogfish", "Capelin"]             <- 0.30   # heuristic
inter["Spiny Dogfish", "Silver Hake"]         <- 0.25   # heuristic
inter["Spiny Dogfish", "Spiny Dogfish"]       <- 0.05   # stability

# Atlantic Halibut — large ambush predator (no data in table)
inter["Atlantic Halibut", "Atlantic Herring"]    <- 0.50   # heuristic
inter["Atlantic Halibut", "American Plaice"]     <- 0.50   # heuristic
inter["Atlantic Halibut", "Yellowtail Flounder"] <- 0.50   # heuristic
inter["Atlantic Halibut", "Sand Lance"]          <- 0.40   # heuristic
inter["Atlantic Halibut", "Haddock"]             <- 0.30   # heuristic

# Winter Skate — similar to Thorny Skate but smaller, more invertivorous
inter["Winter Skate", "Sand Lance"]          <- 0.45   # heuristic (cf. Thorny)
inter["Winter Skate", "American Plaice"]     <- 0.30   # heuristic
inter["Winter Skate", "Atlantic Herring"]    <- 0.20   # heuristic
inter["Winter Skate", "Thorny Skate"]        <- 0.10   # skate-on-skate

# Longhorn Sculpin — benthic, invertivorous with some small fish
inter["Longhorn Sculpin", "Sand Lance"]      <- 0.25   # heuristic
inter["Longhorn Sculpin", "Capelin"]         <- 0.15   # heuristic
inter["Longhorn Sculpin", "Atlantic Herring"]<- 0.10   # heuristic

# ══════════════════════════════════════════════════════════════════════════════
# SECTION C — stability adjustments (unchanged from previous version)
# ══════════════════════════════════════════════════════════════════════════════
inter[, "Spiny Dogfish"]       <- inter[, "Spiny Dogfish"]       * 0.10
inter[, "American Plaice"]     <- inter[, "American Plaice"]     * 0.60
inter[, "Redfish Unseparated"] <- inter[, "Redfish Unseparated"] * 0.50
inter["Spiny Dogfish", "Spiny Dogfish"] <- 0.05   # restore diagonal

# ── Save interactio ─────────────────────────────────────────────────────────────────────
saveRDS(inter, file = "inter.rds")
params <- setInteraction(params, interaction = inter)
