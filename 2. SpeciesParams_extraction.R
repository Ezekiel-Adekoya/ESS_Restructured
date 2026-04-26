# ── Species Parameters Extraction from FishBase ────────────────────────────────
library(rfishbase)
library(tidyverse)

# ── Species list ───────────────────────────────────────────────────────────────
scinames_fb <- c(
  "Gadus morhua",                  # Atlantic Cod
  "Melanogrammus aeglefinus",      # Haddock
  "Urophycis tenuis",              # White Hake
  "Merluccius bilinearis",         # Silver Hake
  "Pollachius virens",             # Pollock
  "Sebastes fasciatus",            # Redfishes Unseparated
  "Hippoglossoides platessoides",  # American Plaice
  "Myzopsetta ferruginea",         # Yellowtail Flounder
  "Clupea harengus",               # Atlantic Herring
  "Mallotus villosus",             # Capelin
  "Scomber scombrus",              # Atlantic Mackerel
  "Argentina silus",               # Atlantic Argentine
  "Amblyraja radiata",             # Thorny Skate
  "Ammodytes dubius",              # Sand Lance
  "Hippoglossus hippoglossus"      # Atlantic Halibut
)

# ── Lookup: FishBase name → survey SCINAME ─────────────────────────────────────
sciname_lookup <- tibble(
  Species = scinames_fb,
  SCINAME = c(
    "GADUS MORHUA",
    "MELANOGRAMMUS AEGLEFINUS",
    "UROPHYCIS TENUIS",
    "MERLUCCIUS BILINEARIS",
    "POLLACHIUS VIRENS",
    "SEBASTES FASCIATUS",
    "HIPPOGLOSSOIDES PLATESSOIDES",
    "LIMANDA FERRUGINEA",           # survey uses old name
    "CLUPEA HARENGUS",
    "MALLOTUS VILLOSUS",
    "SCOMBER SCOMBRUS",
    "ARGENTINA SILUS",
    "AMBLYRAJA RADIATA",
    "AMMODYTES DUBIUS",
    "HIPPOGLOSSUS HIPPOGLOSSUS"
  )
)

# ── Max length → max weight ────────────────────────────────────────────────────
length_weight <- rfishbase::estimate(scinames_fb) |>
  select(Species, a, b)

max_length <- rfishbase::species(scinames_fb) |>
  select(Species, Length)

max_weight <- full_join(max_length, length_weight) |>
  mutate(w_max = a * Length ^ b)

# ── Maturity (length and age at maturity) ─────────────────────────────────────
maturity_tbl <- rfishbase::maturity(scinames_fb) |>
  select(Species, l_mat = Lm, age_mat = tm, Locality) |>
  na.omit()

maturity <- maturity_tbl |>
  group_by(Species) |>
  summarise(l_mat = median(l_mat), age_mat = median(age_mat)) |>
  ungroup()

# ── Von Bertalanffy K ─────────────────────────────────────────────────────────
k_vb <- rfishbase::popgrowth(scinames_fb) |>
  select(Species, K) |>
  filter(!is.na(K)) |>
  group_by(Species) |>
  summarise(k_vb = median(K, na.rm = TRUE), .groups = "drop")

# ── Combine ───────────────────────────────────────────────────────────────────
sp <- max_weight |>
  left_join(maturity, by = "Species") |>
  left_join(k_vb, by = "Species") |>        # <- added
  mutate(w_mat = a * l_mat ^ b)

# ── Patch Sand Lance — separate query, same pattern as original ────────────────
ammodytes <- rfishbase::maturity("Ammodytes americanus") |>
  select(Species, l_mat = Lm, age_mat = tm, Locality)

ammodytes_l_mat <- median(ammodytes$l_mat, na.rm = TRUE)

sand_lance_row <- which(sp$Species == "Ammodytes dubius")
sp[sand_lance_row, "l_mat"]   <- ammodytes_l_mat
sp[sand_lance_row, "age_mat"] <- median(ammodytes$age_mat, na.rm = TRUE)
sp[sand_lance_row, "w_mat"]   <- sp[sand_lance_row, "a"] * ammodytes_l_mat ^ sp[sand_lance_row, "b"]
# ── Add survey SCINAME via lookup ──────────────────────────────────────────────
#sp <- sp |> left_join(sciname_lookup, by = "Species")
sp[sand_lance_row, "age_mat"] <- 1.5
# ── Check ──────────────────────────────────────────────────────────────────────
nrow(sp)  # should be 15
sp
sp <- sp |> left_join(sciname_lookup, by = "Species")
ppmr <- read.csv("Ess_data/predpreylengths.csv")
ppmr$SCINAME <- trimws(ppmr$SCINAME)

# join a and b from sp by SCINAME
ppmr <- ppmr |>
  left_join(sp |> select(SCINAME, a, b), by = "SCINAME") |>
  mutate(
    prey_wght = 0.01 * PYLEN ^ 3,
    pred_wght = a * PDLEN ^ b,
    ppmr      = pred_wght / prey_wght
  ) |>
  filter(PDLEN < 900) |>
  filter(PYNAM != "EMPTY") |>
  filter(!is.na(ppmr))

# check species coverage — should be 14 (no Capelin, added via proxy below)
cat("Species in ppmr:\n")
print(unique(ppmr$SCINAME))

# diagnostic plots
p1 <- ppmr |>
  ggplot(aes(x = PDLEN)) +
  geom_histogram(color = "#e9ecef", alpha = 0.6) +
  facet_wrap(~SCINAME, scales = "free") +
  theme_minimal()

p2 <- ppmr |>
  ggplot(aes(x = log10(ppmr))) +
  geom_histogram(color = "#e9ecef", alpha = 0.6) +
  facet_wrap(~SCINAME, scales = "free") +
  theme_minimal()

p3 <- ggplot(ppmr, aes(log10(pred_wght), log10(prey_wght))) +
  facet_wrap(~SCINAME) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  theme_minimal()

# compute betasig for 14 species with real data
betasig <- ppmr |>
  group_by(SCINAME) |>
  summarise(
    beta     = median(ppmr, na.rm = TRUE),
    sigma    = sd(log10(ppmr), na.rm = TRUE),
    min_ppmr = min(ppmr, na.rm = TRUE),
    max_ppmr = max(ppmr, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    beta_adj     = beta / 2,
    min_ppmr_adj = min_ppmr / 2
  )

# add capelin proxy using herring values
# Mallotus villosus has no stomach data in NOAA food habits database
# proxy: Atlantic Herring (both small pelagic forage fish)
capelin <- betasig |>
  filter(SCINAME == "CLUPEA HARENGUS") |>
  mutate(SCINAME = "MALLOTUS VILLOSUS")

betasig <- bind_rows(betasig, capelin)

# verify — should be 15
cat("betasig species:", nrow(betasig), "\n")
print(betasig$SCINAME)

# join betasig to sp by SCINAME
sp <- sp |> left_join(betasig, by = "SCINAME")

# w_min — 0.001 for teleosts; spiny dogfish uses l_min = 26 cm
sp$w_min <- 0.001
dogfish_row <- which(sp$SCINAME == "SQUALUS ACANTHIAS")
sp$w_min[dogfish_row] <- sp$a[dogfish_row] * 26 ^ sp$b[dogfish_row]
sp
# verify
cat("Final nrow(sp):", nrow(sp), "\n")  # should be 15
sp[, c("Species", "SCINAME", "beta", "sigma", "w_min")]

# I think min_ppmr_adj or min_ppmr probably best reflection of "preferred" ppmr; else could use a box predation kernel
write.csv(sp, "Ess_data/ESS_species_params.csv")

