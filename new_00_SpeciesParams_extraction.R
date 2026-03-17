# SpeciesParams_extraction from fishbase
library(rfishbase)
library(tidyverse)

scinames_fb <- c(
  "Ammodytes americanus",
  "Clupea harengus",
  "Gadus morhua",
  "Glyptocephalus cynoglossus",
  "Hippoglossoides platessoides",
  "Hippoglossus hippoglossus",
  "Myzopsetta ferruginea",
  "Melanogrammus aeglefinus",
  "Merluccius bilinearis",
  "Pollachius virens",
  "Reinhardtius hippoglossoides",
  "Scomber scombrus",
  "Sebastes fasciatus",
  "Squalus acanthias",
  "Urophycis tenuis",
  "Mallotus villosus"
)

# Lookup table — ties SCINAME to Species by name, not row position
sciname_lookup <- tibble(
  Species = scinames_fb,
  SCINAME = c(
    "AMMODYTES DUBIUS",
    "CLUPEA HARENGUS",
    "GADUS MORHUA",
    "GLYPTOCEPHALUS CYNOGLOSSUS",
    "HIPPOGLOSSOIDES PLATESSOIDES",
    "HIPPOGLOSSUS HIPPOGLOSSUS",
    "LIMANDA FERRUGINEA",
    "MELANOGRAMMUS AEGLEFINUS",
    "MERLUCCIUS BILINEARIS",
    "POLLACHIUS VIRENS",
    "REINHARDTIUS HIPPOGLOSSOIDES",
    "SCOMBER SCOMBRUS",
    "SEBASTES FASCIATUS",
    "SQUALUS ACANTHIAS",
    "UROPHYCIS TENUIS",
    "MALLOTUS VILLOSUS"
  )
)

# get max length and convert to max weight
length_weight <- rfishbase::estimate(scinames_fb) |>
  select("Species", "a", "b")

max_length <- rfishbase::species(scinames_fb) |>
  select(Species, Length)

max_weight <- full_join(max_length, length_weight) |>
  mutate(w_max = a * Length ^ b)

# get maturation length and age
maturity_tbl <- rfishbase::maturity(scinames_fb) |>
  select(Species, l_mat = Lm, age_mat = tm, Locality) |>
  na.omit()

maturity <- maturity_tbl |>
  group_by(Species) |>
  summarise(l_mat = median(l_mat), age_mat = median(age_mat)) |>
  ungroup()

sp <- max_weight |>
  left_join(maturity, by = "Species") |>
  mutate(w_mat = a * l_mat ^ b)

# fill in missing maturity for sand lance (Ammodytes americanus)
ammodytes <- rfishbase::maturity("Ammodytes americanus") |>
  select(Species, l_mat = Lm, age_mat = tm, Locality)
ammodytes <- median(ammodytes$l_mat, na.rm = TRUE)

ammo_row <- which(sp$Species == "Ammodytes americanus")
sp[ammo_row, "l_mat"] <- ammodytes
sp[ammo_row, "w_mat"] <- sp[ammo_row, "a"] * sp[ammo_row, "l_mat"] ^ sp[ammo_row, "b"]

# Add SCINAME safely via lookup — no positional assignment
sp <- sp |> left_join(sciname_lookup, by = "Species")

nrow(sp)  # should be 16

# get preferred PPMR, beta and sigma
ppmr <- read.csv("Ess_data/new_predpreylengths.csv")
ppmr$SCINAME <- trimws(ppmr$SCINAME)

ppmr <- ppmr |>
  mutate(
    prey_wght = 0.01 * PYLEN^3,
    pred_wght = a * PDLEN^b,
    ppmr      = pred_wght / prey_wght
  ) |>
  filter(PDLEN < 900) |>
  filter(PYNAM != "EMPTY")

# compute beta and sigma per species
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
# Capelin (Mallotus villosus) has no stomach data in the NOAA food habits database, 
# so you can't compute beta and sigma for it directly. 
# Instead of leaving it empty, the code borrows the values from Atlantic Herring (Clupea harengus) as a proxy — both are small, schooling, pelagic forage fish with similar ecological roles and likely similar predator-prey size ratios.

capelin <- betasig |>
  filter(SCINAME == "CLUPEA HARENGUS") |>
  mutate(SCINAME = "MALLOTUS VILLOSUS")

betasig <- bind_rows(betasig, capelin)

# histograms of pred lengths
p1 <- ppmr |>
  ggplot(aes(x = PDLEN)) +
  geom_histogram(color = "#e9ecef", alpha = 0.6, position = "identity") +
  facet_wrap(~SCINAME, scales = "free") +
  theme_minimal()

# histograms of ppmr on log-scale
p2 <- ppmr |>
  ggplot(aes(x = log10(ppmr))) +
  geom_histogram(color = "#e9ecef", alpha = 0.6, position = "identity") +
  facet_wrap(~SCINAME, scales = "free") +
  theme_minimal()

# log10 pred vs log10 prey size
p3 <- ggplot(ppmr, aes(log10(pred_wght), log10(prey_wght))) +
  facet_wrap(~SCINAME) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  theme_minimal()

# join betasig
sp <- sp |> left_join(betasig, by = "SCINAME")

# verify
nrow(sp)  # should be 16
sp[, c("Species", "SCINAME", "beta", "sigma")]

# w_min — 0.001 for teleosts; spiny dogfish uses l_min = 26 cm
sp$w_min <- 0.001
dogfish_row <- which(sp$SCINAME == "SQUALUS ACANTHIAS")
if (length(dogfish_row) > 0) {
  sp$w_min[dogfish_row] <- sp$a[dogfish_row] * 26^sp$b[dogfish_row]
}

sp
write.csv(sp, "Ess_data/new_ESS_species_params.csv")



