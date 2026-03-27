# SpeciesParams_extraction from fishbase
library(rfishbase)
library(tidyverse)

scinames_fb <- c(
  "Gadus morhua",                    # Atlantic Cod
  "Melanogrammus aeglefinus",        # Haddock
  "Urophycis tenuis",                # White Hake
  "Merluccius bilinearis",           # Silver Hake
  "Pollachius virens",               # Pollock
  "Sebastes fasciatus",              # Redfishes Unseparated
  #"Reinhardtius hippoglossoides",    # Greenland Halibut
  "Hippoglossoides platessoides",    # American Plaice
  #"Glyptocephalus cynoglossus",      # Witch Flounder
  "Myzopsetta ferruginea",              # Yellowtail Flounder
  #"Pseudopleuronectes americanus",   # Winter Flounder 
  "Clupea harengus",                 # Atlantic Herring
  "Mallotus villosus",               # Capelin
  "Scomber scombrus",                # Atlantic Mackerel
  "Argentina silus",                 # Atlantic Argentine 
  #"Dipturus laevis",                 # Barndoor Skate
  "Amblyraja radiata",               #Thorny Skate
 # "Squalus acanthias",               # Spiny Dogfish
  "Ammodytes dubius" # Sand Lance
 # "Hippoglossus hippoglossus" ## Atlantic Halibut
)


# Lookup table — ties SCINAME to Species by name, not row position
sciname_lookup <- tibble(
  Species = scinames_fb,
  SCINAME = c(
    "GADUS MORHUA",                    # Atlantic Cod
    "MELANOGRAMMUS AEGLEFINUS",        # Haddock
    "UROPHYCIS TENUIS",                # White Hake
    "MERLUCCIUS BILINEARIS",           # Silver Hake
    "POLLACHIUS VIRENS",               # Pollock
    "SEBASTES FASCIATUS",              # Redfishes Unseparated
    #"REINHARDTIUS HIPPOGLOSSOIDES",    # Greenland Halibut
    "HIPPOGLOSSOIDES PLATESSOIDES",    # American Plaice
    #"GLYPTOCEPHALUS CYNOGLOSSUS",      # Witch Flounder
    "LIMANDA FERRUGINEA",              # Yellowtail Flounder Myzopsetta MYZOPSETTA.  LIMANDA
    #"PSEUDOPLEURONECTES AMERICANUS",   # Winter Flounder 
    "CLUPEA HARENGUS",                 # Atlantic Herring
    "MALLOTUS VILLOSUS",               # Capelin
    "SCOMBER SCOMBRUS",                # Atlantic Mackerel
    "ARGENTINA SILUS",                 # Atlantic Argentine 
    #"DIPTURUS LAEVIS" ,                # Barndoor Skate
    "AMBLYRAJA RADIATA",               # Thorny Skate
    #"SQUALUS ACANTHIAS",               # Spiny Dogfish
    "AMMODYTES DUBIUS"                # Sand Lance
   # "HIPPOGLOSSUS HIPPOGLOSSUS"        ## Atlantic Halibut
  ))
 
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

#ammo_row <- which(sp$Species == "Ammodytes americanus")
ammo_row <- which(sp$Species == "Ammodytes dubius")
sp[ammo_row, "l_mat"] <- ammodytes
sp[ammo_row, "w_mat"] <- sp[ammo_row, "a"] * sp[ammo_row, "l_mat"] ^ sp[ammo_row, "b"]

# Add SCINAME safely via lookup — no positional assignment
sp <- sp |> left_join(sciname_lookup, by = "Species")

nrow(sp)  # should be 15
sp

ppmr <- read.csv("Ess_data/new_predpreylengths.csv")
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
write.csv(sp, "Ess_data/new_ESS_species_params.csv")