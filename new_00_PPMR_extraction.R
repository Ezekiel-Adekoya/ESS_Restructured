#Predator–Prey Mass Ratio Extarction
library(tidyverse)

#data source: https://www.fisheries.noaa.gov/inport/item/36902

fhpd      <- read.csv("Ess_data/fhpd.csv")
fhpy      <- read.csv("Ess_data/fhpy.csv")
fhpyl     <- read.csv("Ess_data/fhpyl.csv")
fhspecies <- read.csv("Ess_data/svspecies.csv")

# merge predator lengths with predator data
pdlen <- merge(
  fhpyl[, c(2, 4:7, 10:12)],
  fhpd[,  c(2, 6:9, 23)],
  by  = c("CRUISE6", "STATION", "SVSPP", "PDSEX", "PDID"),
  all.x = TRUE
)

# merge species names
pdpylen <- merge(fhspecies[, c(1:3)], pdlen, by = "SVSPP")

# target species in survey data
rscinames <- c(
  "GADUS MORHUA",
  "MELANOGRAMMUS AEGLEFINUS",
  "SEBASTES FASCIATUS",
  "MERLUCCIUS BILINEARIS",
  "POLLACHIUS VIRENS",
  "HIPPOGLOSSOIDES PLATESSOIDES",
  "GLYPTOCEPHALUS CYNOGLOSSUS",
  "LIMANDA FERRUGINEA",
  "REINHARDTIUS HIPPOGLOSSOIDES",
  "HIPPOGLOSSUS HIPPOGLOSSUS",
  "UROPHYCIS TENUIS",
  "CLUPEA HARENGUS",
  "SCOMBER SCOMBRUS",
  "SQUALUS ACANTHIAS",
  "AMMODYTES DUBIUS",
  "BROSME BROSME",
  "UROPHYCIS CHUSS",
  "AMBLYRAJA RADIATA",
  "MALACORAJA SENTA",
  "LEUCORAJA ERINACEA",
  "LEUCORAJA OCELLATA"
  # MALLOTUS VILLOSUS excluded - no stomach data, assign beta/sigma manually
)
#Atlantic Cod	Haddock	White Hake	Silver Hake	Pollock	Redfish (Unseparated)	Atlantic Halibut	Greenland Halibut	American  Plaice	Witch Flounder	Yellowtaail  Flounder	Atlantic Herring	Capelin	Atlantic Mackerel	Sand Lance	Spiny Dogfish
# This species have no ppm in the file
# | Scientific Name    | Common Name  |
  # | ------------------ | ------------ |
  # | BROSME BROSME      | Cusk         |
  # | UROPHYCIS CHUSS    | Red Hake     |
  # | AMBLYRAJA RADIATA  | Thorny Skate |
  # | MALACORAJA SENTA   | Smooth Skate |
  # | LEUCORAJA ERINACEA | Little Skate |
  # | LEUCORAJA OCELLATA | Winter Skate |
  


#Atlantic Cod	Haddock	White Hake	Red Hake	Silver Hake	Cusk	Pollock	Redfish (Unseparated)	Atlantic Halibut	Greenland Halibut	American  Plaice	Witch Flounder	Yellowtaail  Flounder	Atlantic Herring	Capelin	Atlantic Mackerel	Thorny Skate	Smooth Skate	Little Skate	Winter Skate	Sand Lance	Spiny Dogfish
# filter to target species
sub_pdpylen <- pdpylen |>
  filter(SCINAME %in% rscinames)

# verify - both should confirm 15 species
setdiff(rscinames, unique(sub_pdpylen$SCINAME))
unique(sub_pdpylen$SCINAME)

# sub_pdpylen<-rbind(filter(pdpylen,SCINAME==scinames),
#                    filter(pdpylen,grepl('AMMODYTES', SCINAME, ignore.case = TRUE)),
#                    filter(pdpylen,grepl('SEBASTES', SCINAME, ignore.case = TRUE)),
#                    filter(pdpylen,grepl('LIMANDA', SCINAME, ignore.case = TRUE))
# )
# unique(sub_pdpylen$SCINAME)
capelin <- betasig |>
  filter(SCINAME == "CLUPEA HARENGUS") |>
  mutate(SCINAME = "MALLOTUS VILLOSUS")

betasig <- bind_rows(betasig, capelin)

#convert prey length to cm
sub_pdpylen$PYLEN <-sub_pdpylen$PYLEN/100
unique(sub_pdpylen$SCINAME)


# 1. filter and prep data
sub_pdpylen <- pdpylen |> filter(SCINAME %in% rscinames)
sub_pdpylen$PYLEN <- sub_pdpylen$PYLEN / 100

# 2. merge a and b parameters
sub_pdpylen <- merge(sub_pdpylen, sp[, c("SCINAME", "a", "b")], by = "SCINAME")

# 3. calculate ppmr
sub_pdpylen <- sub_pdpylen |>
  mutate(
    prey_wght = 0.01 * PYLEN^3,
    pred_wght = a * PDLEN^b,
    ppmr      = pred_wght / prey_wght
  ) |>
  filter(PDLEN < 900) |>
  filter(PYNAM != "EMPTY")

# 4. compute betasig
betasig <- sub_pdpylen |>
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

# 5. NOW add capelin proxy using herring
capelin <- betasig |>
  filter(SCINAME == "CLUPEA HARENGUS") |>
  mutate(SCINAME = "MALLOTUS VILLOSUS")

betasig <- bind_rows(betasig, capelin)

# verify
unique(betasig$SCINAME)  # should show 22

#save subset file
write.csv(sub_pdpylen,"Ess_data/new_predpreylengths.csv")


