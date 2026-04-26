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
fhspecies<-read.csv("Ess_data/svspecies.csv")
unique(fhspecies$SCINAME)
pdpylen<-merge(fhspecies[,c(1:3)],pdlen,by=c("SVSPP"))

scinames<-c("GADUS MORHUA",                    # Atlantic Cod
            "MELANOGRAMMUS AEGLEFINUS",        # Haddock
            "UROPHYCIS TENUIS",                # White Hake
            "MERLUCCIUS BILINEARIS",           # Silver Hake
            "POLLACHIUS VIRENS",               # Pollock
            "SEBASTES FASCIATUS",              # Redfishes Unseparated
            #"REINHARDTIUS HIPPOGLOSSOIDES",    # Greenland Halibut
            "HIPPOGLOSSOIDES PLATESSOIDES",    # American Plaice
            #"GLYPTOCEPHALUS CYNOGLOSSUS",      # Witch Flounder
            "LIMANDA FERRUGINEA",              # Yellowtail Flounder
            # "PSEUDOPLEURONECTES AMERICANUS",   # Winter Flounder 
            "CLUPEA HARENGUS",                 # Atlantic Herring
            "MALLOTUS VILLOSUS",               # Capelin
            "SCOMBER SCOMBRUS",                # Atlantic Mackerel
            "ARGENTINA SILUS",                 # Argentines 
            #"DIPTURUS LAEVIS" ,                # Barndoor Skate
            "AMBLYRAJA RADIATA",               # Thorny Skate
            #"SQUALUS ACANTHIAS",               # Spiny Dogfish
            "AMMODYTES DUBIUS"   ,             # Sand lance
            # MALLOTUS VILLOSUS excluded - no stomach data, assign beta/sigma manually
            "HIPPOGLOSSUS HIPPOGLOSSUS" # Atlantic Halibut
)
# merge species names#merge species names

fhspecies<-read.csv("Ess_data/svspecies.csv")
unique(fhspecies$SCINAME)
pdpylen<-merge(fhspecies[,c(1:3)],pdlen,by=c("SVSPP"))


sub_pdpylen <- pdpylen |>
  filter(
    SCINAME %in% scinames |
      grepl("AMMODYTES",    SCINAME, ignore.case = TRUE) |
      grepl("SEBASTES",     SCINAME, ignore.case = TRUE) |
      grepl("LIMANDA",      SCINAME, ignore.case = TRUE) |
      grepl("REINHARDTIUS", SCINAME, ignore.case = TRUE)
  )
unique(sub_pdpylen$SCINAME)
# target species in survey data
#convert prey length to cm
sub_pdpylen$PYLEN <-sub_pdpylen$PYLEN/100

#save subset file
write.csv(sub_pdpylen,"Ess_data/predpreylengths.csv")




