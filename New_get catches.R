library(dplyr)
library(ggplot2)

stat <- read.csv("Ess_data/reviewed.STATLANT 21A 4VX.csv") ## Updated

# take a look 
head(stat)

# exclude 4X for now, as not on survey data
#stat <- subset(stat, Division != "4X")
# 4VW is the eastern Scotian Shelf. The NAFO divisions break down as:
# 
# 4V — eastern Scotian Shelf (northern part), sometimes subdivided into 4Vn and 4Vs (north and south of 44°N)
# 4W — eastern Scotian Shelf (western part)
# 44,4VN
# 45,4VS
# 46,4W

stat <- subset(stat, Division %in% c("4VS","4VN", "4W"))

unique(stat$Species.Name)
names(stat)[1] <- "Year"
nafo_names <- c(
  "ATLANTIC COD - COD",
  "HADDOCK - HAD",
  "WHITE HAKE - HKW",
  "SILVER HAKE - HKS",
  "POLLOCK (SAITHE) - POK",
  "ATLANTIC REDFISHES (NS) - RED",
  "AMERICAN PLAICE - PLA",
  "AMERICAN ANGLER - ANG",
  "YELLOWTAIL FLOUNDER - YEL",
  "ATLANTIC HERRING - HER",
  "SKATES (NS) - SKA",
  "SPINY (=PICKED) DOGFISH - DGS",
  "SANDEELS (SANDLANCES) - SAN",
  "ATLANTIC HALIBUT - HAL",
  "RED HAKE - HKR",
  "SKATES (NS) - SKA" , 
  "SCULPINS (NS) - SCU"
)

# Return to original data to subset species in model
stat <- subset(stat, Species.Name %in% nafo_names)

# rename
stat$Species <- case_match(stat$Species.Name,
                           "ATLANTIC COD - COD"             ~ "Atlantic Cod",
                           "HADDOCK - HAD"                  ~ "Haddock",
                           "WHITE HAKE - HKW"               ~ "White Hake",
                           "SILVER HAKE - HKS"              ~ "Silver Hake",
                           "POLLOCK (SAITHE) - POK"         ~ "Pollock",
                           "ATLANTIC REDFISHES (NS) - RED"  ~ "Redfish Unseparated",
                           "AMERICAN PLAICE - PLA"          ~ "American Plaice",
                           "YELLOWTAIL FLOUNDER - YEL"      ~ "Yellowtail Flounder",
                           "ATLANTIC HERRING - HER"         ~ "Atlantic Herring",
                           "SKATES (NS) - SKA"              ~ "Skates Unseparated",
                           "SPINY (=PICKED) DOGFISH - DGS"  ~ "Spiny Dogfish",
                           "SANDEELS (SANDLANCES) - SAN"    ~ "Sand Lance",
                           "ATLANTIC HALIBUT - HAL"         ~ "Atlantic Halibut",
                           "RED HAKE - HKR"                 ~ "Red Hake",
                           "SCULPINS (NS) - SCU"            ~ "Sculpins (NS)",
                           "AMERICAN ANGLER - ANG"          ~ "Monkfish",
                           .default = stat$Species.Name
)

# checking if thorny skate is in the East (found absent)
# subset(stat, Species.Name == "THORNY SKATE (STARRY RAY) - RJR")

stat_grouped <- stat %>%
  group_by(Year, Species) %>%
  reframe(catch_g = sum(Metric.Tonnes) * 1e6)

# check plots
names(stat_grouped) <- c("Year", "Species", "Catches_g_per_yr")

ggplot(stat_grouped, aes(x = Year, y = Catches_g_per_yr, group = Species)) + 
  geom_line() + facet_wrap(~Species, scales = "free_y") 

ggplot(stat_grouped, aes(x = Year, y = Catches_g_per_yr, fill = Species)) + geom_area() 

# saveRDS(stat_grouped, file = "Ess_data/New_NAFO_4VW_Catches.RDS")
#saveRDS(stat_grouped,"New_NAFO_4VW_Catches.RDS")

ggplot(stat_grouped, aes(x = Year, y = Catches_g_per_yr / 1e6)) +
  geom_line(color = "#2166AC", linewidth = 0.7) +
  geom_point(color = "#2166AC", size = 1.5, alpha = 0.7) +
  facet_wrap(~Species, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Year",
    y = "Catch (tonnes)"
  ) +
  theme_classic(base_size = 11) +
  theme(
    strip.text   = element_text(size = 7, face = "bold"),
    axis.title   = element_text(face = "bold"),
    axis.text    = element_text(color = "black", size = 7),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(colour = "grey70", fill = NA, linewidth = 0.5)
  )





#### STUCK on EFFORT

####### Effort Data: Explore STATLANT 21B Database

```{r}
## get data
cols <- c("Year","Country","GearCode","Tonnage","MainSpecies","Divcode","Code","Effort",
          "Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

cols2 <- c("Year","Country","Gear","Tonnage","MainSpecies","AreaCode","SpeciesEffort","Effort",
           "Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# read in each file
nafo60s <- read_delim("Ess_data/nafo-21b-60-69/NAFO21B-60-69.txt")[, cols]
nafo70s <- read_delim("Ess_data/nafo-21b-70-79/NAFO21B-70-79.txt")[, cols]
nafo80s <- read_delim("Ess_data/nafo-21b-80-89/NAFO21B-80-89.txt")[, cols]
nafo90s <- read_delim("Ess_data/nafo-21b-90-99/NAFO21B-90-99.txt")[, cols]
nafo00s <- read_delim("Ess_data/nafo-21b-2000-09/NAFO21B-2000-09.txt")[, cols]
nafo10s <- read_delim("Ess_data/NAFO-21B-2010-19/NAFO-21B-2010-2019.txt")[, cols2]
nafo20s <- read_delim("Ess_data/NAFO-21B-2020-24/NAFO-21B-2020-24.txt")[, cols2]

# rename columns & combine
names(nafo60s) <- names(nafo70s) <- names(nafo80s) <-
  names(nafo90s) <- names(nafo00s) <- cols2

nafo_21B <- rbind(nafo60s, nafo70s, nafo80s, nafo90s, nafo00s, nafo10s, nafo20s)

# subset to effort data
nafo_21B <- subset(nafo_21B, SpeciesEffort %in% c(1, 2, 3))

# subset to 4VW
nafo_21B <- subset(nafo_21B, AreaCode %in% c(44, 45, 46, 48))

# total effort
nafo_21B$TotalEffort <- rowSums(nafo_21B[, c("Jan","Feb","Mar","Apr","May","Jun",
                                             "Jul","Aug","Sep","Oct","Nov","Dec")])

nafo_21B <- nafo_21B[, c("Year","Country","Gear","Tonnage","MainSpecies",
                         "AreaCode","SpeciesEffort","Effort","TotalEffort")]

# days fished
nafo_21B_grouped_daysfished <- nafo_21B %>%
  subset(SpeciesEffort == "2") %>%
  group_by(Year, Gear, Tonnage, MainSpecies, SpeciesEffort) %>%
  reframe(TotalDaysFished = sum(TotalEffort))

### IGNORE THIS - only use if not doing species -specific effort but instead by gear type
### which Gear Codes to include? Trawls only?
# Bottom otter trawl (charters),8,OTB*
# Midwater trawl,9,OTM*
# Bottom otter trawl (side or stern not specified),10,OTB
# Bottom otter trawl (side),11,OTB-1
# Bottom otter trawl,12,OTB-2
# Midwater trawl (side or stern not specified),13,OTM
# Midwater trawl (side),14,OTM-1
# Midwater trawl (stern),15,OTM-2
# Bottom pair trawl (2 boats),16,PTB
# Midwater pair trawl (2 boats),17,PTM
# Beam trawl,18,TBB
# Otter shrimp twin trawl,19,OTS
# Otter twin trawl,192,OTT
# "Set gillnets",41,"GNS"
# "Drift gillnets",42,"GND"


# nafo_21B_gear_subset<-filter(nafo_21B_grouped_daysfished,Gear %in% c(8,9,10,11,12,13,14,15,16,17,18,19,192,51,41))
# 
# unique(nafo_21B_gear_subset$Gear)
unique(nafo_21B_grouped_daysfished$MainSpecies)

## 1, 2,3, 4, 5, 6, 10, 12, 16, 21, 30, 56, 54

nafo_21B_subset <- filter(nafo_21B_grouped_daysfished,
                          MainSpecies %in% c(1, 2, 3, 4, 5, 6, 10, 12, 16, 21, 30, 54, 56))

# MainSpecies to include (these are target species groups, not actual species caught):
# code	3-alpha	Species name
# 1	  COD	Atlantic cod
# 2	  HAD	Haddock
# 3	  RED	Atlantic redfishes
# 10	PLA	American plaice
# 11	WIT	Witch flounder
# 12	YEL	Yellowtail flounder
# 13	GHL	Greenland halibut
# 30	HER	Atlantic herring
# 56	DGX	Dogfish
# 16  HAL,"Atlantic halibut"
# 4   HKS,"Silver hake"
# 21, HKW,"White hake"
# NO NUMBER FOR SANDLANCES - include 49 (Peleagic fish) as dummy to be overwritten with zeroes
# Bycatch species (with and yellowtail flounder)
# 19,"FLX","Flatfishes (NS)"
# Bycatch species (white hake)
# 29,"GRO","Groundfish (NS)"


# rename MainSpecies to match params
nafo_21B_subset$MainSpecies <- as.factor(nafo_21B_subset$MainSpecies)

levels(nafo_21B_subset$MainSpecies) <- c(
  "Atlantic Cod",           # 1
  "Haddock",                # 2
  "Redfish Unseparated",    # 3
  "Silver Hake",            # 4  "Silver Hake",          
  "Red Hake",               # 5
  "Pollock",                # 6
  "American Plaice",        # 10
  "Yellowtail Flounder",    # 12
  "Atlantic Halibut",       # 16
  "White Hake",             # 21
  "Atlantic Herring",       # 30
  "Skates Unseparated" ,     # 54
  "Spiny Dogfish"         # 56


)

# nafo_21B_subset2$MainSpecies<-as.factor(nafo_21B_subset2$MainSpecies)
# levels(nafo_21B_subset2$MainSpecies)<-c("AMERICAN PLAICE", "COD(ATLANTIC)", "HERRING(ATLANTIC)","ATLANTIC HALIBUT","REDFISH UNSEPARATED", "TURBOT, GREENLAND HALIBUT", "HADDOCK","SANDLANCES", "SILVER HAKE", "SPINY DOGFISH",  "GROUNDFISH", "FLATFISH")

# Effort calculations:
### Need to convert Tonnage to Power (kW) by Gear type (FAO guidelines)
### Then multiply Power by Days Fished to get kW days per yr
### Anticamara et al 2011: 
###Power (kW) = 0.436 x (Length^2.021); R2 = 0.9; P < 0.0001
###Power (kW) = 11.26 x (Gross Tonnage^0.71)
#1. Calculate median of each length/tonnage category
#2. Calculate effort in kw*daysfished 


nafo_21B_subset$GT<-nafo_21B_subset$Power_kW<-NA
#code 1 or 2
nafo_21B_subset$GT[nafo_21B_subset$Tonnage %in% c(1,2)]<-(0+49.9)/2	
nafo_21B_subset$GT[nafo_21B_subset$Tonnage ==3]<-(50+149.9)/2
nafo_21B_subset$GT[nafo_21B_subset$Tonnage ==4]<-(150+499.9)/2	
nafo_21B_subset$GT[nafo_21B_subset$Tonnage ==5]<-(500+999.9)/2	
nafo_21B_subset$GT[nafo_21B_subset$Tonnage ==6]<-(1000+1999.9)/2	
nafo_21B_subset$GT[nafo_21B_subset$Tonnage ==7]<-(2000 +5000)/2
nafo_21B_subset$Power_kW<-11.26*nafo_21B_subset$GT^0.71

nafo_21B_subset$Effort_kW_Days_yr<-nafo_21B_subset$Power_kW*nafo_21B_subset$TotalDaysFished


nafo_21B_effort<-
  nafo_21B_subset%>% 
  group_by(Year,MainSpecies) %>%
  reframe(Effort_kW_Days_yr = sum(Effort_kW_Days_yr))


ggplot(nafo_21B_effort,aes(x=Year,y=Effort_kW_Days_yr,group=MainSpecies)) + geom_point(aes(color=MainSpecies))+ geom_line(aes(color=MainSpecies))  + facet_wrap(~MainSpecies,scales = "free") 

```


```