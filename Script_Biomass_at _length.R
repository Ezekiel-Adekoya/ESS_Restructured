# Create species lookup table from the code list you provided 
#Rv Survey species code
species_lookup <- tibble::tribble(
  ~CODE, ~SPECIES_NAME,
  10,   "Atlantic Cod",
  11,   "Haddock",
  12,   "White Hake",
  13,   "Squirrel Or Red Hake",
  14,   "Silver Hake",
  15,   "Cusk",
  16,   "Pollock",
  18,   "Replaces Gadoidei S.O.",
  19,   "Off-Shore Hake",
  23,   "Redfish Unseparated",
  25,   "Tile Fish",
  30,   "Halibut Atlantic",
  31,   "Turbot Greenland Halibut",
  40,   "American Plaice",
  41,   "Witch Flounder",
  42,   "Yellowtail Flounder",
  43,   "Winter Flounder",
  44,   "Gulf Stream Flounder",
  50,   "Striped Atlantic Wolffish",
  51,   "Spotted Wolffish",
  52,   "Northern Wolffish",
  60,   "Herring Atlantic",
  61,   "Shad American",
  62,   "Alewife",
  63,   "Rainbow Smelt",
  64,   "Capelin",
  70,   "Mackerel Atlantic",
  72,   "Swordfish",
  88,   "Pilotfish",
  89,   "Bigeye Scad",
  92,   "Greater Amberjack",
  111,  "Spotted Hake",
  112,  "Longfin Hake",
  114,  "Fourbeard Rockling",
  115,  "Threebeard Rockling",
  117,  "Blue Whiting",
  118,  "Greenland Cod",
  119,  "Rockling Unidentified",
  122,  "Cunner",
  123,  "Rosefish Black Belly",
  141,  "Summer Flounder",
  142,  "Fourspot Flounder",
  143,  "Brill/Windowpane",
  149,  "Longnose Greeneye",
  150,  "Lanternfish",
  151,  "Stomiatoid Unidentified",
  155,  "Longtooth Anglemouth",
  156,  "Short-Nose Greeneye",
  157,  "Glacier Lanternfish",
  158,  "Mueller's Pearlsides",
  159,  "Boa Dragonfish",
  160,  "Argentine Atlantic",
  163,  "Lanternfish Horned",
  169,  "Viperfish",
  180,  "Spotted Lanternfish",
  182,  "Lanternfish Kroyer's",
  196,  "Lefteye Flounder",
  200,  "Barndoor Skate",
  201,  "Thorny Skate",
  202,  "Smooth Skate",
  203,  "Little Skate",
  204,  "Winter Skate",
  205,  "Spinytail Skate",
  207,  "Round Skate",
  211,  "Skates",
  216,  "Atlantic Torpedo",
  220,  "Spiny Dogfish",
  221,  "Black Dogfish",
  224,  "Rough Sagre",
  240,  "Sea Lamprey",
  241,  "Northern Hagfish",
  280,  "Scorpionfishes",
  293,  "Spinirostris",
  300,  "Longhorn Sculpin",
  301,  "Shorthorn Sculpin",
  302,  "Arctic Staghorn Sculpin",
  303,  "Grubby Or Little Sculpin",
  304,  "Mailed Sculpin",
  305,  "Nybelin Sculpin",
  306,  "Snowflake Hookear Sculpin",
  307,  "Polar Sculpin",
  308,  "Pallid Sculpin",
  309,  "Sculpin",
  312,  "Sculpins",
  313,  "Twohorn Sculpin",
  314,  "Spatulate Sculpin",
  316,  "Arctic Sculpin",
  317,  "Ribbed Sculpin",
  320,  "Sea Raven",
  323,  "Hookear Sculpin",
  330,  "Northern Common Sea Robin",
  331,  "Armored Sea Robin",
  340,  "Alligatorfish",
  341,  "Arctic Alligatorfish",
  350,  "Atlantic Sea Poacher",
  351,  "Alligator Fish",
  384,  "Deepbody Boarfish",
  385,  "Deepwater Flounder",
  395,  "Macrostomias",
  400,  "Monkfish Goosefish Angler",
  409,  "American Straptail Grenadier",
  410,  "Marlin-Spike Grenadier",
  411,  "Roughhead Grenadier",
  412,  "Roughnose Grenadier",
  413,  "Longnose Grenadier",
  414,  "Roundnose Grenadier",
  416,  "Grenadiers",
  498,  "Yellowfin Bass",
  500,  "Seasnail Unidentified",
  501,  "Lumpfish",
  502,  "Atlantic Spiny Lumpsucker",
  503,  "Atlantic Seasnail",
  504,  "Striped Seasnail",
  505,  "Seasnail Gelatinous",
  507,  "Longfin Seasnail",
  508,  "Inquiline Seasnail",
  512,  "Seasnail Dusky",
  513,  "Gulf Sea Snail",
  520,  "Sea Tadpole",
  530,  "Gapper",
  556,  "Longspine Snipefish",
  558,  "Melamphaeidae",
  576,  "Blackmouth Bass",
  590,  "Sand Lances",
  592,  "Shark",
  593,  "Greeneyes",
  595,  "Red Dory",
  602,  "Gray's Cutthroat Eel",
  603,  "Wolf Eelpout",
  604,  "Snipe Eel",
  610,  "Northern Sand Lance",
  611,  "Sand Lance",
  616,  "Fish Doctor",
  617,  "Common Wolf Eel",
  619,  "Eelpout Newfoundland",
  620,  "Laval's Eelpout",
  621,  "Rock Gunnel Eel",
  622,  "Snake Blenny",
  623,  "Daubed Shanny",
  624,  "Arctic Shanny",
  625,  "Radiated Shanny",
  626,  "4-Line Snake Blenny",
  628,  "Polar Eelpout",
  630,  "Wrymouth",
  631,  "Slender Eelblenny",
  632,  "Stout Eelblenny",
  635,  "Dragonets",
  637,  "Spotfin Dragonet",
  639,  "Pricklebacks",
  640,  "Ocean Pout",
  641,  "Arctic Eelpout",
  642,  "Eelpouts",
  643,  "Vachon's Eelpout",
  644,  "Blennies Shannies Gunnels",
  646,  "Atlantic Soft Pout",
  647,  "Shorttailed Eelpout",
  650,  "Fawn Cusk Eel",
  664,  "Sea Basses",
  700,  "Hatchetfish Atlantic Silver",
  701,  "Butterfish",
  704,  "American John Dory",
  708,  "Hatchetfish Slope",
  711,  "Short Barracudina",
  712,  "White Barracudina",
  713,  "Barracudina Unidentified",
  714,  "Simonyi's Frostfish",
  720,  "Atlantic Saury Needlefish",
  732,  "Daggertooth",
  741,  "Unidentified Hatchetfish",
  742,  "Atlantic Batfish",
  743,  "American Barrelfish",
  744,  "Stout Beard Fish",
  745,  "Anglemouth",
  746,  "Northern Puffer",
  755,  "Anglemouth",
  771,  "Beardfish",
  774,  "Ogrefish",
  775,  "Alfonsin A Casta Larga",
  777,  "Thorny Tinselfish",
  805,  "Tonguefish",
  816,  "Tongue Fish",
  827,  "Puffer",
  844,  "Batfish Spiny",
  845,  "Eelpout",
  866,  "Spotted Tinselfish",
  868,  "Seasnail",
  880,  "Hookear Sculpin Atlantic",
  883,  "Gonostoma Bathyphilum",
  924,  "Scotian Snailfish",
  1006, "Orange Roughy",
  1100, "Eggs Unidentified",
  1191, "Skate Little Or Winter",
  1224, "Raja/Skate Eggs",
  1270, "Smooth Grenadier",
  1283, "Sergia",
  1510, "Buccinidae/Whelk Eggs",
  1511, "Gasteropoda/Snail/Slug Eggs",
  1530, "Cephalopoda/Squid Eggs",
  1821, "Sea Squirts",
  1823, "Sea Potato",
  1826, "Sea Grapes",
  1827, "Sea Peach",
  1900, "Bryozoans",
  1901, "Lemonweed",
  1930, "Lampshells",
  2100, "Shrimps",
  2223, "Sergestes",
  2332, "Spirontocaris",
  2510, "Brachiuran Crabs",
  2511, "Jonah Crab",
  2513, "Atlantic Rock Crab",
  2519, "Spider Crab",
  2520, "Toad Crab Unidentified",
  2522, "Unknown 2522",
  2523, "Northern Stone Crab",
  2525, "Spiny Crab",
  2526, "Snow Crab Queen",
  2527, "Toad Crab",
  2528, "Porcupine Crab",
  2532, "Red Deepsea Crab",
  2550, "American Lobster",
  2555, "Munida",
  2556, "Munida",
  2559, "Hermit Crabs",
  2600, "Krill Shrimp",
  2611, "Northern Krill",
  2700, "Mysid Shrimp",
  2803, "Amphipods",
  2908, "Copepod",
  2980, "Red Isopod",
  2990, "Barnacles",
  2999, "Isopod",
  3000, "Segmented Worms",
  3100, "Bristle Worms",
  3147, "Fan Worm",
  3161, "Lugworm",
  3200, "Sea Mouse",
  3300, "Gephyrea",
  3600, "Sea Snails Sea Butterflies Pteropoda",
  4200, "Snails And Slugs",
  4210, "Whelks",
  4211, "Wave Whelk",
  4212, "Silky Buccinum",
  4220, "Moon Snail Unspecified",
  4221, "Northern Moonsnail",
  4225, "Groenlandica",
  4227, "New England Neptune",
  4228, "Spindle Shell",
  4230, "Duck Or Pelican Foot",
  4250, "Periwinkles",
  4255, "Top Shells",
  4300, "Bivalves Protobranchia",
  4304, "Ocean Quahaug",
  4312, "Bank Clam",
  4314, "Morrhua Venus Pitar Morrhuana",
  4315, "Razor Clam",
  4317, "Bar Surf Clam",
  4318, "Soft Shell Clam",
  4319, "Soft Shell Or Long Neck Clam",
  4320, "Scallops",
  4321, "Sea Scallop",
  4322, "Iceland Scallop",
  4323, "Hard Clam",
  4330, "Mussels",
  4331, "Common Mussels",
  4332, "Horse Mussels",
  4340, "Cockles",
  4343, "Greenland Cockle",
  4346, "Solemya",
  4348, "Mollusca Shells Empty",
  4349, "Scallop Shells",
  4350, "Nut Shells",
  4355, "Stimpson's Surf Clam",
  4400, "Sea Slugs",
  4410, "Bushy Backslug",
  4431, "Giant Canoe Bubble",
  4511, "Short-Fin Squid",
  4512, "Longfin Squid",
  4514, "Squid",
  4521, "Octopus",
  4536, "Bobtail Squid",
  4700, "Chitons",
  5100, "Sea Spider",
  5200, "Limpet",
  6000, "Spiny Skinned Animals",
  6101, "Ceremaster",
  6102, "Pulvilis",
  6115, "Mud Star",
  6118, "Blood Star Genus",
  6119, "Blood Star",
  6121, "Purple Sunstar",
  6123, "Spiny Sunstar",
  6130, "Poraniomorpha",
  6134, "Slender Armed Sea Star",
  6200, "Brittle Star",
  6211, "Daisy",
  6213, "Sarsi",
  6300, "Basket Stars",
  6400, "Sea Urchins",
  6411, "Green Sea Urchin",
  6413, "Heart Urchin",
  6421, "Purple-Spined Sea Urchin",
  6500, "Sand Dollars",
  6601, "Phyllophoridae",
  6611, "Sea Cucumber Common",
  6713, "Scarlett Psolus",
  6900, "Sea Lilies",
  7100, "Parasites Round Worms",
  7500, "Turbellaria",
  8010, "Moon Jelly",
  8100, "Comb Jellies",
  8300, "Sea Anemone",
  8315, "Tealia",
  8318, "Sea Pen",
  8322, "Sea Corn",
  8323, "Bubble Gum Coral",
  8324, "Sea Cauliflower",
  8325, "Gold-Banded Bamboo Coral",
  8327, "Soft Coral Unidentified",
  8331, "Lophelia Spider Coral",
  8335, "Cup Coral",
  8336, "Dead Man's Fingers",
  8337, "Black Corals",
  8341, "Hydrozoa",
  8343, "Ribbon Worm",
  8360, "Pennatula Borealis",
  8363, "Halipterus",
  8405, "Garland Hydroids",
  8500, "Jellyfishes",
  8511, "Pink Jellyfish",
  8520, "Jellyfish",
  8530, "Sea Corals",
  8600, "Sponges",
  8601, "Russian Hats",
  8611, "Mammilaris",
  8700, "Siphonophora"
)
biomass_length<- read.csv("Biomass_by_length.csv")
# Join species names into biomass_length
biomass_length <- biomass_length %>%
  left_join(species_lookup, by = c("SPECIES" = "CODE"))

# Check it worked
biomass_length %>%
  select(YEAR, SPECIES, SPECIES_NAME, BIOMASS) %>%
  head(20) %>%
  as.data.frame()

write.csv(biomass_length, "biomass_length_with_names.csv", row.names = FALSE)


# ----------------------------
# Biomass % by length class across all species and all years
# ----------------------------
biomass_length<- read.csv("biomass_length_with_names.csv")

length_totals_short <- biomass_length %>%
  filter(LENGTH != -99) %>%                                      # remove unmeasured records
  group_by(LENGTH, SPECIES_NAME) %>%
  summarise(biomass = sum(BIOMASS, na.rm = TRUE), .groups = "drop") %>%
  group_by(LENGTH) %>%
  mutate(grand_total   = sum(biomass),
         pct_of_length = biomass / grand_total * 100) %>%
  ungroup() %>% rename(species = SPECIES_NAME)

# Check
str(length_totals_short)
head(length_totals_short)


length_table <- length_totals_short %>%
  filter(grand_total > 0) %>% mutate(length_class = cut(LENGTH,
          breaks = seq(0, max(LENGTH) + 10, by = 10),
          labels = paste0(seq(1, max(LENGTH) + 1, by = 10), "-",
           seq(10, max(LENGTH) + 10, by = 10)), right  = TRUE,
           include.lowest = TRUE)) %>%
  group_by(length_class, species) %>%
  summarise(biomass = sum(biomass, na.rm = TRUE), .groups = "drop") %>%
  group_by(length_class) %>%
  mutate(total_biomass = sum(biomass), pct_of_length = biomass / total_biomass * 100) %>%ungroup()

# Sanity check
length_table %>%
  group_by(length_class) %>%
  summarise(total_pct = round(sum(pct_of_length), 10)) %>%
  print(n = Inf)

# ----------------------------
# Pivot wide: rows = length class, cols = species %
# ----------------------------
length_table_wide <- length_table %>%
  select(length_class, species, pct_of_length) %>%
  pivot_wider(names_from  = species,
              values_from = pct_of_length,
              values_fill = 0) %>%                               # 0 where species absent
  arrange(length_class)

# Add total biomass as final column
total_by_class <- length_table %>%
  group_by(length_class) %>%
  summarise(Total_Biomass = mean(total_biomass), .groups = "drop") # mean avoids double counting

length_table_wide <- length_table_wide %>%
  left_join(total_by_class, by = "length_class") %>%
  mutate(Total_Biomass = scales::comma(round(Total_Biomass)))

# ----------------------------
# gt table
# ----------------------------
library(gt)

length_table_wide %>%
  gt(rowname_col = "length_class") %>%
  tab_header(
    title    = md("**Biomass Contribution (%) by Length Class**"),
    subtitle = "Each cell = species % of total biomass at that length class | Final column = total biomass") %>%
  fmt_number(
    columns  = where(is.numeric),
    decimals = 1) %>%
  data_color(
    columns  = where(is.numeric),
    palette  = "Blues"  ) %>%
  tab_style(
    style    = list(cell_fill(color = "#2c3e50"),
                    cell_text(color = "white", weight = "bold")),
    locations = cells_stub()) %>%
  tab_style(style    = list(cell_fill(color = "#2c3e50"),
                            cell_text(color = "white", weight = "bold")),
            locations = cells_column_labels()) %>%
  tab_options(
    table.font.size = 12,
    heading.align   = "left" ) %>%
  gtsave("biomass_length_table.html")

cat("Table saved to biomass_length_table.html\n")
