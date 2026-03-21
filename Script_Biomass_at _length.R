# Create species lookup table from the code list you provided 
#Rv Survey species code
  species_lookup_subset <- tibble::tribble(
    ~CODE, ~SPECIES_NAME,
    10,  "Atlantic Cod",
    11,  "Haddock",
    12,  "White Hake",
    14,  "Silver Hake",
    16,  "Pollock",
    23,  "Redfish Unseparated",
    31,  "Greenland Halibut",
    40,  "American Plaice",
    41,  "Witch Flounder",
    42,  "Yellowtail Flounder",
    43,  "Winter Flounder",
    60,  "Atlantic herring",
    61,  "Shad American",
    64,  "Capelin",
    70,  "Mackerel",
    160, "Argentine",
    200, "Barndoor Skate",
    201, "Thorny Skate",
    204, "Winter Skate",
    220, "Spiny Dogfish",
    300, "Longhorn Sculpin",
    400, "Monkfish",
    592, "Sharks (Unspecified)",
    610, "Sand Lance",
    701, "Butterfish",
    2211, "Pandalus Borealis",
    2522, NA,
    4511, "Short-Fin Squid",
    6600, "Holothuroidea",
    6611, "Sea Cucumber")
  
biomass_length<- read.csv("Alida_EZ_biomass_at_Length.csv")
# Join species names into biomass_length
biomass_length <- biomass_length %>%
  left_join(species_lookup_subset, by = c("SPECIES" = "CODE"))

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
