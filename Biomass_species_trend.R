library(tidyverse)

# ----------------------------
# 1. Load and clean data
# ----------------------------
biomass_aj <- read.csv("Biomass_Qadjusted.csv", check.names = FALSE)
biomass_aj <- biomass_aj[, names(biomass_aj) != "NA"]

biomass_aj  <- biomass_aj  %>% select(where(~ sum(is.na(.)) <= 10)) # remove cols with more than 20 NA rows
# Confirm
ncol(biomass_aj)
names(biomass_aj)[names(biomass_aj) == "NA"]  # should return character(0)

biomass_aj <- biomass_aj %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(Year)) %>%
  { if ("X" %in% names(.)) select(., -X) else . } %>%          # remove row-index column if present
  select(Year, where(~!all(is.na(.x)))) %>%                     # drop fully empty species columns
  select(-matches("^NA\\."))                                     # drop unnamed columns (NA., NA..1, etc.)

# ----------------------------
# 2. Compute total biomass per year
# ----------------------------
year_totals <- biomass_aj %>%
  mutate(across(-Year, ~replace_na(.x, 0))) %>%                 # treat missing as 0 for summing
  mutate(total = rowSums(across(-Year))) %>%                     # row-wise sum across all species
  select(Year, total)

# ----------------------------
# 3. Select top 20 species overall
# ----------------------------
top_species <- biomass_aj %>%
  select(-Year) %>%
  summarise(across(everything(), ~sum(.x, na.rm = TRUE))) %>%   # total biomass per species across all years
  pivot_longer(everything(), names_to = "species", values_to = "total") %>%
  slice_max(total, n = 30) %>%                                   # keep top 20 by cumulative biomass
  pull(species)

# ----------------------------
# 4. Build data for top 20 species
# ----------------------------
df_top <- biomass_aj %>%
  select(year = Year, all_of(top_species)) %>%
  pivot_longer(-year, names_to = "species", values_to = "biomass") %>%
  mutate(biomass = replace_na(biomass, 0)) %>%
  left_join(year_totals, by = c("year" = "Year")) %>%           # attach annual totals for proportion calc
  filter(total > 0) %>%
  mutate(
    ratio = biomass / total,                                     # proportion of total biomass
    species = str_replace_all(species, "\\.", " ") %>%          # replace dots with spaces
      str_to_title()                                    # title case (e.g. ATLANTIC.COD -> Atlantic Cod)
  ) %>%
  filter(is.finite(ratio))

# ----------------------------
# 5. Build "Other" category (all non-top-20 species combined)
# ----------------------------
df_other <- biomass_aj %>%
  select(-all_of(top_species)) %>%                              # keep only non-top species + Year
  pivot_longer(-Year, names_to = "name", values_to = "biomass") %>%
  mutate(biomass = replace_na(biomass, 0)) %>%
  group_by(year = Year) %>%
  summarise(biomass = sum(biomass), .groups = "drop") %>%       # sum all minor species per year
  left_join(year_totals, by = c("year" = "Year")) %>%
  filter(total > 0) %>%
  mutate(
    ratio = biomass / total,
    species = "Other"
  )

# ----------------------------
# 6. Combine and set factor order (Other always last in legend)
# ----------------------------
df_long <- bind_rows(df_top, df_other) %>%
  mutate(species = factor(species,
                          levels = c(sort(unique(df_top$species)), "Other")))

# ----------------------------
# 7. Plot
# ----------------------------
p <- ggplot(df_long, aes(x = year, y = ratio, fill = species)) +
  geom_area(alpha = 0.85, colour = "white", linewidth = 0.2) +
  scale_x_continuous(
    breaks = seq(1970, 2018, by = 5),
    limits = c(1970, 2018),
    expand = c(0, 0) ) +
  scale_y_continuous(
    limits = c(0, 1),
    expand = c(0, 0),
    labels = scales::label_number(accuracy = 0.1)               # decimal labels e.g. 0.1, 0.5
  ) +
  scale_fill_manual(
    values = colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(31)  # 21 colours for 20 species + Other
  ) +
  labs(
    title = "Top 30 Species as Proportion of Total Annual Biomass",
    x = "Year",
    y = "Proportion of total biomass",
    fill = "Species"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 9),
    legend.position = "bottom",
    # ↓ shrink legend
    legend.key.size = unit(0.3, "cm"),
    legend.text = element_text(size = 10),
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    
    panel.grid.minor = element_blank()
  )

p
ggsave("plot.jpeg", plot = p, width = 12, height = 8, dpi = 300)

