
library(tidyverse)
library(gt)

# -----------------------------------------------------------------------------
# 0. Species lookup
# -----------------------------------------------------------------------------

species_code_lookup <- c(
  "10"  = "Atlantic Cod",
  "11"  = "Haddock",
  "12"  = "White Hake",
  "14"  = "Silver Hake",
  "16"  = "Pollock",
  "23"  = "Redfish Unseparated",
  "30"  = "Atlantic Halibut",
  "40"  = "American Plaice",
  "60"  = "Atlantic Herring",
  "64"  = "Capelin",
  "70"  = "Atlantic Mackerel",
  "201" = "Thorny Skate",
  "42"  = "Yellowtail Flounder",
  "160" = "Atlantic Argentine",
  "610" = "Sand Lance"
)

# -----------------------------------------------------------------------------
# 1. Bin definitions — 20cm bins
# -----------------------------------------------------------------------------

bin_breaks <- seq(10, 190, by = 20)
bin_labels <- paste0("(", seq(10, 170, by = 20), "–", seq(30, 190, by = 20), "]")

# -----------------------------------------------------------------------------
# 2. Load and clean
# -----------------------------------------------------------------------------

lengthfreq_pre <- read.csv("Ess_data/Alida_Qadj_biomass_at_Length.csv") |>
  select( year  = YEAR, species_code = SPECIES,length_cm = LENGTH,Biomass = BIOMASS) |>
  filter(length_cm > 0,Biomass   > 0,year >= 1978,year <= 1992) |>
  mutate(species_name = species_code_lookup[as.character(species_code)]) |>
  filter(!is.na(species_name))

# -----------------------------------------------------------------------------
# 3. Maximum length bin per species
# -----------------------------------------------------------------------------

species_max_bin <- lengthfreq_pre |>
  group_by(species_name) |>
  summarise(max_length = max(length_cm, na.rm = TRUE), .groups = "drop") |>
  mutate(
    length_bin = cut(max_length, breaks = bin_breaks,
     labels = bin_labels,right = TRUE, include.lowest = TRUE) )

# -----------------------------------------------------------------------------
# 4. Total biomass per species
# -----------------------------------------------------------------------------

species_total_biomass <- lengthfreq_pre |> group_by(species_name) |>
  summarise(total_biomass = sum(Biomass, na.rm = TRUE), .groups = "drop")

# -----------------------------------------------------------------------------
# 5. Join and compute proportions within each bin
# -----------------------------------------------------------------------------

bin_by_maxlen <- species_max_bin |>
  left_join(species_total_biomass, by = "species_name") |>
  filter(!is.na(length_bin)) |>
  group_by(length_bin) |>
  mutate(prop = total_biomass / sum(total_biomass)) |> 
  ungroup()

# -----------------------------------------------------------------------------
# 6. Flag dominant species — 80% threshold OR top 3 per bin
# -----------------------------------------------------------------------------

bin_by_maxlen <- bin_by_maxlen |>
  group_by(length_bin) |>
  arrange(length_bin, desc(prop)) |>
  mutate(
    cum_prop = cumsum(prop),
    rank     = row_number(),
    dominant = lag(cum_prop, default = 0) > 0.8 | rank <= 4 ) |>
  mutate(
    cum_at_threshold = max(cum_prop[dominant]),
    display_val      = ifelse(dominant, round(cum_at_threshold, 2), NA_real_)) |>
  ungroup()

# -----------------------------------------------------------------------------
# 7. Bar chart
# -----------------------------------------------------------------------------

facet_data <- bin_by_maxlen |>
  mutate(
    species_name = fct_reorder(species_name, prop, .desc = TRUE),
    dominant     = ifelse(dominant, "Dominant", "Non-dominant"))


# -----------------------------------------------------------------------------
# 8. Wide table — species as COLUMNS, length bins as ROWS
# -----------------------------------------------------------------------------

col_order <- bin_by_maxlen |>
  arrange(max_length, desc(prop)) |>
  distinct(species_name) |>
  pull(species_name)

wide_table <- bin_by_maxlen |>
  select(length_bin, species_name, display_val) |>
  pivot_wider(
    names_from  = species_name,
    values_from = display_val,
    values_fill = NA_real_
  ) |> arrange(length_bin)

ordered_species <- intersect(col_order, colnames(wide_table))
wide_table <- wide_table |>
  select(length_bin, all_of(ordered_species))

species_cols    <- setdiff(colnames(wide_table), "length_bin")
dominant_lookup <- bin_by_maxlen |> select(length_bin, species_name, dominant)

# -----------------------------------------------------------------------------
# 9. GT table
# -----------------------------------------------------------------------------

gt_table_a <- wide_table |>
  gt(rowname_col = "length_bin") |>
  tab_stubhead(label = "Length-class (cm)") |>
tab_header(
 title    = md("**Reference Period (1978–1992)**")
  #subtitle = "Cumulative biomass proportion at 80% threshold — dominant species by maximum length bin"
  ) |>
  fmt_number(columns = all_of(species_cols), decimals = 2) |>
  fmt_missing(columns = all_of(species_cols), missing_text = "—") |>
  tab_style(
    style     = cell_fill(color = "#EEF1F5"),
    locations = cells_body(columns = all_of(species_cols))) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_stub()) |>
  tab_style(
    style     = cell_text(weight = "bold", align = "center"),
    locations = cells_stubhead()) |>
  cols_width(
    stub()               ~ px(85),
    all_of(species_cols) ~ px(72) ) |>
  tab_style(
    style     = cell_text(size = px(12), weight = "bold", align = "center"),
    locations = cells_column_labels()  ) |>
  tab_style(
    style     = cell_text(size = px(10), align = "center"),
    locations = cells_body() ) |>
  tab_options(
    table.font.size                 = 12,
    table.width                     = pct(90),
    heading.align                   = "center",
    column_labels.font.weight       = "bold",
    stub.font.weight                = "bold",
    row.striping.include_table_body = FALSE,
    data_row.padding                = px(5),
    column_labels.padding           = px(5) )

# Highlight dominant cells dark blue
for (sp in species_cols) {
  dominant_bins <- dominant_lookup |>
    filter(species_name == sp, dominant == TRUE) |>
    pull(length_bin) |>
    as.character()
  
  if (length(dominant_bins) > 0) {
    matching_rows <- which(as.character(wide_table$length_bin) %in% dominant_bins)
    
    if (length(matching_rows) > 0) {
      gt_table_a <- gt_table_a |>
        tab_style(style = list(cell_fill(color = "#1B4F8A"),
        cell_text(color = "white", weight = "bold", size = px(10))),
        locations = cells_body( columns = all_of(sp),rows    = matching_rows) )
    } }}

# Legend / source note
gt_table_a 

gtsave(gt_table_a,"gt_table_a.png",vwidth = 1600,vheight = 500, zoom = 2.5 )


### B



# -----------------------------------------------------------------------------
# Species lookup
# -----------------------------------------------------------------------------

species_code_lookup <- c(
  "10"  = "Atlantic Cod",
  "11"  = "Haddock",
  "12"  = "White Hake",
  "14"  = "Silver Hake",
  "16"  = "Pollock",
  "23"  = "Redfish Unseparated",
  "30"  = "Atlantic Halibut",
  "40"  = "American Plaice",
  "60"  = "Atlantic Herring",
  "64"  = "Capelin",
  "70"  = "Atlantic Mackerel",
  "201" = "Thorny Skate",
  "42"  = "Yellowtail Flounder",
  "160" = "Atlantic Argentine",
  "610" = "Sand Lance"
)

# -----------------------------------------------------------------------------
# 1. Bin definitions
# -----------------------------------------------------------------------------

bin_breaks <- seq(10, 190, by = 20)
bin_labels <- paste0("(", seq(10, 170, by = 20), "–", seq(30, 190, by = 20), "]")

# -----------------------------------------------------------------------------
# 2. Load and clean
# -----------------------------------------------------------------------------

lengthfreq_pre <- read.csv("Ess_data/Alida_Qadj_biomass_at_Length.csv") |>
  select(
    year         = YEAR,
    species_code = SPECIES,
    length_cm    = LENGTH,
    Biomass      = BIOMASS
  ) |> filter(length_cm > 0,Biomass   > 0,year >= 1993,year <= 2022) |>
  mutate(species_name = species_code_lookup[as.character(species_code)]) |>
  filter(!is.na(species_name))

# -----------------------------------------------------------------------------
# 3. Maximum length bin per species
# -----------------------------------------------------------------------------

species_max_bin <- lengthfreq_pre |>
  group_by(species_name) |>
  summarise(max_length = max(length_cm, na.rm = TRUE), .groups = "drop") |>
  mutate(
    length_bin = cut(max_length,
                     breaks         = bin_breaks,
                     labels         = bin_labels,
                     right          = TRUE,
                     include.lowest = TRUE))

# -----------------------------------------------------------------------------
# 4. Total biomass per species
# -----------------------------------------------------------------------------

species_total_biomass <- lengthfreq_pre |>
  group_by(species_name) |>
  summarise(total_biomass = sum(Biomass, na.rm = TRUE), .groups = "drop")

# -----------------------------------------------------------------------------
# 5. Join and compute proportions within each bin
# -----------------------------------------------------------------------------
# Step 5 — CORRECTED: global proportions, then bin grouping for ranking only
# Step 5 — REVERT to within-bin proportions (correct)
bin_by_maxlen <- species_max_bin |>
  left_join(species_total_biomass, by = "species_name") |>
  filter(!is.na(length_bin)) |>
  group_by(length_bin) |>
  mutate(prop = total_biomass / sum(total_biomass)) |>  # within-bin denominator
  ungroup()

# Step 6 — ranking and dominance still within bin
bin_by_maxlen <- bin_by_maxlen |>
  group_by(length_bin) |>
  arrange(length_bin, desc(prop)) |>
  mutate(
    cum_prop = cumsum(prop),
    rank     = row_number(),
    dominant = lag(cum_prop, default = 0) > 0.8 | rank <= 4 ) |>
  mutate(
    cum_at_threshold = max(cum_prop[dominant]),
    display_val      = ifelse(dominant, round(cum_at_threshold, 2), NA_real_)) |>
  ungroup()
# -----------------------------------------------------------------------------
# 7. Wide table
# -----------------------------------------------------------------------------

col_order <- bin_by_maxlen |>
  arrange(length_bin, desc(dominant), desc(prop)) |>  # dominant species first within bin
  distinct(species_name) |>
  pull(species_name)

wide_table <- bin_by_maxlen |>
  select(length_bin, species_name, display_val) |>
  pivot_wider(
    names_from  = species_name,
    values_from = display_val,
    values_fill = list(display_val = NA_real_)
  ) |>
  arrange(length_bin)

ordered_species <- intersect(col_order, colnames(wide_table))
wide_table <- wide_table |>
  select(length_bin, all_of(ordered_species))

# Ensure all bins present
all_bins   <- tibble(length_bin = factor(bin_labels, levels = bin_labels))
wide_table <- all_bins |>
  left_join(wide_table, by = "length_bin") |>
  arrange(length_bin)

species_cols    <- setdiff(colnames(wide_table), "length_bin")
dominant_lookup <- bin_by_maxlen |>
  select(length_bin, species_name, dominant, display_val)

# -----------------------------------------------------------------------------
# 8. GT table base
# -----------------------------------------------------------------------------

gt_table_b <- wide_table |>
  gt(rowname_col = "length_bin") |>
  tab_stubhead(label = "Length-class (cm)") |>
tab_header(
title    = md("**Unconstrained Period (1993-2022)**")
# subtitle = "Cumulative biomass proportion at 80% threshold — dominant species by maximum length bin"
) |>
  fmt_number(columns = all_of(species_cols), decimals = 2) |>
  fmt_missing(columns = all_of(species_cols), missing_text = "—") |>
  tab_style(
    style     = cell_fill(color = "#EEF1F5"),
    locations = cells_body(columns = all_of(species_cols))) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_stub()) |>
  tab_style(
    style     = cell_text(weight = "bold", align = "center"),
    locations = cells_stubhead()) |>
  cols_width(
    stub()               ~ px(85),
    all_of(species_cols) ~ px(72) ) |>
  tab_style(
    style     = cell_text(size = px(12), weight = "bold", align = "center"),
    locations = cells_column_labels()  ) |>
  tab_style(
    style     = cell_text(size = px(10), align = "center"),
    locations = cells_body() ) |>
  tab_options(
    table.font.size                 = 12,
    table.width                     = pct(90),
    heading.align                   = "center",
    column_labels.font.weight       = "bold",
    stub.font.weight                = "bold",
    row.striping.include_table_body = FALSE,
    data_row.padding                = px(5),
    column_labels.padding           = px(5) )

# Highlight dominant cells dark blue
for (sp in species_cols) {
  dominant_bins <- dominant_lookup |>
    filter(species_name == sp, dominant == TRUE) |>
    pull(length_bin) |>
    as.character()
  
  if (length(dominant_bins) > 0) {
    matching_rows <- which(as.character(wide_table$length_bin) %in% dominant_bins)
    
    if (length(matching_rows) > 0) {
      gt_table_b <- gt_table_b |>
        tab_style(style = list(cell_fill(color = "#1B4F8A"),
                               cell_text(color = "white", weight = "bold", size = px(10))),
                  locations = cells_body( columns = all_of(sp),rows    = matching_rows) )
    } }}

# Legend / source note
gt_table_b 

# -----------------------------------------------------------------------------
# 10. Legend / source note
# -----------------------------------------------------------------------------

gt_table_b <- gt_table_b |>
  tab_source_note(
    source_note = md(
      "<div style='text-align:center; padding: 6px 0;'>
       <strong>Cumulative biomass contribution by maximum length bins:</strong> &nbsp;&nbsp;
       <span style='background:#C8DCF0; padding:3px 14px; font-size:11px;'>Less than 80%</span> &nbsp;
       <span style='background:#1B4F8A; padding:3px 14px; color:white; font-size:11px;'>Greater than 80%</span>
       </div>"
    )
  ) |>
  tab_options(
    source_notes.background.color = "white",
    source_notes.font.size        = px(10),
    source_notes.padding          = px(8)
  )

gt_table_b

gtsave(gt_table_b,"gt_table_b.png",vwidth = 1600,vheight = 500, zoom = 2.5 )
