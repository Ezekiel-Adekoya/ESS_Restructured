# =============================================================================
# Biomass-at-Length GT Table
# Eastern Scotian Shelf  |  Reference (1978–1992) & Unconstrained (1993–2022)
# =============================================================================

library(dplyr)
library(tidyr)
library(gt)
library(glue)

# -----------------------------------------------------------------------------
# Constants
# -----------------------------------------------------------------------------

DOM_THRESHOLD <- 0.95
BIN_MIN       <- 10
BIN_MAX       <- 190
BIN_WIDTH     <- 20

COLOUR_DOMINANT    <- "#1B4F8A"
COLOUR_NONDOMINANT <- "#EEF1F5"

SPECIES_LOOKUP <- c(
  "10"  = "Atlantic Cod",       "11"  = "Haddock",
  "12"  = "White Hake",         "14"  = "Silver Hake",
  "16"  = "Pollock",            "23"  = "Redfish Unseparated",
  "30"  = "Atlantic Halibut",   "40"  = "American Plaice",
  "60"  = "Atlantic Herring",   "64"  = "Capelin",
  "70"  = "Atlantic Mackerel",  "201" = "Thorny Skate",
  "42"  = "Yellowtail Flounder","160" = "Atlantic Argentine",
  "610" = "Sand Lance"
)

bin_breaks <- seq(BIN_MIN, BIN_MAX, by = BIN_WIDTH)
bin_labels <- paste0(
  "(", seq(BIN_MIN,             BIN_MAX - BIN_WIDTH, by = BIN_WIDTH),
  "-", seq(BIN_MIN + BIN_WIDTH, BIN_MAX,             by = BIN_WIDTH), "]"
)

# -----------------------------------------------------------------------------
# Core function
# -----------------------------------------------------------------------------

build_gt_table <- function(year_min, year_max, title) {
  
  # 1. Load & clean ----
  df <- read.csv("Ess_data/Alida_Qadj_biomass_at_Length.csv") |>
    filter(ID == "ESS", LENGTH > 0, BIOMASS > 0,
           YEAR >= year_min, YEAR <= year_max) |>
    transmute(
      species_name = SPECIES_LOOKUP[as.character(SPECIES)],
      length_cm    = LENGTH,
      biomass      = BIOMASS
    ) |>
    filter(!is.na(species_name)) |>
    mutate(length_cm = if_else(
      species_name == "Atlantic Herring" & length_cm > 50, NA_real_, length_cm
    )) |>
    filter(!is.na(length_cm))
  
  # 2. Species summary → length bin ----
  species_summary <- df |>
    group_by(species_name) |>
    summarise(
      max_length    = max(length_cm, na.rm = TRUE),
      total_biomass = sum(biomass,   na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      length_bin = cut(
        pmin(max_length, BIN_MAX),
        breaks = bin_breaks, labels = bin_labels,
        right = TRUE, include.lowest = TRUE
      )
    ) |>
    filter(!is.na(length_bin))
  
  # 3. Rank, cumulate, flag dominant ----
  bin_summary <- species_summary |>
    group_by(length_bin) |>
    arrange(desc(total_biomass), .by_group = TRUE) |>
    mutate(
      prop     = total_biomass / sum(total_biomass),
      cum_prop = cumsum(prop),
      dominant = lag(cum_prop, default = 0) < DOM_THRESHOLD
    ) |>
    mutate(
      display_val = if_else(
        dominant,
        round(if (any(dominant)) min(cum_prop[dominant]) else NA_real_, 2),
        NA_real_
      )
    ) |>
    ungroup()
  
  # 4. Column ordering ----
  dominant_species <- bin_summary |>
    filter(dominant) |>
    group_by(species_name) |>
    filter(n_distinct(length_bin) >= 1) |>
    pull(species_name) |>
    unique()
  
  col_order <- bin_summary |>
    filter(species_name %in% dominant_species) |>
    arrange(length_bin, desc(prop)) |>
    distinct(species_name) |>
    pull(species_name)
  
  # 5. Wide table ----
  wide_table <- tidyr::crossing(
    length_bin   = factor(bin_labels, levels = bin_labels),
    species_name = dominant_species
  ) |>
    left_join(
      bin_summary |> select(length_bin, species_name, display_val),
      by = c("length_bin", "species_name")
    ) |>
    pivot_wider(names_from = species_name, values_from = display_val,
                values_fill = NA_real_) |>
    right_join(tibble(length_bin = factor(bin_labels, levels = bin_labels)),
               by = "length_bin") |>
    arrange(length_bin)
  
  species_cols <- intersect(col_order, colnames(wide_table))
  wide_table   <- wide_table |> select(length_bin, all_of(species_cols))
  
  # 6. GT base ----
  gt_tbl <- wide_table |>
    gt(rowname_col = "length_bin") |>
    tab_stubhead(label = "Length-class (cm)") |>
    tab_header(title = md(glue("**{title}**"))) |>
    fmt_number(columns = all_of(species_cols), decimals = 2) |>
    fmt_missing(columns = all_of(species_cols), missing_text = "-") |>
    tab_style(cell_fill(COLOUR_NONDOMINANT),
              cells_body(all_of(species_cols))) |>
    tab_style(cell_text(size = px(12), weight = "bold", align = "center"),
              cells_column_labels()) |>
    tab_style(cell_text(weight = "bold"), cells_stub()) |>
    tab_style(cell_text(weight = "bold", align = "center"), cells_stubhead()) |>
    tab_style(cell_text(size = px(10), align = "center"), cells_body()) |>
    cols_width(stub() ~ px(85), everything() ~ px(72)) |>   # <-- fixed here
    tab_options(
      table.font.size = 12, table.width = pct(90),
      heading.align = "center",
      column_labels.font.weight = "bold", column_labels.padding = px(5),
      stub.font.weight = "bold",
      row.striping.include_table_body = FALSE, data_row.padding = px(5)
    )
  
  # 7. Highlight dominant cells ----
  dominant_rows <- bin_summary |>
    filter(dominant, species_name %in% species_cols) |>
    mutate(row_idx = match(as.character(length_bin),
                           as.character(wide_table$length_bin))) |>
    filter(!is.na(row_idx)) |>
    group_by(species_name) |>
    summarise(rows = list(row_idx), .groups = "drop") |>
    tibble::deframe()
  
  for (sp in names(dominant_rows)) {
    gt_tbl <- gt_tbl |>
      tab_style(
        style = list(cell_fill(COLOUR_DOMINANT),
                     cell_text(color = "white", weight = "bold", size = px(10))),
        locations = cells_body(columns = all_of(sp), rows = dominant_rows[[sp]])
      )
  }
  
  # 8. Legend ----
  gt_tbl |>
    tab_source_note(source_note = md(glue(
      "<div style='text-align:center; padding:6px 0;'>
       <strong>Cumulative biomass contribution by maximum length bins:</strong>&nbsp;&nbsp;
       <span style='background:{COLOUR_NONDOMINANT}; padding:3px 14px; font-size:11px;'>
         Less than {DOM_THRESHOLD * 100}%</span>&nbsp;
       <span style='background:{COLOUR_DOMINANT}; padding:3px 14px;
             color:white; font-size:11px;'>
         Greater than or equal to {DOM_THRESHOLD * 100}%</span>
       </div>"
    ))) |>
    tab_options(source_notes.background.color = "white",
                source_notes.font.size = px(10), source_notes.padding = px(8))
}

# -----------------------------------------------------------------------------
# Build & save both tables
# -----------------------------------------------------------------------------

periods <- list(
  list(min = 1978, max = 1992, title = "Reference Period (1978–1992)",     file = "gt_table_a.png"),
  list(min = 1993, max = 2022, title = "Unconstrained Period (1993–2022)", file = "gt_table_b.png")
)

tables <- lapply(periods, \(p) {
  tbl <- build_gt_table(p$min, p$max, p$title)
  gtsave(tbl, p$file, vwidth = 1600, vheight = 500, zoom = 2.5)
  tbl
})

gt_table_a <- tables[[1]]
gt_table_b <- tables[[2]]

gt_table_a
gt_table_b