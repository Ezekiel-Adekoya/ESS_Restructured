# =============================================================================
# Sensitivity analysis: uncorrected vs. q-corrected biomass estimates
# =============================================================================

library(tidyverse)

# -----------------------------------------------------------------------------
# Config
# -----------------------------------------------------------------------------

TARGET_SPECIES <- c(10, 11, 14, 16, 23, 30, 40, 42, 60, 64, 70, 160, 201, 12, 610)

SPECIES_LABELS <- c(
  "10"  = "Atlantic Cod",
  "11"  = "Haddock",
  "14"  = "Silver Hake",
  "16"  = "Pollock",
  "23"  = "Redfish Unseparated",
  "30"  = "Atlantic Halibut",
  "40"  = "American Plaice",
  "42"  = "Yellowtail Flounder",
  "60"  = "Atlantic Herring",
  "64"  = "Capelin",
  "70"  = "Atlantic Mackerel",
  "160" = "Atlantic Argentine",
  "201" = "Thorny Skate",
  "12"  = "White Hake",
  "610" = "Sand Lance"
)

# Shared plot colours and line types
METHOD_COLOURS   <- c("Q-Corrected" = "#2C7FB8", "Uncorrected" = "#D73027")
METHOD_LINETYPES <- c("Q-Corrected" = "solid",   "Uncorrected" = "dashed")

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

#' Read and reshape a biomass CSV into a wide-format annual summary.
#'
#' @param path    Path to CSV file.
#' @param integer If TRUE, rounds values to integer (use for uncorrected data).
read_biomass <- function(path, integer = FALSE) {
  read.csv(path) |>
    filter(SPECIES %in% TARGET_SPECIES) |>
    mutate(SPECIES_NAME_CLEAN = recode(as.character(SPECIES), !!!SPECIES_LABELS)) |>
    group_by(YEAR, SPECIES_NAME_CLEAN) |>
    summarise(BIOMASS = sum(BIOMASS, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = SPECIES_NAME_CLEAN, values_from = BIOMASS, values_fill = 0) |>
    mutate(
      YEAR = as.integer(YEAR),
      across(-YEAR, if (integer) as.integer else ~ round(.x, 0))
    )
}

# -----------------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------------

biomass_raw <- read_biomass("Ess_data/esswss_lengthbased_notqadj.csv", integer = TRUE)
biomass_qcr <- read_biomass("Ess_data/ESS q-adj data for Ezekiel 1970-2022.csv")

# Long format over shared years only
sensitivity_long <- inner_join(
  pivot_longer(biomass_raw, -YEAR, names_to = "Species", values_to = "Uncorrected"),
  pivot_longer(biomass_qcr, -YEAR, names_to = "Species", values_to = "Corrected"),
  by = c("YEAR", "Species")
)

# -----------------------------------------------------------------------------
# Summary stats (correlation + log bias per species)
# -----------------------------------------------------------------------------

sensitivity_summary <- sensitivity_long |>
  group_by(Species) |>
  summarise(
    r        = cor(Uncorrected, Corrected, use = "complete.obs"),
    log_bias = mean(log(Corrected / Uncorrected), na.rm = TRUE),
    .groups  = "drop"
  ) |>
  mutate(
    label = glue::glue(
      "bold(italic(r))==bold({round(r, 2)})*','~~bold(italic(bias))==bold({round(log_bias, 2)})"
    )
  )

# Uncertainty ribbon: midpoint ± half-width of the gap between methods
sensitivity_ribbon <- sensitivity_long |>
  mutate(
    mid  = (Corrected + Uncorrected) / 2,
    half = abs(Corrected - Uncorrected) / 2
  ) |>
  group_by(Species, YEAR) |>
  summarise(across(c(mid, half), mean, na.rm = TRUE), .groups = "drop") |>
  mutate(lo = pmax(mid - half, 0), hi = mid + half)

# -----------------------------------------------------------------------------
# Plot
# -----------------------------------------------------------------------------

p <- sensitivity_long |>
  pivot_longer(c(Uncorrected, Corrected), names_to = "Method", values_to = "Biomass") |>
  mutate(
    Method = recode(Method, "Corrected" = "Q-Corrected"),
    Method = factor(Method, levels = c("Q-Corrected", "Uncorrected"))
  ) |>
  ggplot(aes(YEAR, Biomass / 1e6, colour = Method, linetype = Method)) +
  
  geom_ribbon(
    data        = sensitivity_ribbon,
    aes(x = YEAR, ymin = lo / 1e6, ymax = hi / 1e6),
    inherit.aes = FALSE,fill = "#2C7FB8", alpha       = 0.07) +
  
  geom_line(linewidth = 0.6) +
  
  geom_text(
    data        = sensitivity_summary,
    aes(x = Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust       = 1.05,
    vjust       = 1.5,
    size        = 2,
    parse       = TRUE,
    colour      = "black"
  ) +
  
  scale_colour_manual(values = METHOD_COLOURS)  +
  scale_linetype_manual(values = METHOD_LINETYPES) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 4), expand = expansion(mult = 0.02)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4), expand = expansion(mult = c(0.02, 0.08))) +
  
  facet_wrap(~ Species, scales = "free_y", ncol = 4) +
  
  labs(
    x        = "Year",
    y        = expression(paste("Biomass (", 10^6, " t)")),
    colour   = NULL,
    linetype = NULL) +
  
  theme_classic(base_size = 8) +
  theme(
    # Legend
    legend.position   = "bottom",
    legend.key.width  = unit(1.5, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.text       = element_text(size = 9),
    legend.margin     = margin(t = 2, b = 2),
    legend.spacing.x  = unit(0.3, "cm"),
    
    # Facet strips
    strip.text        = element_text(size = 7, face = "bold", margin = margin(b = 4, t = 4)),
    strip.background  = element_rect(fill = "grey92", colour = "grey40", linewidth = 0.5),
    
    # Axes
    axis.text         = element_text(size = 7, colour = "black"),
    axis.title.y      = element_text(size = 9, margin = margin(r = 6)),
    axis.title.x      = element_text(size = 9, margin = margin(t = 6)),
    axis.line         = element_line(linewidth = 0.4, colour = "black"),
    axis.ticks        = element_line(linewidth = 0.4, colour = "black"),
    axis.ticks.length = unit(0.1, "cm"),
    
    # Panels
    panel.spacing     = unit(0.6, "lines"),
    panel.border      = element_rect(colour = "grey40", fill = NA, linewidth = 0.5),
    panel.background  = element_rect(fill = "white", colour = NA),
    
    plot.margin       = margin(8, 10, 5, 8)
  )

# -----------------------------------------------------------------------------
# Export
# -----------------------------------------------------------------------------
p
ggsave("Ess_Output/sensitivity_plot.jpeg", p, width = 180, height = 130, units = "mm",
       dpi = 600)

ggsave("Ess_Output/sensitivity_plot.pdf",  p, width = 180, height = 130, units = "mm")
