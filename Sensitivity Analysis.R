library(tidyverse)
uncorrected_biomass <- read.csv("Ess_data/esswss_lengthbased_notqadj.csv")

target_codes <- c(10, 11, 203, 14, 16, 23, 40, 42, 60, 64, 70, 31, 200, 610, 30)

species_map <- c(
  "10" = "Atlantic Cod", "11" = "Haddock", "203" = "White Hake",
  "14" = "Silver Hake", "16" = "Pollock", "23" = "Redfish Unseparated",
  "40" = "American Plaice", "42" = "Yellowtail Flounder", "60" = "Atlantic Herring",
  "64" = "Capelin", "70" = "Atlantic Mackerel", "31" = "Atlantic Argentine",
  "200" = "Thorny Skate", "610" = "Sand Lance", "30" = "Atlantic Halibut"
)

uncorrected_biomass <- uncorrected_biomass |>
  dplyr::filter(SPECIES %in% target_codes) |>
  dplyr::mutate(SPECIES_NAME_CLEAN = dplyr::recode(as.character(SPECIES), !!!species_map)) |>
  dplyr::group_by(YEAR, SPECIES_NAME_CLEAN) |>
  dplyr::summarise(BIOMASS = sum(BIOMASS, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(
    names_from  = SPECIES_NAME_CLEAN,
    values_from = BIOMASS,
    values_fill = 0
  ) |>
  dplyr::mutate(dplyr::across(where(is.numeric), as.integer))



corrected_biomass <- read.csv("Ess_data/ESS q-adj data for Ezekiel 1970-2022.csv")

target_codes <- c(10, 11, 203, 14, 16, 23, 40, 42, 60, 64, 70, 31, 200, 610, 30)

species_map <- c(
  "10" = "Atlantic Cod", "11" = "Haddock", "203" = "White Hake",
  "14" = "Silver Hake", "16" = "Pollock", "23" = "Redfish Unseparated",
  "40" = "American Plaice", "42" = "Yellowtail Flounder", "60" = "Atlantic Herring",
  "64" = "Capelin", "70" = "Atlantic Mackerel", "31" = "Atlantic Argentine",
  "200" = "Thorny Skate", "610" = "Sand Lance", "30" = "Atlantic Halibut"
)



corrected_biomass <- corrected_biomass %>%
  dplyr::filter(SPECIES %in% target_codes) %>%
  dplyr::mutate(SPECIES_NAME_CLEAN = dplyr::recode(as.character(SPECIES), !!!species_map)) %>%
  dplyr::group_by(YEAR, SPECIES_NAME_CLEAN) %>%
  dplyr::summarise(BIOMASS = sum(BIOMASS, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from  = SPECIES_NAME_CLEAN,
    values_from = BIOMASS,
    values_fill = 0
  ) %>%
  dplyr::mutate(
    YEAR = as.integer(YEAR),
    dplyr::across(-YEAR, ~ round(.x, 0))
  )


common_years <- intersect(uncorrected_biomass$YEAR, corrected_biomass$YEAR)


# ── Correlation per species (for facet labels) ────────────────────────────────
sensitivity_summary <- sensitivity_long %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(
    r = cor(Uncorrected, Corrected, use = "complete.obs"),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    label = paste0("bold(r) == ", round(r, 2))
  )

# ── Plot ──────────────────────────────────────────────────────────────────────
p <- sensitivity_long %>%
  pivot_longer(c(Uncorrected, Corrected),
               names_to = "Method", values_to = "Biomass") %>%
  dplyr::mutate(
    Method = dplyr::recode(Method,"Corrected" = "Q-Corrected")) %>%
  ggplot(aes(x = YEAR, y = Biomass / 1e6,colour = Method,linetype = Method)) +
  geom_ribbon(
    data = sensitivity_long,
    aes(x = YEAR,
        ymin = pmin(Uncorrected, Corrected) / 1e6,
        ymax = pmax(Uncorrected, Corrected) / 1e6),
    inherit.aes = FALSE,fill = "#2C7FB8",alpha = 0.15) +
  geom_line(linewidth = 0.8) +
  geom_text(
    data = sensitivity_summary,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.05,
    vjust = 1.2,
    size = 3,
    parse = TRUE ) +
  
  scale_colour_manual(values = c("Q-Corrected" = "#2C7FB8", "Uncorrected" = "red")) +
  
  scale_linetype_manual(values = c(
    "Q-Corrected" = "solid","Uncorrected" = "dashed")) +
  
  facet_wrap(~ Species, scales = "free_y", ncol = 4) +
  
  labs( x = NULL, y = expression("Biomass (×10"^6*")"),
    colour = NULL,
    linetype = NULL) +
  
  theme_classic(base_size = 10) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 8, face = "bold"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 9),
    panel.spacing = unit(0.5, "lines")
  )

# ── Save TIFF ────────────────────────────────────────────────────────────────
ggsave(
  filename = "sensitivity_plot.tiff",
  plot = p,
  width = 9,
  height = 6,
  dpi = 300,
  compression = "lzw")





library(tidyverse)
library(ggplot2)

# ── Correlation + log bias summary per species ────────────────────────────────
sensitivity_summary <- sensitivity_long %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(
    r        = cor(Uncorrected, Corrected, use = "complete.obs"),
    log_bias = mean(log(Corrected / Uncorrected), na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  dplyr::mutate(
    label = paste0(
      "bold(italic(r))==bold(", round(r, 2), ")",
      "*','~~",
      "bold(italic(bias))==bold(", round(log_bias, 2), ")"
    )
  )

# ── Uncertainty ribbon ────────────────────────────────────────────────────────
sensitivity_ribbon <- sensitivity_long %>%
  dplyr::mutate(
    mid  = (Corrected + Uncorrected) / 2,
    half = abs(Corrected - Uncorrected) / 2
  ) %>%
  dplyr::group_by(Species, YEAR) %>%
  dplyr::summarise(
    mid  = mean(mid,  na.rm = TRUE),
    half = mean(half, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    lo = pmax(mid - half, 0),
    hi = mid + half
  )

# ── Plot ───────────────────────────────────────────────────────────────────────
p <- sensitivity_long %>%
  pivot_longer(c(Uncorrected, Corrected),
               names_to  = "Method",
               values_to = "Biomass") %>%
  dplyr::mutate(
    Method = dplyr::recode(Method, "Corrected" = "Q-Corrected"),
    Method = factor(Method, levels = c("Q-Corrected", "Uncorrected"))
  ) %>%
  ggplot(aes(x = YEAR, y = Biomass / 1e6, colour = Method, linetype = Method)) +
  
  # Uncertainty ribbon
  geom_ribbon(
    data = sensitivity_ribbon,
    aes(x = YEAR, ymin = lo / 1e6, ymax = hi / 1e6),
    inherit.aes = FALSE,
    fill  = "#2C7FB8",
    alpha = 0.07) +
  
  geom_line(linewidth = 0.4) +
  
  # Stats annotation — italic, top right
  geom_text(
    data        = sensitivity_summary,
    aes(x = Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust       = 1.05,
    vjust       = 1.5,
    size        = 2,
    parse       = TRUE,
    colour      = "black") +
  
  scale_colour_manual(
    values = c("Q-Corrected" = "#2C7FB8", "Uncorrected" = "#D73027"),
    labels = c("Q-Corrected", "Uncorrected")
  ) +
  
  scale_linetype_manual(
    values = c("Q-Corrected" = "solid", "Uncorrected" = "dashed"),
    labels = c("Q-Corrected", "Uncorrected") ) +
  
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 4),
    expand = expansion(mult = c(0.02, 0.02)) ) +
  
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 4),
    expand = expansion(mult = c(0.02, 0.08)) ) +
  
  facet_wrap(~ Species, scales = "free_y", ncol = 4) +
  
  labs( x = "Year",y = expression(paste("Biomass (", 10^6, " t)")),
    colour   = NULL,
    linetype = NULL ) +
  
  theme_classic(base_size = 8) +
  theme(
    # Legend
    legend.position   = "bottom",
    legend.key.width  = unit(1.5, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.text       = element_text(size = 9),
    legend.margin     = margin(t = 2, b = 2),
    legend.spacing.x  = unit(0.3, "cm"),
    
    # Facet strips — boxed look
    strip.text        = element_text(size = 7, face = "bold",
                                     margin = margin(b = 4, t = 4)),
    strip.background  = element_rect(fill = "grey92", 
                                     colour = "grey40", 
                                     linewidth = 0.5),  # <-- shaded box header
    
    # Axes
    axis.text         = element_text(size = 7, colour = "black"),
    axis.title.y      = element_text(size = 9, margin = margin(r = 6)),
    axis.title.x      = element_text(size = 9, margin = margin(t = 6)),
    axis.line         = element_line(linewidth = 0.4, colour = "black"),
    axis.ticks        = element_line(linewidth = 0.4, colour = "black"),
    axis.ticks.length = unit(0.1, "cm"),
    
    # Panels — full box around each species plot
    panel.spacing     = unit(0.6, "lines"),
    panel.border      = element_rect(colour = "grey40",  fill = NA,linewidth = 0.5),
    panel.background  = element_rect(fill = "white",      # <-- white background
                                     colour = NA),
    
    # Overall
    plot.margin       = margin(8, 10, 5, 8) )

# ── Save TIFF (600 dpi, 2-column journal width) ───────────────────────────────
ggsave(
  filename    = "sensitivity_plot.tiff",
  plot        = p,
  width       = 180,
  height      = 130,
  units       = "mm",
  dpi         = 600,
  compression = "lzw"
)

# ── Save PDF (vector format for journal submission) ───────────────────────────
ggsave(
  filename = "sensitivity_plot.pdf",
  plot     = p,
  width    = 180,
  height   = 130,
  units    = "mm")