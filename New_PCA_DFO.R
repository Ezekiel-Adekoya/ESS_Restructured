library(tidyverse)
library(tidyr)
library(scales)
library(patchwork)

# ─────────────────────────────────────────────
# 1. LOAD & FILTER
# ─────────────────────────────────────────────
stat <- read.csv("Ess_data/LF_Biomass_Abund.csv")

# need to exclude deep water shelf edge strata, I think the ones to include are 440–466 
# but we can change as needed depending on what Alida suggests as there might be a good 
# reason to exclude 4Vn which are strata 440-442  ).
# ─────────────────────────────────────────────
# 2. WIDE BIOMASS TABLE
# ─────────────────────────────────────────────
inv_keywords <- paste(
  "CRAB|LOBSTER|SCALLOP|SQUID|OCTOPUS|JELLYFISH|AMPHIPODA|CRUSTACEA|CEPHALOPODA|MUNIDA|SHRIMP",
  collapse = "|")

biomass_wide <- stat %>%
  filter(!is.na(STRAT), STRAT != "") %>%
  filter(suppressWarnings(as.numeric(STRAT)) >= 440,
         suppressWarnings(as.numeric(STRAT)) <= 466) %>%
  # remove invertebrates here
  filter(!grepl(inv_keywords, COMMON_NAME)) %>%
  group_by(YEAR, COMMON_NAME) %>%
  summarise(
    Biomass_Strata_KG = sum(Biomass_Strata_KG, na.rm = TRUE),
    .groups = "drop") %>%
  pivot_wider(
    names_from  = COMMON_NAME,
    values_from = Biomass_Strata_KG,
    values_fill = 0) %>%
  arrange(YEAR)
write.csv(biomass_wide, "Ess_data/DFO_Biomas_all_species", row.names = FALSE)
# ─────────────────────────────────────────────
# 3. PHASE LABELS  — single source of truth
# ─────────────────────────────────────────────
# ─────────────────────────────────────────────
PHASE_REF  <- "1979–1992 (Reference Period)"
PHASE_POST <- "1993–2025 (Post-Moratorium Period)"

# ─────────────────────────────────────────────
# 4. LONG PHASE DATA
# ─────────────────────────────────────────────
long_df_phase <- biomass_wide %>%
  filter(between(YEAR, 1979, 2025)) %>%
  mutate(Phase = factor(
    if_else(YEAR <= 1992, PHASE_REF, PHASE_POST),
    levels = c(PHASE_REF, PHASE_POST)
  )) %>%
  pivot_longer(-c(YEAR, Phase), names_to = "Species", values_to = "BIOMASS") %>%
  filter(!is.na(BIOMASS), BIOMASS > 0) %>%
  group_by(Species) %>%
  filter(n() >= 5) %>%
  ungroup()

# ─────────────────────────────────────────────
# 5. PHASE PERCENTAGE SUMMARY
# ─────────────────────────────────────────────
phase_pct <- long_df_phase %>%
  group_by(Phase, Species) %>%
  summarise(BIOMASS = sum(BIOMASS), .groups = "drop") %>%
  group_by(Phase) %>%
  mutate(Pct = BIOMASS / sum(BIOMASS)) %>%
  ungroup() %>%
  arrange(Phase, desc(Pct)) %>%
  group_by(Phase) %>%
  mutate(
    Rank   = row_number(),
    CumPct = cumsum(Pct)
  ) %>%
  filter(is.finite(Pct)) %>%
  mutate(Species = str_to_sentence(str_squish(Species))) %>%
  ungroup()

# ─────────────────────────────────────────────
# 6. COLOUR PALETTE  — keyed to phase constants
# ─────────────────────────────────────────────
pal <- c(
  `1979–1992 (Reference Period)`      = "#E07B2A",
  `1993–2025 (Post-Moratorium Period)` = "#2471A3"
)

# ─────────────────────────────────────────────
# 7. PLOT FUNCTION
# ─────────────────────────────────────────────
make_phase_plot <- function(ph, df = phase_pct) {
  col <- pal[[ph]]
  df_ph <- df %>%
    filter(Phase == ph) %>%
    mutate(Species = factor(Species, levels = rev(unique(Species))))
  
  # 95 % cumulative cut-off
  cut_rank <- df_ph %>%
    filter(CumPct >= 0.95) %>%
    slice(1) %>%
    pull(Rank)
  
  if (length(cut_rank) == 0 || !is.finite(cut_rank)) cut_rank <- nrow(df_ph)
  
  df_ph <- df_ph %>%
    mutate(Above95 = Rank <= cut_rank)
  
  ypos <- as.numeric(df_ph$Species[df_ph$Rank == cut_rank][1]) - 0.5
  
  ggplot(df_ph, aes(Pct, Species)) +
    annotate("rect",
             xmin = -Inf, xmax = Inf, ymin = ypos, ymax = Inf,
             fill = col, alpha = 0.06) +
    geom_col(aes(alpha = Above95),
             fill = col, colour = "white", linewidth = 0.2, width = 0.85) +
    scale_alpha_manual(values = c("TRUE" = 0.85, "FALSE" = 0.35), guide = "none") +
    geom_hline(yintercept = ypos, colour = col, linetype = "dashed", linewidth = 0.7) +
    annotate("label",
             x = Inf, y = ypos,
             label   = "← 95% cumulative biomass",
             colour  = col, fill = "white",
             label.padding = unit(0.18, "lines"),
             hjust = 1.05, vjust = -0.3,
             size = 2.6, fontface = "bold") +
    scale_x_continuous(
      labels = percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.30))
    ) +
    labs(title = ph, x = "% of total biomass", y = NULL) +
    theme_classic(base_size = 10, base_family = "serif") +
    theme(
      panel.border       = element_rect(colour = col, fill = NA, linewidth = 1.1),
      plot.background    = element_rect(fill = "white", colour = col, linewidth = 0.4),
      plot.title         = element_text(face = "bold", size = 10, colour = col,
                                        margin = margin(b = 6)),
      axis.text.y        = element_text(size = 7.5, colour = "grey15"),
      axis.text.x        = element_text(size = 7.0, colour = "grey40"),
      axis.title.x       = element_text(size = 7.5, colour = "grey40", margin = margin(t = 4)),
      axis.ticks         = element_line(colour = "grey70", linewidth = 0.3),
      axis.line          = element_blank(),
      panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.margin        = margin(10, 16, 10, 10)
    )
}

# ─────────────────────────────────────────────
# 8. ASSEMBLE & DISPLAY
# ─────────────────────────────────────────────
p <- make_phase_plot(PHASE_REF) +
  make_phase_plot(PHASE_POST) +
  plot_layout(ncol = 2) +
  plot_annotation(
    title    = "Scotian Shelf Groundfish Community Biomass Composition",
    #subtitle = "Strata 440–466 · DFO Summer Bottom Trawl Survey",
    theme = theme(
      plot.title      = element_text(face = "bold", size = 13, family = "serif",
                                     hjust = 0.5, margin = margin(b = 4)),
      plot.subtitle   = element_text(colour = "grey45", size = 9, family = "serif",
                                     hjust = 0.5, margin = margin(b = 12)),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

p
ggsave("Ess_data/species_contri.jpeg",
       plot   = p,
       width  = 14,
       height = 10,
       dpi    = 300,
       device = "jpeg")

# ─────────────────────────────────────────────
# 4. PREPARE MATRIX
# ─────────────────────────────────────────────
long_wide <- biomass_wide %>% filter(!YEAR %in% c(2018, 2021, 2022)) # outliers
years     <- long_wide$YEAR
pl.data   <- long_wide %>% select(-YEAR)
pl.data   <- as.data.frame(lapply(pl.data, as.numeric))
rownames(pl.data) <- years

# ── 5. REMOVE SPECIES PRESENT IN FEWER THAN 2 YEARS ──
pl.data[is.na(pl.data)] <- 0
pl.data <- pl.data[, colSums(pl.data > 0) >= 2]

# ── 6. PCA ──
pl.data.log <- log1p(pl.data)
res.pca     <- prcomp(pl.data.log, center = TRUE, scale. = TRUE)
PoV         <- res.pca$sdev^2 / sum(res.pca$sdev^2)

# ── 7. SCORES + CLUSTERING ──
pca_2d         <- res.pca$x[, 1:2]
scores         <- as.data.frame(res.pca$x)
scores$Year    <- as.integer(rownames(scores))
d              <- dist(pca_2d, method = "euclidean")
hc             <- hclust(d, method = "ward.D2")
scores$Cluster <- factor(cutree(hc, k = 2))

# ── 8. PLOT ──
cluster_colours <- c("1" = "#2166AC", "2" = "#D6604D")

p <- ggplot(scores, aes(PC1, PC2)) +
  stat_ellipse(aes(fill = Cluster, color = Cluster),
               geom      = "polygon",
               alpha     = 0.08,
               linetype  = "dashed",
               linewidth = 0.4) +
  geom_point(aes(color = Cluster),
             shape = 16,
             size  = 2,
             alpha = 0.85) +
  geom_text_repel(aes(label = Year, color = Cluster),
                  size          = 2.9,
                  fontface      = "bold",
                  max.overlaps  = 50,
                  box.padding   = 0.35,
                  segment.color = NA,
                  show.legend   = FALSE) +
  scale_color_manual(values = cluster_colours, name = "Cluster") +
  scale_fill_manual(values  = cluster_colours, guide = "none") +
  labs(
    x = paste0("PC1  (", round(PoV[1] * 100, 1), "% variance explained)"),
    y = paste0("PC2  (", round(PoV[2] * 100, 1), "% variance explained)")
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.title       = element_text(face = "bold"),
    axis.text        = element_text(color = "black"),
    legend.title     = element_text(face = "bold", size = 10),
    legend.text      = element_text(size = 9),
    legend.position  = "right",
    panel.border     = element_rect(colour = "grey70", fill = NA, linewidth = 0.6),
    plot.background  = element_blank(),
    plot.margin      = margin(12, 12, 12, 12)
  )
p
ggsave("Ess_data/species_cluster.jpeg",
       plot   = p,
       width  = 14,
       height = 10,
       dpi    = 300,
       device = "jpeg")
## PCA

###################################################################################
# 9. PCA INDIVIDUALS (contribution coloured)
###################################################################################
fviz_pca_ind(res.pca,
             col.ind       = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel         = TRUE,
             title         = "PCA Individuals — Contribution")

###################################################################################
# 10. PCA VARIABLES (contribution coloured)
###################################################################################
fviz_pca_var(res.pca,
             col.var       = "contrib",
             gradient.cols = c("#36648B", "#FFA500", "#8B2500"),
             repel         = TRUE,
             title         = "PCA Variables — Contribution")







# COD(ATLANTIC) is column 9 (index includes YEAR as col 1)
species_col <- colnames(biomass_wide)[9]

ggplot(biomass_wide, aes(x = YEAR, y = .data[[species_col]] / 1e6)) +
  geom_line(color = "#2166AC", linewidth = 0.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = species_col,
    x     = "Year",
    y     = "Biomass (KG)") +
  theme_classic(base_size = 13) +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.5),
    axis.title   = element_text(face = "bold"),
    axis.text    = element_text(color = "black"),
    panel.border = element_rect(colour = "grey70", fill = NA, linewidth = 0.6))



stat <- read.csv("Ess_data/LF_Biomass_Abund.csv")
lengthfreq  <- stat %>%
  filter(!is.na(STRAT), STRAT != "") %>%
  filter(suppressWarnings(as.numeric(STRAT)) >= 440,
         suppressWarnings(as.numeric(STRAT)) <= 466) %>%
  filter(SPEC < 1000) %>%
  group_by(YEAR, SPEC, COMMON_NAME, FLEN) %>%
  summarise(
    Abundance_Strata = sum(Abundance_Strata, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(YEAR, SPEC, FLEN)

write.csv(lengthfreq, "Ess_data/DFO_Abundance_at_length", row.names = FALSE)


lengthfreq


abundance_long <- abundance_long %>%
  select(year = YEAR, species_name = COMMON_NAME, length_cm = FLEN, abundance = Abundance_Strata) %>%
  filter(!is.na(species_name), length_cm > 0) %>%        # drop -99 or NA lengths
  summarise(
    meanN = mean(abundance, na.rm = TRUE),
    .by = c(species_name, length_cm)
  )

p <- ggplot(abundance_long, aes(x = log10(length_cm), y = log10(meanN), group = species_name)) +
  geom_line() +
  facet_wrap(~species_name) +
  labs(
    x = "log10(Length cm)",
    y = "log10(Mean Abundance)") +
  theme_classic(base_size = 11) +
  theme(
    strip.text   = element_text(size = 7, face = "bold"),
    axis.title   = element_text(face = "bold"),
    axis.text    = element_text(color = "black", size = 7),
    panel.border = element_rect(colour = "grey70", fill = NA, linewidth = 0.5))
p



selected_cols <- c(
  1,   # YEAR
  9,   # COD(ATLANTIC)
  14,  # HADDOCK
  32,  # REDFISH UNSEPARATED
  3,   # AMERICAN PLAICE
  35,  # SILVER HAKE
  25,  # MONKFISH,GOOSEFISH,ANGLER
  43,  # WHITE HAKE
  41,  # THORNY SKATE
  45,  # WINTER SKATE
  31,  # POLLOCK
  48,  # YELLOWTAIL FLOUNDER
  #20,  # LONGHORN SCULPIN
  165,  # SCULPIN (NS)
  39,  # SQUIRREL OR RED HAKE
  16,  # HERRING(ATLANTIC)
  27,  # NORTHERN SAND LANCE
  58,  # SPINY DOGFISH
  15   # HALIBUT(ATLANTIC)
)

biomass_subset <- biomass_wide[, selected_cols]

# Convert to long format
biomass_long <- biomass_subset %>%
  pivot_longer(
    cols = -YEAR,
    names_to = "Species",
    values_to = "Biomass" )

# Facet plot
ggplot(biomass_long, aes(x = YEAR, y = Biomass)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ Species, scales = "free_y") +
  theme_bw() +
  labs(
    x = "Year",
    y = "Biomass",
    title = "Biomass Trends by Species"
  ) +
  theme(
    strip.text = element_text(size = 6),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
