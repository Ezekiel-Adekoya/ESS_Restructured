library(reshape2)
library(vegan) # For distance/overlap metrics

# 1. Pivot your survey data: Strata as rows, Species as columns


library(tidyverse)
library(vegan) # For Schoener's D logic

stat <- read.csv("/Users/juliab6/UTAS Research Dropbox/Julia Blanchard/NEW DATA ESS/Untitled/Ess_data/LF_Biomass_Abund.csv")
setwd("/Users/juliab6/UTAS Research Dropbox/Julia Blanchard/NEW DATA ESS/Untitled")
# need to exclude deep water shelf edge strata, I think the ones to include are 440–466 
# 
# 
stat <- read.csv("Ess_data/LF_Biomass_Abund.csv")
# #1. Define your 18 species names (must match the names in your survey data exactly)
selected_spp_names <- c("COD(ATLANTIC)", "HADDOCK", "REDFISH UNSEPARATED",
                        "AMERICAN PLAICE", "SILVER HAKE", "MONKFISH,GOOSEFISH,ANGLER",
                        "WHITE HAKE", "THORNY SKATE", "WINTER SKATE", "POLLOCK",
                        "YELLOWTAIL FLOUNDER", "LONGHORN SCULPIN", "SQUIRREL OR RED HAKE",
                        "HERRING(ATLANTIC)", "NORTHERN SAND LANCE", "SPINY DOGFISH",
                        "HALIBUT(ATLANTIC)", "CAPELIN")

# 2. Filter and Pivot
stratum_matrix <- stat %>%
  # Filter only for your modeling species
  filter(COMMON_NAME %in% selected_spp_names) %>%
  group_by(STRAT, COMMON_NAME) %>%
  summarise(biomass = sum(Biomass_Strata_KG, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = COMMON_NAME, values_from = biomass, values_fill = 0) %>%
  # MOVE Stratum out of the data columns and into the row labels
  column_to_rownames("STRAT")

# 3. Calculate Overlap (Schoener's D)
# Normalize each species' total biomass across the strata/year snapshots to 1
dist_norm <- sweep(stratum_matrix, 2, colSums(stratum_matrix), "/")

# Calculate interaction matrix
inter_matrix <- 1 - (as.matrix(dist(t(dist_norm), method = "manhattan")) / 2)
diag(inter_matrix) <- 1

# 4. Generate Dendrogram
d <- as.dist(1 - inter_matrix)
fit <- hclust(d, method = "ward.D2")
plot(fit, main = "4VsW Spatial Associations: Selected 18 Species", 
     xlab = "", sub = "Clustered by Depth Stratum Overlap", hang = -1)



biomass_overlap <- stat %>%
  filter(!is.na(STRAT), STRAT != "") %>%
  filter(suppressWarnings(as.numeric(STRAT)) >= 443, # 443 = 4Vs (confirmed); 440-442 = 4Vn excluded
         suppressWarnings(as.numeric(STRAT)) <= 466) %>%
  filter(SPEC < 1000) %>%
  group_by(STRAT, COMMON_NAME) %>%
  summarise(Biomass = sum(Biomass_Strata_KG, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = COMMON_NAME, values_from = Biomass, values_fill = 0) 
  # Remove Year/Stratum columns for the math
  #select(-YEAR, -STRAT)

# 2. Normalize: Species distributions across all observations (sum to 1)
# This ensures a "dominant" species doesn't skew the overlap math
dist_norm <- sweep(stratum_matrix, 2, colSums(stratum_matrix), "/")

# 3. Calculate Schoener's D (Percentage Overlap)
# Manhattan distance / 2 gives the difference; 1 minus that gives the overlap
inter_matrix <- 1 - (as.matrix(dist(t(dist_norm), method = "manhattan")) / 2)

# 4. Cleanup: Ensure the diagonal is exactly 1
diag(inter_matrix) <- 1


# Convert to a distance object (1 - overlap)
# High overlap = Low distance
d <- as.dist(1 - inter_matrix)

# Hierarchical Clustering using Ward's method (minimizes variance within clusters)
fit <- hclust(d, method = "ward.D2")

# Plot with a horizontal line to suggest 'guilds'
plot(fit, main = "4VsW Species Associations by Depth Stratum", 
     xlab = "", sub = "Based on Schoener's D Overlap", ylab = "Ecological Distance")
abline(h = 0.5, col = "red", lty = 2) # Threshold for distinct guilds

# 1. Add rectangles to the dendrogram to highlight the clusters
# 'h = 0.5' matches your red line; alternatively use 'k = 4' for four main guilds
rect.hclust(fit, h = 0.5, border = "blue") 

# 2. Extract which species belong to which cluster (useful for your interaction matrix)
clusters <- cutree(fit, h = 0.5)
print(clusters) # Shows the guild assignment for each of your 18 species

# 3. (Optional) Create a more modern Cluster Plot using ggplot2
library(factoextra)
fviz_dend(fit, h = 0.5,                 # Set threshold
          type = "rectangle",           # Or use "phylogenic" for a circular plot
          rect = TRUE,                  # Add rectangles
          rect_fill = TRUE,             # Fill rectangles with colour
          k_colors = "jco",             # Colour palette
          cex = 0.6,                    # Label size
          main = "4VsW Ecological Guilds (Depth Stratum Overlap)")




# ── INTERACTION MATRIX ────────────────────────────────────────────────────────
# Guild structure from Schoener's D depth-stratum dendrogram (h = 0.5 cut)
# Ecological rules from Cook & Bundy 2010 (Table 4.1.1)
# ─────────────────────────────────────────────────────────────────────────────

params_spp_interactions <- params
inter <- interaction_matrix(params_spp_interactions)

# ── BASELINE ──────────────────────────────────────────────────────────────────
inter[] <- 0.5
diag(inter) <- 0.2

# ── GUILDS (from dendrogram) ──────────────────────────────────────────────────
# Guild 1: pelagic/slope — high overlap, compete for same prey
pelagics    <- c("Atlantic Herring", "Capelin", "Silver Hake", "Red Hake",
                 "Monkfish", "Sand Lance")

# Guild 2: mid-water/mixed demersal
mid_water   <- c("White Hake", "Pollock", "Redfish Unseparated")

# Guild 3: shelf demersal — high benthic overlap
benthivores <- c("Haddock", "American Plaice", "Yellowtail Flounder",
                 "Witch Flounder", "Longhorn Sculpin")

# Guild 4: large demersal predators
piscivores  <- c("Atlantic Cod", "Spiny Dogfish", "White Hake")

# Guild 5: large skates/halibut — limited predators due to size/handling
large_elasmo <- c("Thorny Skate", "Winter Skate", "Atlantic Halibut")

# ── GUILD-LEVEL RULES (Cook & Bundy 2010) ────────────────────────────────────
# Piscivores: strong preference for pelagics, moderate on benthi/piscivores
inter[piscivores, pelagics]     <- 0.8
inter[piscivores, benthivores]  <- 0.6
inter[piscivores, piscivores]   <- 0.6
inter[piscivores, mid_water]    <- 0.5

# Benthivores: strong within-guild, weak on pelagics/piscivores
inter[benthivores, benthivores] <- 0.8
inter[benthivores, pelagics]    <- 0.1
inter[benthivores, piscivores]  <- 0.1
inter[benthivores, mid_water]   <- 0.4

# Mid-water: moderate on pelagics, low on strict benthos
inter[mid_water, pelagics]      <- 0.6
inter[mid_water, benthivores]   <- 0.3
inter[mid_water, piscivores]    <- 0.4

# ── SPECIES-SPECIFIC OVERRIDES ────────────────────────────────────────────────
# White Hake: less benthic than other piscivores (Cook & Bundy 2010)
inter["White Hake", "Sand Lance"]        <- 0.2

# Halibut: less pelagic than other large predators
inter["Atlantic Halibut", "Atlantic Herring"] <- 0.2

# Haddock: more gadoid predation than other benthivores
inter["Haddock", c(piscivores, benthivores)] <- 0.5

# Large skates/halibut: few predators due to size and handling difficulty
inter[, large_elasmo]                        <- 0.0
inter[piscivores,    large_elasmo]           <- 0.5
inter["Spiny Dogfish", large_elasmo]         <- 0.3  # dogfish less likely on large flatfish

# Thorny/Winter Skate: different depth strata (from dendrogram — Thorny deeper)
# Cross-skate predation low; within-guild overlap retained from dendrogram
inter["Thorny Skate", "Winter Skate"]    <- 0.2
inter["Winter Skate",  "Thorny Skate"]   <- 0.2

# Diagonal: self-interaction (resource competition within species)
diag(inter) <- 0.2

# ── APPLY TO PARAMS ───────────────────────────────────────────────────────────
params <- setInteraction(params, inter)

# ── HEATMAP ───────────────────────────────────────────────────────────────────
# Order species by guild for visual clarity
guild_order <- c(
  # Piscivores
  "Atlantic Cod", "Spiny Dogfish",
  # Mid-water
  "White Hake", "Pollock", "Redfish Unseparated",
  # Benthivores
  "Haddock", "American Plaice", "Yellowtail Flounder", "Witch Flounder", "Longhorn Sculpin",
  # Large elasmo/halibut
  "Atlantic Halibut", "Thorny Skate", "Winter Skate",
  # Pelagics
  "Atlantic Herring", "Capelin", "Silver Hake", "Red Hake", "Monkfish", "Sand Lance"
)

# Reorder matrix and melt
inter_ordered <- inter[guild_order, guild_order]
inter_long <- reshape2::melt(inter_ordered,
                             varnames = c("Predator", "Prey"),
                             value.name = "InteractionStrength")
inter_long$Predator <- factor(inter_long$Predator, levels = rev(guild_order))
inter_long$Prey     <- factor(inter_long$Prey,     levels = guild_order)

# Guild separator positions (after each guild block)
vlines <- c(2.5, 5.5, 10.5, 13.5)  # vertical   (Prey axis)
hlines <- c(2.5, 5.5, 10.5, 13.5)  # horizontal (Predator axis, reversed)

heatmap_plot <- ggplot(inter_long, aes(x = Prey, y = Predator, fill = InteractionStrength)) +
  geom_tile(color = "white", linewidth = 0.15) +
  geom_vline(xintercept = vlines, color = "grey30", linewidth = 0.4, linetype = "dashed") +
  geom_hline(yintercept = hlines, color = "grey30", linewidth = 0.4, linetype = "dashed") +
  scale_fill_gradientn(
    colors = RColorBrewer::brewer.pal(9, "Blues"),
    limits = c(0, 1),
    name   = "Interaction\nStrength"
  ) +
  labs(x = "Prey Species", y = "Predator Species") +
  theme_minimal(base_family = "Arial") +
  theme(
    axis.text.x    = element_text(angle = 90, hjust = 1, size = 9,  face = "bold"),
    axis.text.y    = element_text(size = 9,  face = "bold"),
    axis.title     = element_text(size = 11, face = "bold"),
    legend.title   = element_text(size = 9,  face = "bold"),
    legend.text    = element_text(size = 8),
    panel.grid     = element_blank(),
    panel.border   = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.background = element_blank()
  )

print(heatmap_plot)
