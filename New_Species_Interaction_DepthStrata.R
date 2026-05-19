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
  filter(suppressWarnings(as.numeric(STRAT)) >= 444, # exclude 4vN
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
