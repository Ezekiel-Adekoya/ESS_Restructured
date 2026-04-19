list.files("Ess_data")
filename <- "Ess_data/Alida_Qadj_Biomass.csv"

###################################################################################
# Data Handling
###################################################################################

pl.data <- read.table(filename, sep=",", header=TRUE, row.names=1)

###################################################################################
# DO PCA calculations - matrix needs to be complete (no blank cells)
###################################################################################

dimC <- dim(pl.data)

# Impute NAs with column means
pl.data.clean <- as.data.frame(lapply(pl.data, function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}))

# Restore row names lost by lapply
rownames(pl.data.clean) <- rownames(pl.data)

# Run PCA
pl.pca <- prcomp(pl.data.clean, scale. = TRUE)

# Extract scores once here and reuse everywhere
scores <- pl.pca$x

# Print out PCA loadings
pl.pca$rotation

# Print out eigenvalues
pl.pca$sdev^2

# Print PCA summary
summary(pl.pca)

# Scree plot
plot(pl.pca, type="lines")

# Plot year scores and label points
plot(scores[,1], scores[,2], xlab="PC1", ylab="PC2", main="PCA Scores", type="n")
plot(pl.pca, type="lines")

# Plot points
text(scores[,1], scores[,2], labels=rownames(pl.data), cex=0.7)

###################################################################################
# Colourful Biplot
###################################################################################

# Colours for year points - gradient from blue (early) to red (late)
n_years   <- nrow(scores)
n_species <- nrow(pl.pca$rotation)

year_cols    <- colorRampPalette(c("steelblue", "gold", "firebrick"))(n_years)
species_cols <- colorRampPalette(c("darkgreen", "purple"))(n_species)

# Loading coordinates (standard biplot scaling)
loadings <- pl.pca$rotation[, 1:2]
scale_factor <- max(abs(scores[, 1:2])) / max(abs(loadings)) * 0.7

# Set up blank plot
plot(scores[,1], scores[,2],
     type="n",
     xlab=paste0("PC1 (", round(summary(pl.pca)$importance[2,1]*100, 1), "%)"),
     ylab=paste0("PC2 (", round(summary(pl.pca)$importance[2,2]*100, 1), "%)"))
abline(h=0, v=0, col="grey80", lty=2)

# Draw species arrows
for (i in seq_len(n_species)) {
  arrows(0, 0,
         loadings[i,1] * scale_factor,
         loadings[i,2] * scale_factor,
         length=0.1, col=species_cols[i], lwd=1.5)
}

# Label species
text(loadings[,1] * scale_factor * 1.1,
     loadings[,2] * scale_factor * 1.1,
     labels=rownames(loadings),
     col=species_cols, cex=0.65, font=2)

# Draw year points
points(scores[,1], scores[,2],
       pch=19, col=year_cols, cex=1.4)

# Label years
text(scores[,1], scores[,2],
     labels=rownames(scores),
     col=year_cols, cex=0.65, pos=3)

############################################ Alternative PCA approaches #########################################

library(factoextra)
library(plot3D)
library(plotly)

PoV <- pl.pca$sdev^2 / sum(pl.pca$sdev^2)
fviz_eig(pl.pca)

pcx <- scores[, 1]
pcy <- scores[, 2]
pcz <- scores[, 3]

pcxlab <- paste("PC1 (", round(PoV[1] * 100, 2), "%)")
pcylab <- paste("PC2 (", round(PoV[2] * 100, 2), "%)")
pczlab <- paste("PC3 (", round(PoV[3] * 100, 2), "%)")

# 3D as points
scatter3D(pcx, pcy, pcz, bty = "g", pch = 20, cex = 2,
          col = gg.col(100), theta = 150, phi = 0, main = "PCA Scores",
          xlab = pcxlab, ylab = pcylab, zlab = pczlab)
text3D(pcx, pcy, pcz, labels = rownames(scores), add = TRUE, colkey = FALSE, cex = 0.7)

# 3D as connected line
scatter3D(pcx, pcy, pcz, bty = "g", type = "b", pch = 20, cex = 2,
          col = gg.col(100), theta = 150, phi = 0, lwd = 4, main = "PCA Scores",
          xlab = pcxlab, ylab = pcylab, zlab = pczlab)
text3D(pcx, pcy, pcz, labels = rownames(scores), add = TRUE, colkey = FALSE, cex = 0.7)

# Plot 3D with plotly
df3D <- data.frame(comp1 = scores[, 1],
                   comp2 = scores[, 2],
                   comp3 = scores[, 3],
                   year  = rownames(scores))   # store row names as column for text labels

fig <- plot_ly(df3D, x = ~comp1, y = ~comp2, z = ~comp3, color = ~comp3,
               mode = 'lines+markers', text = ~year)
fig <- fig %>% add_markers()
fig <- fig %>% add_text(textposition = "top right")
fig <- fig %>% layout(scene = list(xaxis = list(title = pcxlab),
                                   yaxis = list(title = pcylab),
                                   zaxis = list(title = pczlab)),
                      annotations = list(x = 1.13, y = 1.05, text = 'PC3 Score',
                                         showarrow = FALSE))
fig


# =============================================================================
# Principal Component Analysis (PCA) via Singular Value Decomposition
# =============================================================================

library(factoextra)
library(FactoMineR)
library(plot3D)
library(plotly)
library(ggplot2)

# -----------------------------------------------------------------------------
# 1. DATA PREPROCESSING
# -----------------------------------------------------------------------------

#' Remove zero-variance and all-NA columns, then impute remaining NAs with
#' column means to ensure prcomp() receives a complete numeric matrix.

clean_for_pca <- function(df) {
  # Drop columns with no variance (including all-NA columns)
  col_vars <- apply(df, 2, var, na.rm = TRUE)
  df <- df[, !is.na(col_vars) & col_vars > 0]
  # Impute remaining NAs with column mean
  as.data.frame(lapply(df, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
}

pl.data.clean <- clean_for_pca(pl.data)

# -----------------------------------------------------------------------------
# 2. RUN PCA
# -----------------------------------------------------------------------------

res.pca <- prcomp(pl.data.clean, scale. = TRUE)   # scale. = TRUE: unit variance
scores  <- res.pca$x                               # Individual scores (n × p)
PoV     <- res.pca$sdev^2 / sum(res.pca$sdev^2)   # Proportion of variance

# Axis labels with explained variance
pc_label <- function(i, pov) {
  sprintf("PC%d (%.2f%%)", i, pov[i] * 100)
}

pcxlab <- pc_label(1, PoV)
pcylab <- pc_label(2, PoV)
pczlab <- pc_label(3, PoV)

# -----------------------------------------------------------------------------
# 3. SCREE PLOT  –  Explained variance per component
# -----------------------------------------------------------------------------

fviz_eig(res.pca,addlabels = TRUE,ylim = c(0, 100), main = "Scree Plot – Proportion of Variance Explained")

# -----------------------------------------------------------------------------
# 4. INDIVIDUALS PLOT  –  Colour by contribution
# -----------------------------------------------------------------------------

fviz_pca_ind(res.pca,
             col.ind      = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel        = TRUE,
             title        = "PCA – Individuals",
             xlab         = pcxlab,
             ylab         = pcylab)

# -----------------------------------------------------------------------------
# 5. VARIABLES PLOT  –  Colour by contribution
# -----------------------------------------------------------------------------

fviz_pca_var(res.pca, 
             col.var = "contrib",
             gradient.cols = c("#36648B", "#FFA500", "#8B2500"),
             repel = TRUE) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        plot.title = element_blank(),
        legend.title = element_blank())

# -----------------------------------------------------------------------------
# 6. HIERARCHICAL CLUSTERING ON PRINCIPAL COMPONENTS (HCPC)
# -----------------------------------------------------------------------------

res.pca4 <- PCA(pl.data.clean, ncp = 3, graph = FALSE)
res.hcpc  <- HCPC(res.pca4,   graph = FALSE)

# -- Dendrogram ---------------------------------------------------------------
fviz_dend(res.hcpc, cex = 0.7,palette = "jco",rect = TRUE,rect_fill = TRUE,rect_border = "jco",
          labels_track_height = 0.8, main = "Hierarchical Clustering Dendrogram")

# -- Cluster map on factor plane ----------------------------------------------
fviz_cluster(res.hcpc,
             repel           = TRUE,
             show.clust.cent = TRUE,
             palette         = "jco",
             ggtheme         = theme_bw(),
             main            = "HCPC – Factor Map")

# -----------------------------------------------------------------------------
# 7. 3D SCATTER PLOT  –  First three principal components
# -----------------------------------------------------------------------------

pcx <- scores[, 1]
pcy <- scores[, 2]
pcz <- scores[, 3]

scatter3D(
  x     = pcx, y = pcy, z = pcz, bty  = "g",pch   = 20,
  cex   = 2, col   = gg.col(100),theta = 150,phi   = 0,
  main  = "PCA Scores – 3D", xlab  = pcxlab, ylab  = pcylab, zlab  = pczlab)

text3D(x = pcx,y= pcy,z = pcz, labels = rownames(scores),add = TRUE,colkey = FALSE,cex = 0.7)


########################### Use prcomp() instead - this uses singular value decomposition ###########################

library(factoextra)
library(FactoMineR)
library(plot3D)
library(plotly)
library(ggplot2)

# ---------- Data Cleaning: remove zero-variance/all-NA columns, impute NAs ----------
col_vars      <- apply(pl.data, 2, var, na.rm = TRUE)
pl.data.clean <- pl.data[, which(!is.na(col_vars) & col_vars > 0)]
pl.data.clean <- as.data.frame(lapply(pl.data.clean, function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}))

# Assign row names if missing (required for text labels in plots)
if (is.null(rownames(pl.data.clean))) {
  rownames(pl.data.clean) <- paste0("Obs", seq_len(nrow(pl.data.clean)))
}

# ---------- Run PCA ----------
res.pca <- prcomp(pl.data.clean, scale. = TRUE)

# Build single source-of-truth score data frame
score_df       <- as.data.frame(res.pca$x)
score_df$label <- rownames(score_df)

# Pull vectors from score_df - lengths guaranteed identical
pcx       <- score_df[, "PC1"]
pcy       <- score_df[, "PC2"]
pcz       <- score_df[, "PC3"]
pc_labels <- score_df[, "label"]

# Safety check
stopifnot(length(pcx) == length(pc_labels))

# Proportion of variance explained by each PC
PoV <- res.pca$sdev^2 / sum(res.pca$sdev^2)

# Axis labels
pcxlab <- paste("PC1 (", round(PoV[1] * 100, 2), "%)")
pcylab <- paste("PC2 (", round(PoV[2] * 100, 2), "%)")
pczlab <- paste("PC3 (", round(PoV[3] * 100, 2), "%)")

# ---------- Scree Plot ----------
# Visualize eigenvalues - percentage of variance explained by each PC
fviz_eig(res.pca)

# ---------- Graph of Individuals ----------
# Individuals with a similar profile are grouped together
fviz_pca_ind(res.pca,col.ind       = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel= TRUE ) + labs(title = "PCA", x = "PC1", y = "PC2")

# ---------- Graph of Variables ----------
fviz_pca_var(res.pca,col.var  = "contrib", gradient.cols = c("#36648B", "#FFA500", "#8B2500"), repel = TRUE)

# ---------- Hierarchical Clustering on Principal Components (HCPC) ----------
res.pca4 <- PCA(pl.data.clean, ncp = 3, graph = FALSE)
res.hcpc  <- HCPC(res.pca4,   graph = FALSE)

# Dendrogram
fviz_dend(res.hcpc,cex = 0.7,palette = "jco",rect = TRUE, rect_fill = TRUE, rect_border = "jco", 
          labels_track_height = 0.8)

# Cluster factor map
fviz_cluster(res.hcpc,repel = TRUE, show.clust.cent = TRUE,palette = "jco",
             ggtheme = theme_bw(),main = "Factor map")

# ---------- 3D Scatter – Points ----------
scatter3D(pcx, pcy, pcz,
          bty  = "g", pch = 20, cex = 2,col  = gg.col(100), theta = 150, phi = 0,
          main = "PCA Scores", xlab = pcxlab, ylab = pcylab, zlab = pczlab)
text3D(pcx, pcy, pcz,labels = pc_labels,add = TRUE, colkey = FALSE, cex = 0.7)

# ---------- 3D Scatter – Connected Line ----------
scatter3D(pcx, pcy, pcz, bty  = "g", type = "b", pch = 20, cex = 2, lwd = 2,
          col  = gg.col(100), theta = 120, phi = 0,
          main = "PCA Scores", xlab = pcxlab, ylab = pcylab, zlab = pczlab)
text3D(pcx, pcy, pcz, labels = pc_labels,add = TRUE, colkey = FALSE, cex = 0.7)

# ---------- 2D ggplot – Rainbow (with path) ----------
ggplot(data = score_df, aes(x = PC1, y = PC2, group = 1)) +
  geom_point(size = 5, aes(colour = label)) +
  geom_path(linewidth = 0.2) +
  geom_text(aes(label = label)) +
  theme(legend.position = "none")

# ---------- 2D ggplot – Rainbow (points only) ----------
ggplot(data = score_df, aes(x = PC1, y = PC2, group = 1)) +
  geom_point(size = 5, aes(colour = label)) +
  geom_text(aes(label = label)) +
  theme(legend.position = "none")

# ---------- Interactive 3D – Plotly ----------
fig <- plot_ly(score_df, x     = ~PC1, y = ~PC2, z = ~PC3,
       color = ~PC3, mode  = "lines+markers",text  = ~label)

fig <- fig %>%
  add_markers() %>% add_text(textposition = "top right") %>%
  layout(scene = list(xaxis = list(title = pcxlab),
      yaxis = list(title = pcylab), zaxis = list(title = pczlab) ),
    annotations = list( x= 1.13, y = 1.05, text = "PC3 Score", showarrow = FALSE))

fig

################################# DENDROGRAMS ###############################
# Dendrogram flowing down the page
library(ape)
dd <- dist(scale(pl.data), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hc, hang = -1, cex = 0.3)   # Put the labels at the same height: hang = -1
plot(hc, hang = -1, cex = 1.5)   # Put the labels at the same height: hang = -1

# Dendrogram flowing across the page
# Define nodePar
hcd <- as.dendrogram(hc)
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19),  cex = 0.7, col = "blue")
par(cex=0.5)   # Customized plot; remove labels
par(cex=1.0)   # Customized plot; remove labels
plot(hcd,  xlab = "Height", nodePar = nodePar, horiz = TRUE)

# Unrooted - dendrogram on an arc
plot(as.phylo(hc), type = "unrooted", cex = 0.6, no.margin = TRUE)

# Fan
par(cex=0.3)
par(cex=0.5)
plot(as.phylo(hc), type = "fan")
colors = c("red", "blue", "green", "black", "deeppink1", "darkviolet", "grey", "brown", "magenta", "orange", "violetred", "tomato")
clus = cutree(hc, 12)
plot(as.phylo(hc), type = "fan", tip.color = colors[clus],label.offset = 1, cex = 2, font = 11, lwd = 12, edge.width = 5)
plot(as.phylo(hc), type = "fan", tip.color = colors[clus],label.offset = 1, cex =2, font = 11, lwd = 12, edge.width = 5)


########################### CORRELATION RESULTS ###########################  
# Perform correlation on indicators
library(corrplot)
library(RColorBrewer)
library(corrplot)
library(RColorBrewer)
library(reshape2)
library(ggplot2)

# ── 0. Load data ──────────────────────────────────────────────────────────────
pl.data <- read.table(filename, sep = ",", header = TRUE)

# ── 1. Correlation plot ───────────────────────────────────────────────────────
# Keep only numeric columns
pl.data.numeric <- pl.data[, sapply(pl.data, is.numeric), drop = FALSE]

# Remove zero-variance AND NA-variance columns
variances <- apply(pl.data.numeric, 2, var, na.rm = TRUE)
pl.data.clean <- pl.data.numeric[, which(variances > 0), drop = FALSE]

# Compute Spearman correlation
M <- cor(pl.data.clean, method = "spearman", use = "pairwise.complete.obs")

# Remove any remaining NA rows/cols
keep <- rowSums(is.na(M)) == 0
M <- M[keep, keep]

# Plot correlation matrix
corrplot(M, type = "upper", order = "hclust",
         col = brewer.pal(n = 8, name = "RdYlBu"),
         tl.col = "black", tl.cex = 0.5)

# ── 2. Colour palette ─────────────────────────────────────────────────────────
col3Palette <- c(
  brewer.pal(9, "Oranges")[c(8, 7, 6, 5, 2)],
  brewer.pal(11, "PiYG")[c(1, 2, 3, 4, 5)],
  brewer.pal(9, "Purples")[c(2, 4, 3, 6, 7, 8, 9)],
  'mediumorchid4', 'mediumorchid3', 'mediumorchid2', 'mediumorchid1',
  'orchid4', 'orchid3', 'orchid2', 'orchid1',
  'hotpink4', 'hotpink2', 'hotpink1', 'violetred2', 'violetred3', 'violetred4',
  brewer.pal(9, "Greens")[c(2, 6, 7, 8, 9)],
  'turquoise4', 'turquoise3', 'turquoise2', 'turquoise1', 'paleturquoise1',
  brewer.pal(9, "Blues")[c(1, 2, 3, 4, 5, 6, 7, 8, 9)],
  'royalblue3', 'darkslateblue', 'midnightblue',
  brewer.pal(9, "BrBG")[c(1, 2, 3, 4, 5)],
  'chocolate4', 'chocolate3', 'chocolate2', 'chocolate1',
  'goldenrod2', 'goldenrod1', 'lightgoldenrod1', 'khaki4',
  'black', brewer.pal(9, "Greys")[c(5, 4, 3, 2)],
  'aliceblue', 'darkred', 'red3', 'red1', 'firebrick4', 'firebrick3', 'firebrick1', 'rosybrown1'
)

# # ── 3. Reshape data for ggplot ────────────────────────────────────────────────

final_catch <- reshape2::melt(pl.data, id = c("Year"))
colnames(final_catch)[2:3] <- c("Species", "Catch")

# ── 4. Stacked bar chart — no legend ─────────────────────────────────────────
ggplot(data = final_catch, aes(x = Year, y = Catch, fill = Species)) +
  geom_bar(colour = "black", stat = "identity", linewidth = 0.1) +
  scale_fill_manual(values = col3Palette) +
  theme_bw() + labs(x = "Years", y = "Tonnes") +
  theme(legend.position = "none",
  axis.text = element_text(size = 16, face = "bold"),
  axis.title = element_text(size = 20, face = "bold"))

# ── 5. Stacked bar chart — with legend ───────────────────────────────────────
ggplot(data = final_catch, aes(x = Year, y = Catch, fill = Species)) +
  geom_bar(colour = "black", stat = "identity", linewidth = 0.1) +
  scale_fill_manual(name = "Species", values = col3Palette) +
  theme_bw() +
  labs(x = "Years", y = "Tonnes") +
  theme(legend.text = element_text(size = 10),
        axis.text = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"))

# ── 6. Proportional stacked bar chart ────────────────────────────────────────
ggplot(data = final_catch, aes(x = Year, y = Catch, fill = Species)) +
  geom_bar(colour = "black", position = "fill", stat = "identity", linewidth = 0.1) +
  scale_fill_manual(name = "Species", values = col3Palette) +
  theme_bw() +
  labs(x = "Years", y = "Proportion of catch") +
  theme(legend.text = element_text(size = 10),
        axis.text = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"))
        
  
  
  
################################# Heatmap ####################################################
# Create row names and trim off first column
library(heatmaply)
pl.data <- read.table(filename,sep=",",header=T)

SP_as_col <- pl.data
rownames(SP_as_col) <- SP_as_col[,1]
SP_as_colA <- SP_as_col %>% dplyr::select(-Year)

# Transpose SP_as_col so years are column headers
# Here the va;ues are proportion of the catch but could be absolute catch
Yr_as_col <- t(SP_as_colA)

#Replace NA with 0
Yr_as_col[is.na(Yr_as_col)] <- 0
Prop_Catch <- apply(Yr_as_col, 2, function(i) i/sum(i))

nSP <- length(Yr_as_col[,1])
nYr <- length(Yr_as_col[1,])

# Create new df to populate
drRanked <- data.frame(matrix(ncol = nYr, nrow = nSP))
colnames(drRanked)[1:nYr] = as.character(colnames(Yr_as_col)[1:nYr])
rownames(drRanked)[1:nSP] = as.character(rownames(Yr_as_col)[1:nSP])

# Do ranking (using order() routine as want largest value to be rated 1)
for (i in 1:nSP) {
  drRanked[i,] <- dplyr::dense_rank(-Yr_as_col[i,])
}
# Create the heatmap - switch our row for both to see if years cluster up
HeatmapTitle <- paste ("Heatmap of contribution to biomass through time, yellow represents a larger contribution",sep="") 
HeatmapFile <- "HeatmapFilename.pdf"

heatmaply(drRanked,
          xlab = "Year",
          ylab = "Species",
          main = HeatmapTitle,
          dendrogram = "row",
          #dendrogram = "both",
          fontsize_row = 10,
          file = HeatmapFile)
      
