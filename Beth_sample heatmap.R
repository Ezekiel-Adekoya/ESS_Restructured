## Clear the space
rm(list = ls()) # clear memory

# Data handling and table reshaping
library(tidyverse)
library(reshape2)
library(devtools)
# Plotting
library(ggplot2)
library(RColorBrewer)
library(ggbiplot)
library("plot3D")
library(plotly)
library(heatmaply)
library (data.table)
# PCA and Clustering
library(factoextra)
library (FactoMineR)
library(corrplot)
library(ape)

# Set the working directory
setwd("//Users/ful083/Work/Lenfest_EBFM/case_studies/Thailand/EwE_info_&_Scenarios_Definitions_&_Workup/ScenarioResults/exploration_&_indicator_calc/")

filename <- 'ThaiDB_RV_CPUE_spcol_renamed.csv'
filename <-'Thai_SP_as_col.csv'

###################################################################################
# Data Handling
###################################################################################

# Load data - years as rows, species as columns
pl.data <- read.table(filename ,sep=",",header=T,row.names=1)

# Can also consider inverse data - species as rows and years as columns
#pl.data <- read.table('Year_as_column_data.csv',sep=",",header=T,row.names=1)

# print out data as a check
View(pl.data)
# prints out a copy of the data table

# DO PCA calculations
# DO PCA calculations - matrix needs to be complete (no blank cells) 

# So create variable names to follow
dimC <- dim(pl.data)
pc.f <- formula(paste("~", paste(names(pl.data)[1:dimC[2]], collapse = "+")))
pl.pca <- princomp(pc.f, cor=TRUE, data=pl.data)

# Print out PCA loadings
pl.pca$loadings

# Print out eigenvalues
pl.pca$sd^2

# Print PCA summary
summary(pl.pca)

# Plot results - look to see number of PCA axes to retain
plot(pl.pca, type="lines")

# Plot points
text(pl.pca$scores, labels=as.character(row.names(pl.data)), pos=1, cex=0.7)

# Biplot of PCA
biplot(pl.pca, cex=0.8, col=c(1,8))

# Linked biplot
View(pl.pca$scores)
library(ggplot2)
df <- data.frame(comp1=pl.pca$scores[,1],
                 comp2=pl.pca$scores[,2])
ggplot(data = df, aes(x=comp1, y=comp2, group=1)) +
  geom_point(size=5, aes(colour=rownames(pl.pca$scores))) +
  geom_path(size = 0.2) +
  geom_text(label=rownames(pl.pca$scores)) + 
  theme(legend.position="none")

############################################ Alternative PCA approaches #########################################

PoV <- pl.pca$sdev^2/sum(pl.pca$sdev^2)
fviz_eig(pl.pca)

# Put on row named
#Rename Col 1
View(pl.data)

pcx <- pl.pca$scores[,1]
pcy <- pl.pca$scores[,2]
pcz <- pl.pca$scores[,3]

pcxlab <- paste("PC1 (", round(PoV[1] * 100, 2), "%)")
pcylab <- paste("PC2 (", round(PoV[2] * 100, 2), "%)")
pczlab <- paste("PC3 (", round(PoV[3] * 100, 2), "%)")

View(pl.pca$scores)

# 3D as points
scatter3D(pcx, pcy, pcz, bty = "g", pch = 20, cex = 2, 
          col = gg.col(100), theta = 150, phi = 0, main = "PCA Scores", xlab = pcxlab,
          ylab =pcylab, zlab = pczlab)
text3D(pcx, pcy, pcz,  labels = rownames(pl.pca$scores), add = TRUE, colkey = FALSE, cex = 0.7)

# 3D as connected line
scatter3D(pcx, pcy, pcz, bty = "g", type = "b", pch = 20, cex = 2, 
          col = gg.col(100), theta = 150, phi = 0, lwd = 4, main = "PCA Scores", xlab = pcxlab,
          ylab =pcylab, zlab = pczlab)
text3D(pcx, pcy, pcz,  labels = rownames(pl.pca$scores), add = TRUE, colkey = FALSE, cex = 0.7)

# Plot3D with plotly
df3D <- data.frame(comp1=pl.pca$scores[,1],
                   comp2=pl.pca$scores[,2],
                   comp3=pl.pca$scores[,3])
fig <- plot_ly(df3D, x = ~comp1, y = ~comp2, z = ~comp3, color = ~comp3,  mode = 'lines+markers',
               # Hover text:
               text = ~rownames(pl.pca$scores))
fig <- fig %>% add_markers()
fig <- fig %>% add_text(textposition = "top right")
fig <- fig %>% layout(scene = list(xaxis = list(title = pcxlab),
                                   yaxis = list(title = pcylab),
                                   zaxis = list(title = pczlab)),
                      annotations = list(
                        x = 1.13,
                        y = 1.05,
                        text = 'PC3 Score',
                        showarrow = FALSE
                      ))
fig

########################### Use prcomp() instead - this uses singular value decomposition  ###########################
res.pca <- prcomp(pl.data, scale = TRUE)
res.pca$x

# Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
fviz_eig(res.pca)

#Graph of individuals. Individuals with a similar profile are grouped together.
fviz_pca_ind(res.pca,
             col.ind = "contrib", # Color by congtribution
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             #label=SP_as_col$Year
) +
  labs(title ="PCA", x = "PC1", y = "PC2")

# Graph of variables
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#36648B", "#FFA500", "#8B2500"),
             repel = TRUE     # Avoid text overlapping
)

# Compute hierarchical clustering on principal components
res.pca4 <- PCA(pl.data, ncp = 3, graph = FALSE)
res.hcpc <- HCPC(res.pca4, graph = FALSE)

fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)

fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_bw(),
             main = "Factor map"
)


PoV <- res.pca$sdev^2/sum(res.pca$sdev^2)
fviz_eig(res.pca)


pcx <- res.pca$x[,1]
pcy <- res.pca$x[,2]
pcz <- res.pca$x[,3]

pcxlab <- paste("PC1 (", round(PoV[1] * 100, 2), "%)")
pcylab <- paste("PC2 (", round(PoV[2] * 100, 2), "%)")
pczlab <- paste("PC3 (", round(PoV[3] * 100, 2), "%)")

# 3D as points
scatter3D(pcx, pcy, pcz, bty = "g", pch = 20, cex = 2, 
          col = gg.col(100), theta = 150, phi = 0, main = "PCA Scores", xlab = pcxlab,
          ylab =pcylab, zlab = pczlab)
text3D(pcx, pcy, pcz,  labels = rownames(res.pca$x), add = TRUE, colkey = FALSE, cex = 0.7)

# 3D as connected line
scatter3D(pcx, pcy, pcz, bty = "g", type = "b", pch = 20, cex = 2, 
          col = gg.col(100), theta = 120, phi = 0, lwd = 2, main = "PCA Scores", xlab = pcxlab,
          ylab =pcylab, zlab = pczlab)
text3D(pcx, pcy, pcz,  labels = rownames(res.pca$x), add = TRUE, colkey = FALSE, cex = 0.7)

# Rainbow version
df <- data.frame(comp1=pcx, comp2=pcy)
ggplot(data = df, aes(x=comp1, y=comp2, group=1)) +
  geom_point(size=5, aes(colour=rownames(res.pca$x))) +
  geom_path(size = 0.2) +
  geom_text(label=rownames(res.pca$x)) + 
  theme(legend.position="none")

ggplot(data = df, aes(x=comp1, y=comp2, group=1)) +
  geom_point(size=5, aes(colour=rownames(res.pca$x))) +
  geom_text(label=rownames(res.pca$x)) + 
  theme(legend.position="none")

#Plotly version
df3D <- data.frame(comp1=pcx, comp2=pcy, comp3=pcz)
fig <- plot_ly(df3D, x = ~comp1, y = ~comp2, z = ~comp3, color = ~comp3,  mode = 'lines+markers',
               # Hover text:
               text = ~rownames(res.pca$x))
fig <- fig %>% add_markers()
fig <- fig %>% add_text(textposition = "top right")
fig <- fig %>% layout(scene = list(xaxis = list(title = pcxlab),
                                   yaxis = list(title = pcylab),
                                   zaxis = list(title = pczlab)),
                      annotations = list(
                        x = 1.13,
                        y = 1.05,
                        text = 'PC3 Score',
                        showarrow = FALSE
                      ))
fig

################################# DENDROGRAMS ###############################
# Dendrogram flowing down the page
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
par(cex=1.0)
plot(as.phylo(hc), type = "fan")
colors = c("red", "blue", "green", "black", "yellow", "purple", "grey", "brown", "magenta", "cyan", "violetred", "tomato")
clus = cutree(hc, 12)
plot(as.phylo(hc), type = "fan", tip.color = colors[clus],label.offset = 1, cex = 0.3)
plot(as.phylo(hc), type = "fan", tip.color = colors[clus],label.offset = 1, cex = 1.0)


########################### CORRELATION RESULTS ###########################  
# Perform correlation on indicators
library(corrplot)
library(RColorBrewer)

M <-cor(pl.data, method="pearson")
M <-cor(pl.data, method="spearman")
corrplot(M, type="upper", order="hclust", col=brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.cex=0.5)
corrplot(M, type="upper", order="hclust", col=brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.cex=1.0)

############################# AREA plots #################################
col3Palette <- c(brewer.pal(9, "Oranges")[c(8, 7, 6, 5, 2)], 
                 brewer.pal(11, "PiYG")[c(1, 2, 3, 4, 5)],
                 brewer.pal(9, "Purples")[c(2, 4, 3, 6, 7, 8, 9)],
                 'mediumorchid4', 'mediumorchid3', 'mediumorchid2', 'mediumorchid1',
                 'orchid4', 'orchid3', 'orchid2', 'orchid1',
                 'hotpink4', 'hotpink2', 'hotpink1',
                 'violetred2', 'violetred3', 'violetred4',
                 brewer.pal(9, "Greens")[c(2, 6, 7, 8, 9)],
                 'turquoise4', 'turquoise3', 'turquoise2', 'turquoise1', 'paleturquoise1',
                 brewer.pal(9, "Blues")[c(1, 2, 3, 4, 5, 6, 7, 8, 9)],
                 'royalblue3', 'darkslateblue', 'midnightblue',
                 brewer.pal(9, "BrBG")[c(1, 2, 3, 4, 5)], 
                 'chocolate4', 'chocolate3', 'chocolate2', 'chocolate1',
                 brewer.pal(9, "BrBG")[c(3, 2, 1)], 
                 'goldenrod2', 'goldenrod1', 'lightgoldenrod1', 'khaki4',
                 'black', brewer.pal(9, "Greys")[c(5, 4, 3, 2)],
                 'aliceblue', 'darkred', 'red3', 'red1',
                 'firebrick4', 'firebrick3', 'firebrick1', 'rosybrown1')

pl.data <- read.table(filename,sep=",",header=T)
final_catch <- reshape2::melt(pl.data, id = c("Year"))
colnames(final_catch)[2:3] <- c("Species", "Catch")
colourCount = length(unique(final_catch$Species))
ggplot(data = final_catch, aes(x = Year, y = Catch, fill = Species)) + geom_bar(colour = "black", stat="identity", size = 0.1) +
  scale_fill_manual(values = col3Palette) + theme_bw() +
  labs(x="Years", y = "Tonnes") + theme(legend.position = "none") +
  theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=20,face="bold"))

ggplot(data = final_catch, aes(x = Year, y = Catch, fill = Species)) + geom_bar(colour = "black", stat="identity", size = 0.1) +
  scale_fill_manual(name = "Species", values = col3Palette) + theme_bw() +
  labs(x="Years", y = "Tonnes") + theme(legend.text=element_text(size=10)) +
  theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=14,face="bold"))

ggplot(data = final_catch, aes(x = Year, y = Catch, fill = Species)) + geom_bar(colour = "black", position="fill", stat="identity", size = 0.1) +
  scale_fill_manual(name = "Species", values = col3Palette) + theme_bw() +
  labs(x="Years", y = "proportion of catch") + theme(legend.text=element_text(size=10)) +
  theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=14,face="bold"))

################################# Heatmap ####################################################
# Create row names and trim off first column
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
  drRanked[i,] <- frank(Yr_as_col[i,], ties.method ="dense")
}

# Create the heatmap - switch our row for both to see if years cluster up
HeatmapTitle <- paste ("Heatmap of contribution to catch through time, yellow represents a larger contribution",sep="") 
HeatmapFile <- "HeatmapFilename.pdf"

heatmaply(drRanked,
          xlab = "Year",
          ylab = "Species",
          main = HeatmapTitle,
          dendrogram = "row",
          #dendrogram = "both",
          fontsize_row = 10,
          file = HeatmapFile
)
