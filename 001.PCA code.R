# ══════════════════════════════════════════════════════════════
# BENTHIC COMMUNITY PCA & CHANGEPOINT ANALYSIS
# ══════════════════════════════════════════════════════════════

library(tidyverse); library(FactoMineR); library(factoextra)
library(trend); library(segmented); library(patchwork); library(scales)

select <- dplyr::select  # prevent masking conflicts

# ── 1. DATA LOADING ───────────────────────────────────────────
long_df <- read.csv("Ess_data/Alida_Qadj_Biomass.csv") |>
  dplyr::select(-matches("^NA\\.?$"),
                -any_of(c("Sharks..Unspecified.", "Pandalus.borealis",
                          "Holothuroidea", "Sea.Cucumber"))) |>
  filter(between(Year, 1979, 2020))
# filter(between(Year, 1979, 2020), Year != 2018)  # optional: exclude outlier

# ── 2. PCA PREPARATION ────────────────────────────────────────
# Log-transform, drop sparse columns (>20 non-NA), impute with median
Biomass_pca <- long_df |>
  column_to_rownames("Year") |>
  mutate(across(everything(), log1p)) |>
  dplyr::select(where(~sum(!is.na(.x)) > 20))
Biomass_pca[] <- lapply(Biomass_pca, function(x) { x[is.na(x)] <- median(x, na.rm=TRUE); x })

# ── 3. RUN PCA ────────────────────────────────────────────────
pca_res <- PCA(Biomass_pca, scale.unit=TRUE, graph=FALSE)

# ── 4. PCA VISUALISATION ──────────────────────────────────────
# Biplots for PC1-2 and PC3-4
bp_args <- list(pca_res, repel=TRUE, col.var="contrib",
                gradient.cols=c("#00AFBB","#E7B800","#FC4E07"))
do.call(fviz_pca_biplot, bp_args)
do.call(fviz_pca_biplot, c(bp_args, axes=list(c(3,4))))

# Variable contributions and quality of representation
fviz_contrib(pca_res, choice="var", axes=1, top=20)
fviz_contrib(pca_res, choice="var", axes=2, top=20)
fviz_cos2(pca_res,   choice="var", axes=1:2)

# Cos² bar chart with 0.25 threshold line
cos2_df <- data.frame(Species = rownames(get_pca_var(pca_res)$cos2),
                      Cos2    = rowSums(get_pca_var(pca_res)$cos2[, 1:2])) |>
  arrange(desc(Cos2))

ggplot(cos2_df, aes(reorder(Species, Cos2), Cos2)) +
  geom_col(fill="steelblue", width=0.5) +
  geom_hline(yintercept=0.25, linetype="dashed", colour="red") +
  coord_flip() +
  labs(title="Quality of variable representation on PC1–PC2",
       subtitle="Red line = cos² threshold (0.25)", x=NULL, y="Cos²") +
  theme_minimal() +
  theme(axis.text.y = element_text(face="bold"))

# ── 5. K-MEANS CLUSTERING ON PC1–PC2 ─────────────────────────
set.seed(42)
year_scores         <- as.data.frame(pca_res$ind$coord[, 1:2])
year_scores$Cluster <- factor(kmeans(year_scores, centers=2)$cluster)
fviz_cluster(kmeans(year_scores[,1:2], centers=2),
             data=year_scores[,1:2], repel=TRUE,
             ellipse.alpha=0) +
  theme(panel.background=element_blank(), plot.background=element_blank())
year_scores

# ── 6. CHANGEPOINT ANALYSIS — PC1 ────────────────────────────
# Pettitt test: non-parametric single changepoint detection
pettitt.test(year_scores$Dim.1)

# Mann-Kendall: monotonic trend test
mk.test(ts(year_scores$Dim.1, start=1979))

# Segmented regression: structural breakpoint with CI (2018 excluded)
df_clean <- data.frame(year = as.integer(rownames(year_scores)),
                       dim1 = year_scores$Dim.1) |>
  filter(year != 2018)
seg_fit2 <- segmented(lm(dim1 ~ year, data=df_clean), seg.Z=~year, psi=1994)
summary(seg_fit2); confint(seg_fit2)

# ── 7. CHANGEPOINT ANALYSIS — PC2 ────────────────────────────
pettitt.test(year_scores$Dim.2)
mk.test(ts(year_scores$Dim.2, start=1979))

df2_clean <- data.frame(year = as.integer(rownames(year_scores)),
                        dim2 = year_scores$Dim.2) |>
  filter(year != 2018)
seg_fit3 <- segmented(lm(dim2 ~ year, data=df2_clean), seg.Z=~year, psi=1998)
summary(seg_fit3); confint(seg_fit3)

# ── 8. BIOMASS CONTRIBUTION BY PHASE ─────────────────────────
# Pre/post-moratorium split at 1992 based on cod moratorium (1992)
long_df_phase <- read.csv("Ess_data/Alida_Qadj_Biomass.csv") |>
  dplyr::select(-matches("^NA\\.?$"),
                -any_of(c("Sharks..Unspecified.", "Pandalus.borealis",
                          "Holothuroidea", "Sea.Cucumber"))) |>
  filter(between(Year, 1979, 2020)) |>
  #filter(between(Year, 1979, 2020), Year != 2018) |> # if oulier is to be removed
  mutate(Phase = factor(if_else(Year <= 1992,
                                "1979-1992 (Pre-Moratorium)",
                                "1993-2020 (Post-Moratorium)"),
                        levels = c("1979-1992 (Pre-Moratorium)",
                                   "1993-2020 (Post-Moratorium)"))) |>
  pivot_longer(-c(Year, Phase), names_to="Species", values_to="Biomass") |>
  filter(!is.na(Biomass), Biomass > 0) |>
  group_by(Species) |> filter(n() >= 5) |> ungroup()

phase_pct <- long_df_phase |>
  summarise(Biomass = sum(Biomass), .by=c(Phase, Species)) |>
  mutate(Pct = Biomass / sum(Biomass), .by=Phase) |>
  arrange(Phase, desc(Pct)) |>
  mutate(Rank=row_number(), CumPct=cumsum(Pct), .by=Phase) |>
  filter(is.finite(Pct)) |>
  mutate(Species = str_to_sentence(str_squish(str_replace_all(Species, "\\.+", " "))))

pal <- c("1979-1992 (Pre-Moratorium)" = "#E07B2A",
         "1993-2020 (Post-Moratorium)" = "#2471A3")

make_phase_plot <- function(ph) {
  col      <- pal[[ph]]
  cut_rank <- phase_pct |> filter(Phase==ph) |>
    summarise(min(Rank[CumPct >= 0.95])) |> pull()
  df <- phase_pct |> filter(Phase==ph) |>
    mutate(Species=factor(Species, levels=rev(unique(Species))),
           Above95=Rank <= cut_rank)
  ypos <- as.numeric(df$Species[df$Rank==cut_rank][1]) - 0.5
  
  ggplot(df, aes(Pct, Species)) +
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=ypos, ymax=Inf, fill=col, alpha=0.06) +
    geom_col(aes(alpha=Above95), fill=col, colour="white", linewidth=0.2, width=0.72) +
    scale_alpha_manual(values=c("TRUE"=0.85,"FALSE"=0.45), guide="none") +
    geom_hline(yintercept=ypos, colour=col, linetype="dashed", linewidth=0.8) +
    annotate("label", x=Inf, y=ypos, label="95% cumulative biomass",
             colour=col, fill="white", label.size=0,
             hjust=1.1, vjust=-0.2, size=2.5, fontface="bold") +
    scale_x_continuous(labels=percent_format(accuracy=1),
                       expand=expansion(mult=c(0,.28))) +
    labs(title=ph, x=NULL, y=NULL) +
    theme_classic(base_size=10, base_family="serif") +
    theme(
      panel.border       = element_rect(colour=col, fill=NA, linewidth=1.2),
      plot.background    = element_rect(colour=col, fill="white", linewidth=0.4),
      plot.title         = element_text(face="bold", size=10, colour=col, margin=margin(b=6)),
      axis.text.y        = element_text(size=7.5, colour="grey20"),
      axis.text.x        = element_text(size=7, colour="grey40"),
      axis.ticks         = element_line(colour="grey60", linewidth=0.3),
      axis.line          = element_blank(),
      panel.grid.major.x = element_line(colour="grey90", linewidth=0.3),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.margin        = margin(10, 14, 10, 10)
    )
}

make_phase_plot("1979-1992 (Pre-Moratorium)") +
  make_phase_plot("1993-2020 (Post-Moratorium)") +
  plot_layout(ncol=2) +
  plot_annotation(
    title    = "Species contribution to total biomass by time period",
    subtitle = "Shaded area = species accounting for 95% of cumulative biomass",
    theme    = theme(
      plot.title      = element_text(face="bold", size=12, family="serif", margin=margin(b=3)),
      plot.subtitle   = element_text(colour="grey40", size=9, family="serif", margin=margin(b=10)),
      plot.background = element_rect(fill="white", colour=NA)
    )
  )