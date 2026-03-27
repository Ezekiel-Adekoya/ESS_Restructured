# ══════════════════════════════════════════════════════════════
# FISH COMMUNITY PCA & CHANGEPOINT ANALYSIS
# ══════════════════════════════════════════════════════════════
#install.packages(c("tidyverse", "FactoMineR", "factoextra",
                   "trend", "segmented", "patchwork", "scales"))

library(tidyverse); library(FactoMineR); library(factoextra)
library(trend); library(segmented); library(patchwork); library(scales);library(dplyr)
library(purrr);library(stringr)

select <- dplyr::select  # prevent masking conflicts

# ── 1. DATA LOADING ───────────────────────────────────────────
#long_df <- read.csv("Ess_data/Alida_Qadj_Biomass.csv") - no file
# filter fish only codes and years 1970-2017, then identify codes with >=5 consecutive years of presence (BIOMASS>0 if BIOMASS exists) to retain for PCA; also restrict to species$CODE if available
long_df <- read.csv("Ess_data/ESS q-adj data for Ezekiel 1970-2022.csv") |>
  filter(SPECIES < 1000, YEAR >=1978, YEAR <= 2022) |>
  filter(!is.na(SPECIES_NAME), !grepl("INCL |snail|unidentified|GADOIDEI|unid.|S.O.", SPECIES_NAME, ignore.case = TRUE))  |> 
  filter(!SPECIES_NAME %in% c(
    "INCL SHARK & CHONDRICHTHYES; SHARK",
    "SAND LANCES",
    "SKATES",
    "EELPOUTS"
  ))|>
rename(Year = YEAR, Species = SPECIES_NAME)

# check species inlcuded in long_df
unique(long_df$Species)
unique(long_df$SPECIES)

write.csv(long_df, "Ess_data/long_df_for_PCA.csv", row.names = FALSE)

# ── 2. PCA PREPARATION ────────────────────────────────────────
# Log-transform, drop sparse columns (>20 non-NA), impute with median
# R
long_wide <- long_df|>
  dplyr::group_by(Year, Species) |>
  dplyr::summarise(Biomass = sum(BIOMASS, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = Species, values_from = Biomass, values_fill = 0)

Biomass_pca <- long_wide |>
  dplyr::arrange(Year) |>
  tibble::column_to_rownames("Year") |>
  dplyr::mutate(across(everything(), log1p)) |>
  dplyr::select(where(~sum(!is.na(.x)) > 20))

Biomass_pca[] <- lapply(Biomass_pca, function(x) { x[is.na(x)] <- median(x, na.rm = TRUE); x })
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
year_scores$Cluster <- factor(kmeans(year_scores, centers=3)$cluster)
fviz_cluster(kmeans(year_scores[,1:2], centers=3),
             data=year_scores[,1:2], repel=TRUE,
             ellipse.alpha=0) +
  theme(panel.background=element_blank(), plot.background=element_blank())
year_scores

# ── 6. CHANGEPOINT ANALYSIS — PC1 ────────────────────────────
# Pettitt test: non-parametric single changepoint detection
pettitt.test(year_scores$Dim.1)

# Mann-Kendall: monotonic trend test
mk.test(ts(year_scores$Dim.1, start=1978))

# Segmented regression: structural breakpoint with CI (2018 excluded)
df_clean <- data.frame(year = as.integer(rownames(year_scores)),
                       dim1 = year_scores$Dim.1) #|>
  #filter(year != 2018)
seg_fit2 <- segmented(lm(dim1 ~ year, data=df_clean), seg.Z=~year, psi=c(1992,2000))
summary(seg_fit2); confint(seg_fit2)
plot(seg_fit2, col="red", lwd=2)

# ── 7. CHANGEPOINT ANALYSIS — PC2 ────────────────────────────
pettitt.test(year_scores$Dim.2)
mk.test(ts(year_scores$Dim.2, start=1978))

df2_clean <- data.frame(year = as.integer(rownames(year_scores)),
                        dim2 = year_scores$Dim.2) |>
  filter(year != 2018)
seg_fit3 <- segmented(lm(dim2 ~ year, data=df2_clean), seg.Z=~year, psi=c(1992,2000))
summary(seg_fit3); confint(seg_fit3)
plot(seg_fit3, col="red", lwd=2)

# ── 8. BIOMASS CONTRIBUTION BY PHASE ─────────────────────────
# Pre/post-moratorium split at 1992 based on cod moratorium (1992)
long_df_phase <- long_df |>
   #dplyr::select(-matches("^NA\\.?$"),
        #        -any_of(c("Sharks..Unspecified.", "Pandalus.borealis",
         #                 "Holothuroidea", "Sea.Cucumber"))) |>
  filter(between(Year, 1978, 2022)) |>
  #filter(between(Year, 1978, 2017, Year != 2018) |> # if oulier is to be removed
  mutate(Phase = factor(if_else(Year <= 1992,
                                "1978-1992 (Pre-Moratorium)",
                                "1993-2022 (Post-Moratorium)"),
                        levels = c("1978-1992 (Pre-Moratorium)",
                                   "1993-2022 (Post-Moratorium)"))) |>
  #pivot_longer(-c(Year, Phase), names_to="Species", values_to="BIOMASS") |>
  filter(!is.na(BIOMASS), BIOMASS > 0) |>
  group_by(Species) |> filter(n() >= 5) |> ungroup()

phase_pct <- long_df_phase |>
  group_by(Phase, Species) |>
  summarise(BIOMASS = sum(BIOMASS)) |>
  mutate(Pct = BIOMASS / sum(BIOMASS)) |>
  ungroup()
  

phase_pct <- phase_pct|>
arrange(Phase, desc(Pct))|>
  group_by(Phase) |>
  mutate(Rank=row_number(), CumPct=cumsum(Pct)) |>
  filter(is.finite(Pct)) |>
  mutate(Species = str_to_sentence(str_squish(str_replace_all(Species, "\\.+", " "))))

pal <- c("1978-1992 (Pre-Moratorium)" = "#E07B2A",
  "1993-2022 (Post-Moratorium)" = "#2471A3")



make_phase_plot <- function(ph,df=phase_pct) {
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

make_phase_plot("1978-1992 (Pre-Moratorium)") +
  make_phase_plot("1993-2022 (Post-Moratorium)") +
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



###### For determinning period to average over for steady state calibration; 

library(zoo)


# Calculate stability metrics for each year
year_scores_stable <- year_scores |>
  rownames_to_column("Year") |>
  mutate(
    Year = as.integer(Year),
    euclidean_dist = sqrt(Dim.1^2 + Dim.2^2)  # distance from origin
  ) |>
  mutate(
    rolling_mad = zoo::rollapply(euclidean_dist, width = 3, FUN = function(x) median(abs(x - median(x))), fill = NA),
    rolling_sd = zoo::rollapply(euclidean_dist, width = 3, FUN = sd, fill = NA),
    is_stable = rolling_mad < median(rolling_mad, na.rm = TRUE)
  )

# Extract stable periods
stable_periods <- year_scores_stable |>
  filter(is_stable) |>
  summarise(
    min_year = min(Year, na.rm = TRUE),
    max_year = max(Year, na.rm = TRUE),
    n_years = n(),
    mean_dim1 = mean(Dim.1),
    mean_dim2 = mean(Dim.2)
  )

# Or use your changepoints from segmented regression directly
breakpoint <- confint(seg_fit2)["psi1.year", 1]

phase_means <- year_scores |>
  rownames_to_column("Year") |>
  mutate(Year = as.integer(Year), Phase = Year <= breakpoint) |>
  group_by(Phase) |>
  summarise(
    across(starts_with("Dim"), mean, na.rm = TRUE),
    year_range = paste(min(Year), "-", max(Year))
  )