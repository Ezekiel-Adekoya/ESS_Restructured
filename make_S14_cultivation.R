## =============================================================================
## make_S14_cultivation.R  — Supplementary Fig S14
## DIRECT per-web evidence of the forage-cultivation mechanism from the paired
## ensemble: strength of the (herring + sand lance) -> cod cultivation interaction
## vs cod biomass change over the paper's 1986-2024 PROJECTION window.
##
## Window convention MATCHES the rest of the paper (03/04): the 1978-1985 period is
## the calibration/reference (grey in Figs), 1986-2024 is the projection. So cod
## "change" is measured relative to the 1978-1985 REFERENCE MEAN (robust to single-
## year noise), evaluated at the terminal 2024 state, in the FACTUAL (seal-increase)
## world. Cod biomass uses the SAME cutoff-based method as 03 (term_bio).
##
## Cultivation strength for web k = mean of the perturbed interaction entries
## interaction[c("Atlantic Herring","Sand Lance"), "Atlantic Cod"] (row=forage
## predator, col=cod prey; base value 0.9 set in 02's cult_prey). Read directly
## from each web's stored params, so it is exactly the value that drove the sim.
##
## Runs after run_ensemble_sims.R (needs sims_factual.RDS). Reproducible.
## =============================================================================
suppressMessages({library(mizer); library(mizerExperimental); library(tidyverse)})

p  <- readRDS("Ess_data/params_stage2_AUTO.RDS")
sf <- readRDS("Ess_Output/sims_factual.RDS")          # factual = seal increase
cat(sprintf("Ensemble: %d webs\n", length(sf)))

## ── Cod biomass, cutoff-based, IDENTICAL to 03's term_bio ────────────────────
cutoff <- species_params(p)$biomass_cutoff; cutoff[is.na(cutoff)] <- 0
spp    <- species_params(p)$species
cod_j  <- which(spp == "Atlantic Cod")
wdw    <- p@w * p@dw
cod_bio <- function(sim, years) {                     # mean cod biomass (kt) over `years`
  keep <- p@w >= cutoff[cod_j]
  vapply(as.character(years), function(y)
    sum(sim@n[y, cod_j, keep] * wdw[keep]) / 1e9, numeric(1)) |> mean()
}

ref_years <- 1978:1985                                # paper's reference period
end_year  <- 2024

## ── Cultivation strength per web (herring + sand lance -> cod) ────────────────
cult_rows <- c("Atlantic Herring", "Sand Lance")
cultivation <- vapply(sf, function(s)
  mean(s@params@interaction[cult_rows, "Atlantic Cod"]), numeric(1))

## ── Cod change over the 1986-2024 projection: (2024 - refmean)/refmean * 100 ──
cod_ref <- vapply(sf, cod_bio, numeric(1), years = ref_years)
cod_end <- vapply(sf, cod_bio, numeric(1), years = end_year)
cod_pct <- 100 * (cod_end - cod_ref) / cod_ref

df <- tibble(web = seq_along(sf), cultivation, cod_ref, cod_end, cod_pct)

## ── Robustness diagnostics ───────────────────────────────────────────────────
r_p <- cor(df$cultivation, df$cod_pct, method = "pearson")
r_s <- cor(df$cultivation, df$cod_pct, method = "spearman")
# guard against tiny-denominator blow-ups: drop webs with a near-zero reference stock
keep <- df$cod_ref > quantile(df$cod_ref, 0.05)
r_p_trim <- cor(df$cultivation[keep], df$cod_pct[keep], method = "pearson")
terc <- cut(df$cultivation, quantile(df$cultivation, c(0, 1/3, 2/3, 1)),
            include.lowest = TRUE, labels = c("weak", "mid", "strong"))
tmed <- tapply(df$cod_pct, terc, median)

cat(sprintf("cultivation range: %.2f-%.2f (mean %.2f)\n",
            min(cultivation), max(cultivation), mean(cultivation)))
cat(sprintf("cor(cultivation, cod %% change 1986-2024): Pearson %.2f | Spearman %.2f | trimmed %.2f\n",
            r_p, r_s, r_p_trim))
cat(sprintf("cod %% change by cultivation tercile: weak %.0f  mid %.0f  strong %.0f\n",
            tmed["weak"], tmed["mid"], tmed["strong"]))
cat(sprintf("cod ref-period biomass range (kt): %.1f - %.1f\n",
            min(cod_ref), max(cod_ref)))

## ── Figure ───────────────────────────────────────────────────────────────────
P <- ggplot(df, aes(cultivation, cod_pct)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_point(alpha = 0.18, size = 0.9, colour = "grey20") +
  geom_smooth(method = "lm", colour = "firebrick", fill = "grey70", linewidth = 1) +
  labs(x = "Herring + sand lance cultivation of cod (interaction strength)",
       y = "Cod biomass change 1986-2024 (%)") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank())

dir.create("figures/supplementary", recursive = TRUE, showWarnings = FALSE)
ggsave("figures/supplementary/figureS14_cultivation_vs_cod.jpeg", P,
       width = 7.5, height = 5.5, dpi = 200)
cat("Saved figures/supplementary/figureS14_cultivation_vs_cod.jpeg\n")
