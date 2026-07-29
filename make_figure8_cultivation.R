## =============================================================================
## make_figure8_cultivation.R  — MAIN-TEXT Figure 8
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
suppressMessages({library(mizer); library(mizerExperimental); library(tidyverse); library(patchwork)})

p  <- readRDS("Ess_data/params_stage2_AUTO.RDS")
sf <- readRDS("Ess_Output/sims_factual.RDS")          # factual = seal increase
sc <- readRDS("Ess_Output/sims_counter.RDS")          # counterfactual = seals constant
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
## Cultivation is a property of the WEB (same interaction matrix in both scenarios).
cult_rows <- c("Atlantic Herring", "Sand Lance")
cultivation <- vapply(sf, function(s)
  mean(s@params@interaction[cult_rows, "Atlantic Cod"]), numeric(1))

## ── Cod change over the 1986-2024 projection: (2024 - refmean)/refmean * 100 ──
## Same web run under both efforts: factual (seal increase) vs counterfactual.
cod_pct_scen <- function(sims) {
  ref <- vapply(sims, cod_bio, numeric(1), years = ref_years)
  end <- vapply(sims, cod_bio, numeric(1), years = end_year)
  list(ref = ref, pct = 100 * (end - ref) / ref)
}
fac <- cod_pct_scen(sf); cou <- cod_pct_scen(sc)

## Panel A = FACTUAL marginal; Panel B = PAIRED seal effect = the TREATMENT effect of
## the seal increase = factual - counterfactual (same cod %-change scale). NEGATIVE
## Panel-B values = the seal increase SUPPRESSED cod recovery.
df  <- tibble(web = seq_along(sf), cultivation,
              cod_ref = fac$ref, cod_pct = fac$pct,        # factual
              seal_effect = fac$pct - cou$pct)             # factual - counterfactual

## ── Diagnostics ──────────────────────────────────────────────────────────────
terc <- cut(df$cultivation, quantile(df$cultivation, c(0, 1/3, 2/3, 1)),
            include.lowest = TRUE, labels = c("weak","mid","strong"))
cat(sprintf("cultivation range: %.2f-%.2f (mean %.2f)\n",
            min(cultivation), max(cultivation), mean(cultivation)))
cat(sprintf("Panel A (factual): Pearson %.2f | Spearman %.2f | tercile %.0f/%.0f/%.0f\n",
            cor(df$cultivation, df$cod_pct), cor(df$cultivation, df$cod_pct, method="spearman"),
            median(df$cod_pct[terc=="weak"]), median(df$cod_pct[terc=="mid"]),
            median(df$cod_pct[terc=="strong"])))
cat(sprintf("Panel B (seal effect = factual-counterfactual, pct-pts): median %.1f | %% webs <0 %.1f | vs cultivation r %.2f | tercile %.1f/%.1f/%.1f\n",
            median(df$seal_effect), 100*mean(df$seal_effect < 0),
            cor(df$cultivation, df$seal_effect),
            median(df$seal_effect[terc=="weak"]), median(df$seal_effect[terc=="mid"]),
            median(df$seal_effect[terc=="strong"])))

## ── Figure: 2 panels ─────────────────────────────────────────────────────────
xlab <- "Herring + sand lance cultivation of cod (interaction strength)"
pA <- ggplot(df, aes(cultivation, cod_pct)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_point(alpha = 0.16, size = 0.9, colour = "grey20") +
  geom_smooth(method = "lm", colour = "firebrick", fill = "grey70", linewidth = 1) +
  labs(x = xlab, y = "Cod biomass change 1986-2024 (%)",
       subtitle = "(a) Factual (seal increase)") +
  theme_bw(base_size = 11) + theme(panel.grid.minor = element_blank())

pB <- ggplot(df, aes(cultivation, seal_effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_point(alpha = 0.16, size = 0.9, colour = "steelblue4") +
  geom_smooth(method = "lm", colour = "steelblue4", fill = "grey70", linewidth = 1) +
  annotate("text", x = mean(range(df$cultivation)), y = min(df$seal_effect),
           hjust = 0.5, vjust = 0, label = "below 0 = seal increase suppresses cod",
           size = 3, colour = "grey35") +
  labs(x = xlab, y = "Seal effect on cod change (factual - counterfactual, % pts)",
       subtitle = "(b) Grey-seal effect (paired, within-web)") +
  theme_bw(base_size = 11) + theme(panel.grid.minor = element_blank())

P <- pA + pB + plot_layout(ncol = 2)

## Working output in Ess_Output/ (gitignored, regenerable), then copy the tracked
## MANUSCRIPT figure into ./figures — same convention as 03/04's main figures.
ggsave("Ess_Output/cultivation_vs_cod.jpeg", P, width = 12, height = 5.5, dpi = 200)
dir.create("figures", showWarnings = FALSE)
invisible(file.copy("Ess_Output/cultivation_vs_cod.jpeg",
                    "figures/figure8_cultivation.jpeg", overwrite = TRUE))
cat("Saved figures/figure8_cultivation.jpeg\n")
