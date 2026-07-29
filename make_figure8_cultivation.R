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
suppressMessages({library(mizer); library(mizerExperimental); library(tidyverse)})

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

df <- bind_rows(
  tibble(web = seq_along(sf), cultivation, cod_ref = fac$ref, cod_pct = fac$pct,
         Scenario = "Factual (seal increase)"),
  tibble(web = seq_along(sc), cultivation, cod_ref = cou$ref, cod_pct = cou$pct,
         Scenario = "Counterfactual (seals constant)")
) |> mutate(Scenario = factor(Scenario,
      levels = c("Counterfactual (seals constant)", "Factual (seal increase)")))

## ── Robustness diagnostics (per scenario) ────────────────────────────────────
diag <- function(d) {
  keep <- d$cod_ref > quantile(d$cod_ref, 0.05)
  terc <- cut(d$cultivation, quantile(d$cultivation, c(0, 1/3, 2/3, 1)),
              include.lowest = TRUE, labels = c("weak","mid","strong"))
  tmed <- tapply(d$cod_pct, terc, median)
  cat(sprintf("  Pearson %.2f | Spearman %.2f | trimmed %.2f | tercile %.0f/%.0f/%.0f\n",
              cor(d$cultivation, d$cod_pct), cor(d$cultivation, d$cod_pct, method="spearman"),
              cor(d$cultivation[keep], d$cod_pct[keep]),
              tmed["weak"], tmed["mid"], tmed["strong"]))
}
cat(sprintf("cultivation range: %.2f-%.2f (mean %.2f)\n",
            min(cultivation), max(cultivation), mean(cultivation)))
cat("Factual (seal increase):\n");         diag(filter(df, Scenario == "Factual (seal increase)"))
cat("Counterfactual (seals constant):\n"); diag(filter(df, Scenario == "Counterfactual (seals constant)"))

## ── Figure ───────────────────────────────────────────────────────────────────
P <- ggplot(df, aes(cultivation, cod_pct, colour = Scenario, fill = Scenario)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_point(alpha = 0.16, size = 0.9) +
  geom_smooth(method = "lm", linewidth = 1) +
  scale_colour_manual(values = c("Counterfactual (seals constant)" = "steelblue4",
                                 "Factual (seal increase)"         = "firebrick")) +
  scale_fill_manual(values   = c("Counterfactual (seals constant)" = "steelblue4",
                                 "Factual (seal increase)"         = "firebrick")) +
  labs(x = "Herring + sand lance cultivation of cod (interaction strength)",
       y = "Cod biomass change 1986-2024 (%)", colour = NULL, fill = NULL) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

## Working output in Ess_Output/ (gitignored, regenerable), then copy the tracked
## MANUSCRIPT figure into ./figures — same convention as 03/04's main figures.
ggsave("Ess_Output/cultivation_vs_cod.jpeg", P, width = 7.5, height = 5.5, dpi = 200)
dir.create("figures", showWarnings = FALSE)
invisible(file.copy("Ess_Output/cultivation_vs_cod.jpeg",
                    "figures/figure8_cultivation.jpeg", overwrite = TRUE))
cat("Saved figures/figure8_cultivation.jpeg\n")
