## =============================================================================
## make_S6_seal_diet.R  — Supplementary Fig S6 (corrected)
## Grey-seal diet: model EMERGENT diet vs Bowen & Harrison (1994).
## KEY FIX: Bowen sampled offshore Sable Is. in the early 1990s — AFTER the
## sand-lance irruption. The seal diet is strongly time-varying, so we compare
## Bowen to the model's diet IN THE BOWEN ERA (1990-1995), not the 1978 reference
## state (pre-outburst, flatfish-dominated). We show all three for context.
## Runs after 02 (needs params_stage2_AUTO). Reproducible: same inputs -> same fig.
## =============================================================================
suppressMessages({library(mizer); library(mizerExperimental); library(tidyverse)})

p   <- readRDS("Ess_data/params_stage2_AUTO.RDS")
gc  <- c("Bottom_Trawl","Fixed_Gear","Small_Mesh","Pelagic","Seals")
e   <- read.csv("Ess_data/effort_gear_with_seals.csv")
m   <- as.matrix(e[e$Year %in% 1978:2024, gc]); rownames(m) <- e$Year[e$Year %in% 1978:2024]
sim <- project(p, effort = m)

# Emergent seal diet composition (% of consumption) at a given year.
# = seal catchability x lognormal size-selectivity x abundance x weight, integrated.
# (The time-varying seal effort is a scalar on all prey, so it cancels in the %.)
g   <- gear_params(p); sealg <- g[g$gear == "Seals", ]
sel <- function(w, w_opt, sigma = 0.75) exp(-0.5 * ((log(w) - log(w_opt)) / sigma)^2)
w   <- p@w; dw <- p@dw; spp <- species_params(p)$species
diet_year <- function(years) {
  n <- if (length(years) == 1) sim@n[as.character(years), , ]
       else apply(sim@n[as.character(years), , , drop = FALSE], c(2, 3), mean)
  cons <- sapply(seq_along(spp), function(j) {
    r <- sealg[sealg$species == spp[j], ]; if (!nrow(r)) return(0)
    sum(r$catchability * sel(w, r$w_opt) * n[j, ] * w * dw)
  })
  setNames(100 * cons / sum(cons), spp)
}
d_ref   <- diet_year(1978)          # pre-collapse reference
d_bowen <- diet_year(1990:1995)     # Bowen sampling era (post-irruption)

# Bowen & Harrison (1994) Table 2, % wet weight (offshore Sable Island)
bowen <- c("Sand Lance"=80.9, "Atlantic Cod"=11.0, "Silver Hake"=2.6,
           "American Plaice"=1.4, "Redfish Unseparated"=1.3, "Yellowtail Flounder"=1.1,
           "Witch Flounder"=0.4, "Capelin"=0.4, "Pollock"=0.2, "White Hake"=0.2,
           "Atlantic Herring"=0.2, "Haddock"=0.05)

df <- tibble(species = spp,
             `Model 1978\n(pre-collapse)`      = round(d_ref[spp], 1),
             `Model 1990-95\n(Bowen era)`      = round(d_bowen[spp], 1),
             `Bowen & Harrison\n1994`          = ifelse(spp %in% names(bowen), bowen[spp], 0)) %>%
  pivot_longer(-species, names_to = "source", values_to = "pct") %>%
  mutate(source = factor(source, levels = c("Model 1978\n(pre-collapse)",
                                            "Model 1990-95\n(Bowen era)",
                                            "Bowen & Harrison\n1994")))
ord <- names(sort(d_bowen + (spp %in% names(bowen)) * 1e-6, decreasing = TRUE))

P <- ggplot(df, aes(source, factor(species, levels = rev(ord)), fill = pct)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = ifelse(pct >= 1, round(pct), "")), size = 2.7,
            colour = ifelse(df$pct > 45, "white", "grey20")) +
  scale_fill_distiller(palette = "Blues", direction = 1, name = "% diet") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(face = "bold"))

ggsave("Ess_Output/seal_diet_vs_bowen.jpeg", P, width = 7.5, height = 6, dpi = 200)
dir.create("figures/supplementary", recursive = TRUE, showWarnings = FALSE)
file.copy("Ess_Output/seal_diet_vs_bowen.jpeg",
          "figures/supplementary/figureS6_seal_diet.jpeg", overwrite = TRUE)
cat("Saved S6 (1994-vs-Bowen).\n")
cat(sprintf("Sand lance in seal diet: 1978 = %.0f%% | Bowen era (1990-95) = %.0f%% | Bowen = %.0f%%\n",
            d_ref["Sand Lance"], d_bowen["Sand Lance"], bowen["Sand Lance"]))
