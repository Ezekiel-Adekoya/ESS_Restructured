## =============================================================================
## run_ensemble_sims.R
## Generate the n=1000 PAIRED randomised-web ensemble (factual vs counterfactual)
## that feeds 03_Scenarios_Through_time.Rmd and 04_Indicators.
##
## This is the EXPENSIVE step (~15 min). Run it deliberately — in RStudio or
## headless (`Rscript run_ensemble_sims.R`) — whenever `params` changes. It writes:
##   Ess_Output/sims_factual.RDS   (seal increase)
##   Ess_Output/sims_counter.RDS   (seals held constant at 1978 level)
## The 03 Rmd then reads those RDS via its cache guard and knits in seconds.
##
## Reproducible: run_paired_sims() is seed-locked (seed = 42), so results are
## bit-identical to an in-Rmd run. Function defs live in Ess_data/Useful_functions.R
## (single source of truth — do NOT redefine them here).
## =============================================================================

suppressPackageStartupMessages({
  library(here); library(mizer); library(mizerExperimental); library(tidyverse)
})
setwd(here::here())
source("Ess_data/Useful_functions.R")   # randomize_interaction_matrix + run_paired_sims

# sd of the food-web perturbation: default 0.1 (primary); override on the command line,
# e.g. `Rscript run_ensemble_sims.R 0.2` for the stress-test ensemble. Non-default sd
# writes suffixed RDS so it never overwrites the primary cache.
.args     <- commandArgs(trailingOnly = TRUE)
sd_web    <- if (length(.args) >= 1) as.numeric(.args[1]) else 0.1
n_sims    <- 1000
sd_suffix <- if (isTRUE(all.equal(sd_web, 0.1))) "" else sprintf("_sd%g", sd_web)
cache_f   <- sprintf("Ess_Output/sims_factual%s.RDS", sd_suffix)
cache_c   <- sprintf("Ess_Output/sims_counter%s.RDS", sd_suffix)

## ── Factual model (Stage 2) ──────────────────────────────────────────────────
params <- readRDS(if (file.exists("Ess_data/params_stage2_AUTO.RDS"))
                    "Ess_data/params_stage2_AUTO.RDS" else
                    "Ess_data/params_stage2_factual.RDS")

## ── Effort: FACTUAL = seal increase 1978->2024; COUNTERFACTUAL = seals constant ─
eff <- read.csv("Ess_data/effort_gear_with_seals.csv")
gear_cols <- c("Bottom_Trawl","Fixed_Gear","Small_Mesh","Pelagic","Seals")
effort_factual <- as.matrix(eff[eff$Year %in% 1978:2024, gear_cols])
rownames(effort_factual) <- eff$Year[eff$Year %in% 1978:2024]

# Counterfactual: seal effort HELD CONSTANT at the 1978 level; fishing identical.
effort_counter <- effort_factual
effort_counter[, "Seals"] <- effort_factual[1, "Seals"]

## ── Run the paired ensemble ──────────────────────────────────────────────────
cat(sprintf("Running %d paired sims (sd_web = %.2g, seed = 42)...\n", n_sims, sd_web))
paired <- run_paired_sims(params, n_sims = n_sims, sd = sd_web,
                          effort_factual = effort_factual,
                          effort_counter = effort_counter, seed = 42)
sims_factual <- paired$factual
sims_counter <- paired$counter

# Keep only COMPLETE pairs (drop index i if either scenario failed) so the two lists
# stay aligned & indexable: factual[[k]] and counter[[k]] are the SAME web.
ok <- which(!vapply(sims_factual, is.null, logical(1)) &
            !vapply(sims_counter, is.null, logical(1)))
sims_factual <- sims_factual[ok]; sims_counter <- sims_counter[ok]
cat(sprintf("Kept %d / %d complete paired ensemble members\n", length(ok), n_sims))

## ── Save (read by 03's cache guard and by 04_Indicators) ─────────────────────
saveRDS(sims_factual, cache_f)   # seal increase (factual)
saveRDS(sims_counter, cache_c)   # seals constant (counterfactual)
cat(sprintf("Saved %s + %s\n", cache_f, cache_c))



## GEar Plots...
library(tidyverse)
library(patchwork)

# Reshape effort_counter (excluding Seals)
effort_counter_df <- as.data.frame(effort_counter) %>%
  rownames_to_column("Year") %>%
  pivot_longer(-Year, names_to = "Gear", values_to = "Effort") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Gear != "Seals") %>%
  mutate(Gear = recode(Gear,
                       "Bottom_Trawl" = "Bottom Trawl",
                       "Fixed_Gear"   = "Fixed Gear",
                       "Small_Mesh"   = "Small Mesh"))

# Extract Seals from effort_counter (constant)
seal_counter <- as.data.frame(effort_counter) %>%
  rownames_to_column("Year") %>%
  mutate(Year = as.numeric(Year)) %>%
  dplyr::select(Year, Effort = Seals) %>%
  mutate(Scenario = "Counterfactual (Constant)")

# Extract Seals from effort_factual (increasing)
seal_factual <- as.data.frame(effort_factual) %>%
  rownames_to_column("Year") %>%
  mutate(Year = as.numeric(Year)) %>%
  dplyr::select(Year, Effort = Seals) %>%
  mutate(Scenario = "Factual (Increasing)")

# Combine both seal series
seal_combined <- bind_rows(seal_counter, seal_factual)

# Add seal as a facet-compatible data frame (use only one scenario for shape,
# color handled separately via geom layering)
seal_for_facet <- seal_combined %>%
  dplyr::select(Year, Effort, Scenario) %>%
  mutate(Gear = "Grey Seal Predation")

# Combine gear and seal into one data frame
effort_all_df <- bind_rows(
  effort_counter_df %>% mutate(Scenario = NA),
  seal_for_facet)

# Set facet order so seal panel appears last
gear_levels <- c("Bottom Trawl", "Fixed Gear", "Small Mesh", "Pelagic", "Grey Seal Predation")
effort_all_df <- effort_all_df %>%
  mutate(Gear = factor(Gear, levels = gear_levels))

# Plot
effort_all_df = ggplot(effort_all_df, aes(x = Year, y = Effort)) +
  # Gear lines (no scenario)
  geom_line(data = filter(effort_all_df, Gear != "Grey Seal Predation"),
            linewidth = 0.5, color = "grey30") +
  geom_point(data = filter(effort_all_df, Gear != "Grey Seal Predation"),
             size = 0.8, color = "grey20") +
  # Seal lines (with scenario color + linetype)
  geom_line(data = filter(effort_all_df, Gear == "Grey Seal Predation"),
            aes(color = Scenario, linetype = Scenario), linewidth = 0.5) +
  geom_point(data = filter(effort_all_df, Gear == "Grey Seal Predation"),
             aes(color = Scenario), size = 0.8) +
  scale_color_manual(
    values = c("Counterfactual (Constant)" = "steelblue4",
               "Factual (Increasing)"      = "firebrick4"),
    na.value = "grey30") +
  scale_linetype_manual(
    values = c("Counterfactual (Constant)" = "dashed",
               "Factual (Increasing)"      = "solid"),
    na.value = "solid") +
  facet_wrap(~ Gear, scales = "free_y", ncol = 2) +
  labs(
    x        = "Year",
    y        = "Relative Effort",
    color    = NULL,
    linetype = NULL) +
  theme_classic(base_size = 11) +
  theme(
    strip.text        = element_text(face = "bold", size = 10),
    strip.background  = element_blank(),
    axis.title        = element_text(face = "bold", size = 10),
    axis.text         = element_text(color = "black", size = 9),
    legend.position   = "bottom",
    legend.background = element_rect(fill = "white"),
    legend.text       = element_text(size = 11,face = "bold"),
    legend.key.width  = unit(1.4, "cm"),
    panel.border = element_rect(linewidth = 1.5),
   # panel.border      = element_rect(colour = "grey60", fill = NA, linewidth = 0.5),
    panel.spacing     = unit(0.8, "lines"),
    plot.margin       = margin(6, 8, 6, 8)) +
  theme(
    strip.text       = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "grey", color = NA),
  )

ggsave(filename = "Ess_Output/effort_by_gear.jpeg", plot = effort_all_df,
       width = 11, height = 7.5, units = "in", dpi = 400)
