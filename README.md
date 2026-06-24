# Eastern Scotian Shelf (4VsW) size-spectrum model

A 19-species `mizer` size-spectrum model for the Eastern Scotian Shelf, used to test
the hypothesis that the Atlantic cod **collapse** was driven by **fishing** and its
**lack of recovery** by the **grey-seal increase** (seal predation on juvenile cod,
reinforced by forage-fish "cultivation" of cod recruits).

The paper examines **one counterfactual** — *no seal increase* (seal abundance held at
its 1978 level instead of increasing to 2024) — and tests whether the resulting
difference in cod/groundfish recovery is **robust to food-web structural uncertainty**
(a randomised-interaction-matrix ensemble).

## Workflow (run in order)

Scripts are numbered by stage: **00 = data prep, 01 = calibration, 02 = scenarios, 03 = indicators.**

| Step | Script | Produces |
|---|---|---|
| **00** | `00_Species_selection_DFO_RVsurvey.Rmd` | **run first** — species selection from the DFO RV survey (PCA) |
| **00** | `00_EZ_get catches.Rmd` | NAFO catch / yield + gear effort time series |
| **00** | `00_EZ_digitise_seal_data.r` | seal abundance (logistic fit), `seal_gear_params.csv`, effort-with-seals |
| **00** | `00_EZ_SpeciesParams_extraction.R` | species parameters (a, b, w_max, w_mat …) |
| **00** | `00_EZ_PPMR_extraction.R` | predator–prey mass ratios (beta) |
| **00** | `00_EZ_Workflow Diagram.Rmd` | workflow overview diagram |
| **01** | `01_Calibration_Stage1_steady.Rmd` | **time-averaged 1978–85 steady state** → `params_stage1_steady.RDS` + steady-state diagnostics (size spectra, growth, biomass/yield fits, emergent diet vs Cook & Bundy, seal "catch" vs Bowen & Harrison) |
| **01** | `01_Calibration_Stage2_AUTO_fastOptim.R` | **factual time-series model (recommended)** → `params_stage2_AUTO.RDS`. Objective-driven: structural decisions fixed in `apply_structural()`, ecological criteria as penalties, only nuisance parameters optimised (`optimParallel`). Reproducible. |
| **01** | `01_Calibration_Stage2_timeseries.R` | hand-tuned factual model → `params_stage2_factual.RDS`. Kept as a **validation reference** for the AUTO model. |
| **02** | `02_Scenarios Through time.Rmd` | factual vs counterfactual (seals constant) over a **paired** randomised-web ensemble; recovery-robustness analysis + figures |
| **03** | `03_Indicators.Rmd` | community indicators (e.g. mean-weight) computed from the scenario runs |

Interaction-matrix prep: `Cook and Bundy.R` / `New_Species_Interaction_DepthStrata.R` (Schoener's D spatial overlap → θ_ij).

`Ess_data/Useful_functions.R` is the **single** project functions file (custom
biomass calibration, seal selectivity, and the `getError*` loss functions used by the
optimiser). All scripts `source()` it.

## Two-stage calibration

- **Stage 1** produces a time-averaged 1978–85 steady state (high groundfish, low
  pelagics) as the foundation — *structure only*, no time-series dynamics.
- **Stage 2** projects through the 1978–2024 historical fishing + seal forcing and
  tunes the few processes that reproduce the collapse + non-recovery. The **AUTO**
  script is the recommended, reproducible route; the hand-tuned script is the reference.

## The counterfactual (paper)

- **Factual:** fishing as observed + seal effort **increases** 1978→2024 (digitised logistic).
- **Counterfactual:** identical fishing + seal effort **held constant** at the 1978 level.
- The ensemble is **paired** (`run_paired_sims`): each randomised food web is run under
  *both* scenarios, so the per-web difference isolates the seal effect on identical
  structure. Seeded for reproducibility.

## Data

Large inputs/outputs are git-ignored (`Ess_data/*.RDS`, `Ess_Output/*`). Re-run the
**00** scripts to regenerate the data inputs, then **01** → **02**. Required hand-made
inputs (e.g. `Ess_data/seal_gear_params.csv`, survey CSVs) are produced by the 00 step.

## Requirements

R with `mizer`, `mizerExperimental`, `tidyverse`, `optimParallel`, `patchwork`,
`ggpubr`. Calibration and the scenario ensemble use parallel cores.
