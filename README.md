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

Run in order. **`00a–00e` = data prep**, then **`01–04` = calibration → scenarios → indicators**.

| Step | Script | Produces |
|---|---|---|
| **00a** | `00a_Species_selection_DFO_RVsurvey.Rmd` | species selection from the DFO RV survey (PCA) |
| **00b** | `00b_SpeciesParams_extraction.R` | species parameters (a, b, w_max, w_mat …) |
| **00c** | `00c_PPMR_extraction.R` | predator–prey mass ratios (beta) |
| **00d** | `00d_get_catches.Rmd` | NAFO catch / yield + gear effort time series |
| **00e** | `00e_digitise_seal_data.r` | seal abundance (logistic fit), `seal_gear_params.csv`, effort-with-seals |
| **01** | `01_Calibration_Stage1_steady.Rmd` | **time-averaged 1978–85 steady state** → `params_stage1_steady.RDS` + steady-state diagnostics (size spectra, growth, biomass/yield fits, emergent diet vs Cook & Bundy, seal "catch" vs Bowen & Harrison) |
| **02** | `02_Calibration_Stage2_AUTO.R` | **factual time-series model** → `params_stage2_AUTO.RDS`. Objective-driven: structural decisions fixed in `apply_structural()`, ecological criteria as penalties, only nuisance parameters optimised (`optimParallel`). Reproducible. |
| **03** | `03_Scenarios_Through_time.Rmd` | factual vs counterfactual (seals constant) over a **paired** randomised-web ensemble; recovery-robustness analysis + figures. Reads the cached ensemble (`Ess_Output/sims_{factual,counter}.RDS`) — generate it first with `run_ensemble_sims.R` (see below). |
| **04** | `04_Indicators.Rmd` | community indicators (e.g. mean-weight) computed from the scenario runs |

### Supplementary files (not numbered steps)
- `run_ensemble_sims.R` — generates the **n=1000 paired randomised-web ensemble** (the expensive step, ~15 min) → `Ess_Output/sims_{factual,counter}.RDS`. Run once (RStudio or `Rscript run_ensemble_sims.R`) whenever the Stage-2 `params` change; **03** then knits in seconds from the cache. Seed-locked, so results are identical to an in-Rmd run. (In 03, set `force_resim <- TRUE` to rebuild the cache instead.)
- `Calibration_Stage2_manual_reference.R` — the hand-tuned factual model (`params_stage2_factual.RDS`); kept as a **validation reference** for the AUTO model (step 02).
- `Workflow_Diagram.Rmd` — workflow overview diagram.
- `Cook and Bundy.R` / `New_Species_Interaction_DepthStrata.R` — interaction-matrix prep (Schoener's D spatial overlap → θ_ij).

`Ess_data/Useful_functions.R` is the **single** project functions file (custom
biomass calibration, seal selectivity, the `getError*` loss functions used by the
optimiser, and the paired-ensemble helpers `randomize_interaction_matrix` /
`run_paired_sims`). All scripts `source()` it.

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

Built and run with **R 4.5.2** and the following key packages:

| Package | Version |
|---|---|
| `mizer` | **2.5.3** |
| `mizerExperimental` | **2.5.3.9000** (development build from GitHub: `sizespectrum/mizerExperimental`) |

`mizerExperimental` is a development version (the `.9000` suffix), so install it from GitHub
to match, e.g. `remotes::install_github("sizespectrum/mizerExperimental")`; `mizer` 2.5.3 is on
CRAN. Also requires `tidyverse`, `optimParallel`, `patchwork`, and `ggpubr`. Calibration and
the scenario ensemble use parallel cores.
