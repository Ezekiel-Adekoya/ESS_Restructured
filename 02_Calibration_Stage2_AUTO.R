## ============================================================================
## 02_Calibration_Stage2_AUTO.R   (STEERED, objective-driven Stage 2)
##
## Reproducible alternative to the hand-tuned Stage 2. ALL decisions live in one
## of three labelled places, each traceable to a STUDY OBJECTIVE — no ad-hoc
## hand-tuning of the continuous parameters:
##
##   (1) STRUCTURAL  (apply_structural(), FIXED) — the hypotheses / ecology:
##         - cultivation: forage -> cod recruits         (non-recovery hypothesis)
##         - logistic seal forcing + F_peak ceiling      (data + predator satiation)
##         - demersal seal predation                     (grey seals are benthic foragers)
##         - monkfish cannibalism                        (life history)
##         - finite R_max cap for the BISTABLE stocks    (stability requirement)
##           (Pollock, Red Hake) — choosing WHICH is a structural call
##
##   (2) CRITERIA  (penalties in the loss) — what a valid "factual" model must satisfy:
##         - forage must persist & rise (herring high in the non-recovery era)
##         - no stock explodes (max <= k * max observed)
##         - no stock implausibly collapses (min >= frac * min observed)
##         - biomass trajectory fit (weighted SS log-residual, cod weighted high)
##
##   (3) CONTINUOUS  (optimised) — nuisance params, no scientific claim:
##         - fishing catchability multiplier per fished species  (-> the collapse)
##         - F_peak_seal (seal predation ceiling)                (-> non-recovery strength)
##
## Optimiser is WARM-STARTED at the hand-tuned solution and only POLISHES within
## that basin (the penalties keep it out of the Pollock-explode / herring-crash
## basins the naive optimiser fell into). Reproducible: same inputs -> same output.
## ============================================================================

suppressMessages({
  library(mizer); library(mizerExperimental); library(dplyr)
  library(tidyr); library(tibble); library(ggplot2); library(optimParallel)
})
source("Ess_data/Useful_functions.R")

MAXIT <- 60   # optimiser iterations (warm start is near-optimal, so few needed)

## ── Inputs ───────────────────────────────────────────────────────────────────
p0  <- readRDS("Ess_data/params_stage1_steady.RDS")
sp  <- p0@species_params$species
eff <- read.csv("Ess_data/effort_gear_with_seals.csv")
gear_cols  <- c("Bottom_Trawl","Fixed_Gear","Small_Mesh","Pelagic","Seals")
fish_gears <- c("Bottom_Trawl","Fixed_Gear","Small_Mesh","Pelagic")
effort <- as.matrix(eff[eff$Year %in% 1978:2024, gear_cols])
rownames(effort) <- eff$Year[eff$Year %in% 1978:2024]
dat_ts <- readRDS("Ess_data/biomass_ts.RDS")

## ============================================================================
## (1) STRUCTURAL DECISIONS — FIXED, from study objectives (no seal-F rescale yet;
##     that depends on the tuned F_peak so it is done per-eval in the loss).
## ============================================================================
forage <- c("Atlantic Herring","Sand Lance","Capelin")
# Relative seal selectivity on demersals (grey seals are benthic foragers; the
# Bowen 1994 snapshot under-represents benthic prey). Multipliers on the empirical
# seal_gear_params catchability; cod is the binding non-recovery lever.
seal_rel <- c("Atlantic Cod"=3, "American Plaice"=14, "Yellowtail Flounder"=14,
              "Witch Flounder"=8, "Longhorn Sculpin"=40, "Monkfish"=15)

# Cultivation strengths (forage -> predator-recruit predation): WHICH species are
# cultivated is the structural/objective decision (cod = non-recovery; gadoids &
# benthics damp their overshoots). Values are the documented Stage-1/2 set.
cult_prey <- c("Atlantic Cod"=0.9, "Haddock"=0.8, "White Hake"=0.4,
               "Silver Hake"=0.33, "Pollock"=0.15, "Redfish Unseparated"=0.25,
               "American Plaice"=0.25, "Yellowtail Flounder"=0.25,
               "Witch Flounder"=0.25, "Red Hake"=0.25, "Monkfish"=0.25)
FORAGE_RMAX_MULT <- 8   # forage productivity so released forage reaches realistic levels

apply_structural <- function(p) {
  inter <- p@interaction
  for (s in names(cult_prey)) inter[forage, s] <- cult_prey[s]  # cultivation
  inter["Monkfish","Monkfish"] <- 0.6                           # cannibalism
  p <- setInteraction(p, inter)
  # finite R_max caps for the BISTABLE / runaway stocks (structural stability call)
  p@species_params$R_max[sp=="Pollock"]  <- getRDI(p)["Pollock"]  * 0.006  # tight: bound the bistable high branch near obs
  p@species_params$R_max[sp=="Red Hake"] <- getRDI(p)["Red Hake"] * 0.30
  # Atlantic Halibut: unbounded APEX predator — fishing is otherwise the only control,
  # so it explodes (x213) & wipes the demersal community in no-fishing runs. Strong
  # density-dependent cap so it is self-limiting (needed for valid counterfactuals).
  p@species_params$R_max[sp=="Atlantic Halibut"] <- getRDI(p)["Atlantic Halibut"] * 1.0
  p <- setParams(p)
  # forage productivity boost (so released forage can reach realistic biomass)
  for (s in forage) p@species_params$R_max[sp==s] <- p@species_params$R_max[sp==s]*FORAGE_RMAX_MULT
  p <- setParams(p)
  # demersal + cod seal selectivity (relative; absolute scale set by F_peak below)
  gp <- gear_params(p)
  for (s in names(seal_rel)) { ix <- gp$species==s & gp$gear=="Seals"
    gp$catchability[ix] <- gp$catchability[ix]*seal_rel[s] }
  gear_params(p) <- gp
  p
}
p_base <- apply_structural(p0)

# Seal catchability rescale so realized PEAK F (2024 logistic top) = F_peak (keeps
# the logistic shape; cod is the binding species).
set_seal_Fpeak <- function(p, F_peak) {
  emax <- max(effort[,"Seals"])
  per  <- max(getFMortGear(p)["Seals",,]) / initial_effort(p)["Seals"]
  gp <- gear_params(p)
  gp$catchability[gp$gear=="Seals"] <- gp$catchability[gp$gear=="Seals"] *
    as.numeric(F_peak/(per*emax))
  gear_params(p) <- gp; p
}

## ============================================================================
## (3) CONTINUOUS params + (2) CRITERIA loss
## ============================================================================
tune_fish <- c("Atlantic Cod","Haddock","Pollock","White Hake","Silver Hake",
               "Monkfish","Thorny Skate","Winter Skate")
# loss weights: cod high (the mechanism), herring high (must not be sacrificed),
# forage-undersampled low (level is catchability-confounded).
wts <- setNames(rep(1, length(sp)), sp)
wts[c("Atlantic Cod","Atlantic Herring")] <- c(6, 3)
wts[c("Sand Lance","Capelin")] <- c(0.2, 0.5)

project_bio <- function(p) {
  sim <- project(p, effort=effort, t_max=nrow(effort), t_save=1, progress_bar=FALSE)
  cl <- p@species_params$biomass_cutoff; cl[is.na(cl)] <- 0
  st <- as.integer(dimnames(sim@n)[[1]])
  mb <- sapply(seq_along(sp), function(j)
    rowSums(sweep(sim@n[,j,p@w>=cl[j],drop=FALSE],3,(p@w*p@dw)[p@w>=cl[j]],"*")))
  colnames(mb) <- sp; rownames(mb) <- st; mb/1e9      # kt
}

# observed summaries for the penalty criteria
obs_max  <- sapply(sp, function(s){v<-dat_ts$Biomass[dat_ts$Species==s]/1e9; if(length(v)) max(v,na.rm=TRUE) else NA})
obs_min  <- sapply(sp, function(s){v<-dat_ts$Biomass[dat_ts$Species==s & dat_ts$Biomass>0]/1e9; if(length(v)) min(v,na.rm=TRUE) else NA})

getErrorSteered <- function(vary, return_params=FALSE) {
  n <- length(tune_fish)
  fm <- 10^vary[1:n]; Fpeak <- 10^vary[n+1]
  if (any(!is.finite(c(fm,Fpeak))) || Fpeak<0.5 || Fpeak>12) return(1e9)
  p <- p_base
  gp <- gear_params(p)
  for (i in seq_len(n)) { ix <- gp$species==tune_fish[i] & gp$gear %in% fish_gears
    gp$catchability[ix] <- gp$catchability[ix]*fm[i] }
  gear_params(p) <- gp
  p <- tryCatch(set_seal_Fpeak(setParams(p), Fpeak), error=function(e) NULL)
  if (is.null(p)) return(1e9)
  if (return_params) return(p)
  mb <- tryCatch(project_bio(p), error=function(e) NULL); if (is.null(mb)) return(1e9)
  st <- as.integer(rownames(mb))

  ## (2a) weighted SS log-residual (trajectory fit)
  loss <- 0
  for (s in sp) {
    o <- dat_ts$Biomass[dat_ts$Species==s]/1e9; y <- dat_ts$Year[dat_ts$Species==s]
    cm <- intersect(st,y); if(!length(cm)) next
    os <- o[match(cm,y)]; ms <- mb[as.character(cm),s]
    ok <- !is.na(os)&os>0&!is.na(ms)&ms>0; if(!sum(ok)) next
    loss <- loss + wts[s]*sum((log(ms[ok]/os[ok]))^2)
  }
  ## (2b) ECOLOGICAL CRITERIA penalties (study objectives)
  pen <- 0; LAM <- 50
  # forage must be RELEASED & sustained in the non-recovery era (cultivation needs it)
  hr <- mb[as.character(intersect(st,2010:2024)),"Atlantic Herring"]
  if (length(hr)) pen <- pen + LAM*max(0, log(1200/mean(hr)))^2
  # no stock explodes (max <= 4x max observed); no implausible collapse (min >= 0.1x min obs)
  for (s in sp) {
    if (!is.na(obs_max[s])) pen <- pen + LAM*max(0, log(max(mb[,s])/(4*obs_max[s])))^2
    if (!is.na(obs_min[s])) pen <- pen + LAM*max(0, log((0.1*obs_min[s])/min(mb[,s])))^2
  }
  loss + pen
}

## ── WARM START at the hand-tuned solution ─────────────────────────────────────
warm_fish <- c("Atlantic Cod"=4,"Haddock"=1.8,"Pollock"=4,"White Hake"=3.5,
               "Silver Hake"=0.5,"Monkfish"=10,"Thorny Skate"=25,"Winter Skate"=20)
par0  <- c(log10(warm_fish[tune_fish]), log10(4))      # + F_peak = 4
lower <- c(log10(rep(0.3,length(tune_fish))), log10(1.5))
upper <- c(log10(rep(30, length(tune_fish))), log10(8))

cat("Loss at warm start (manual solution):", round(getErrorSteered(par0),1), "\n")

## ── fastOptim: parallel L-BFGS-B, polishing the warm start ────────────────────
ncores <- max(1, min(parallel::detectCores()-1, 2*length(par0)))
cl <- makeCluster(ncores)
clusterEvalQ(cl, { suppressMessages({library(mizer);library(mizerExperimental);library(dplyr)})
                   source("Ess_data/Useful_functions.R") })
clusterExport(cl, c("p_base","effort","dat_ts","sp","tune_fish","fish_gears",
                    "wts","obs_max","obs_min","set_seal_Fpeak","project_bio"))
setDefaultCluster(cl)
t0 <- Sys.time()
fit <- optimParallel(par=par0, fn=getErrorSteered, lower=lower, upper=upper,
                     control=list(maxit=MAXIT, factr=1e9),
                     parallel=list(forward=TRUE, loginfo=FALSE))
stopCluster(cl)
cat(sprintf("Optim %.1f min | loss %.1f -> %.1f\n",
            as.numeric(Sys.time()-t0,units="mins"), getErrorSteered(par0), fit$value))

## ── Apply, report, save, plot ─────────────────────────────────────────────────
pf <- getErrorSteered(fit$par, return_params=TRUE)
saveRDS(pf, "Ess_data/params_stage2_AUTO.RDS")
mb <- project_bio(pf)
o24 <- sapply(sp, function(s){v<-dat_ts$Biomass[dat_ts$Species==s & dat_ts$Year==2024]/1e9; if(length(v)) v[1] else NA})
cat("\nSteered-AUTO 2024 vs obs:\n")
print(data.frame(species=sp, model=round(mb[as.character(2024),],1), obs=round(o24,1)), row.names=FALSE)
cat(sprintf("\nTuned F_peak_seal = %.2f | fishing mults:\n", 10^fit$par[length(fit$par)]))
print(round(setNames(10^fit$par[1:length(tune_fish)], tune_fish),2))

long <- as.data.frame(mb) |> rownames_to_column("Year") |> mutate(Year=as.integer(Year)) |>
  pivot_longer(-Year, names_to="Species", values_to="B")
mk <- function(logsc){ g <- ggplot(long, aes(Year,B)) +
    geom_line(colour="firebrick", linewidth=0.7) +
    geom_point(data=filter(dat_ts, Biomass>0), aes(Year, Biomass/1e9), colour="black", size=0.6, inherit.aes=FALSE) +
    facet_wrap(~Species, scales="free_y") + theme_bw(base_size=9) +
    labs(subtitle="black = DFO survey | red = steered-AUTO model")
  if(logsc) g+scale_y_log10()+labs(title="Stage 2 steered-AUTO (log10)", y="Biomass (kt, log10)")
  else g+labs(title="Stage 2 steered-AUTO (linear)", y="Biomass (kt)") }
ggsave("Ess_Output/stage2_AUTOsteered_trajectories.jpeg",       mk(TRUE),  width=14, height=10, dpi=200)
ggsave("Ess_Output/stage2_AUTOsteered_trajectories_linear.jpeg",mk(FALSE), width=14, height=10, dpi=200)
cat("\nSaved params_stage2_AUTO.RDS + steered-AUTO plots\n")

## ── YIELD-vs-time check: modelled fishing yield vs observed NAFO catch ────────
## Confirms the catch fit through time is intact. Sums over the fishing gears only
## (excludes the Seals gear, which is predation, not catch).
yield_ts <- readRDS("Ess_data/yield_ts.RDS")            # observed catch (g): Year, Species, Yield
sim_y <- project(pf, effort=effort, t_max=nrow(effort), t_save=1, progress_bar=FALSE)
yg    <- getYieldGear(sim_y)                             # [time, gear, species] (g/yr)
fyld  <- apply(yg[, dimnames(yg)[[2]] %in% fish_gears, , drop=FALSE], c(1,3), sum)  # fishing gears only
ylong <- as.data.frame(fyld/1e9) |> rownames_to_column("Year") |> mutate(Year=as.integer(Year)) |>
  pivot_longer(-Year, names_to="Species", values_to="Yield")
gy <- ggplot(ylong, aes(Year, Yield)) +
  geom_line(colour="firebrick", linewidth=0.7) +
  geom_point(data=filter(yield_ts, Yield>0), aes(Year, Yield/1e9), colour="black", size=0.6, inherit.aes=FALSE) +
  facet_wrap(~Species, scales="free_y") + theme_bw(base_size=9) +
  labs(title="Stage 2 steered-AUTO: fishing yield (red) vs observed NAFO catch (black)",
       x="Year", y="Yield (kt)")
ggsave("Ess_Output/stage2_AUTOsteered_yield.jpeg", gy, width=14, height=10, dpi=200)
cat("Saved stage2_AUTOsteered_yield.jpeg\n")

## ============================================================================
## STANDING POST-HOC DIAGNOSTICS (run on the final factual params)
## ============================================================================
sim <- project(pf, effort=effort, t_max=nrow(effort), t_save=1, progress_bar=FALSE)
cl  <- pf@species_params$biomass_cutoff; cl[is.na(cl)] <- 0
nt  <- dim(sim@n)[1]
bio <- function(n) sapply(seq_along(sp), function(j) sum(n[j,p_w_keep[[j]]]*ww_keep[[j]]))/1e9
p_w_keep <- lapply(seq_along(sp), function(j) which(pf@w>=cl[j]))
ww_keep  <- lapply(seq_along(sp), function(j) (pf@w*pf@dw)[p_w_keep[[j]]])

cat("\n=== DIAGNOSTIC 1: feeding level (flag if saturated >0.95 or starving <0.1) ===\n")
flm <- rowMeans(getFeedingLevel(pf)); bad <- flm>0.95 | flm<0.1
cat("range", paste(round(range(flm),2),collapse="-"),
    "| flagged:", ifelse(any(bad),paste(sp[bad],collapse=", "),"none"),"\n")

cat("\n=== DIAGNOSTIC 2: extinct (<0.01 kt) / exploded at 2024 ===\n")
b24 <- bio(sim@n[nt,,])
cat("extinct:", ifelse(any(b24<0.01),paste(sp[b24<0.01],collapse=", "),"none"),
    "| largest:", sp[which.max(b24)], round(max(b24)),"kt\n")

## DIAGNOSTIC 3 — COUNTERFACTUAL sanity check (the paper's ONE counterfactual).
## FACTUAL = seal effort increases 1978->2024; COUNTERFACTUAL = seal effort HELD
## CONSTANT at the 1978 level (no increase); fishing identical in both. Confirms the
## factual params carry the mechanism: cod & groundfish RECOVER more when seals do
## not increase. (The full randomised-web ensemble lives in 02_Scenarios.)
eff_cf <- effort; eff_cf[,"Seals"] <- effort[1,"Seals"]        # seals constant at 1978
simCF  <- project(pf, effort=eff_cf, t_max=nrow(effort), t_save=1, progress_bar=FALSE)
bF <- bio(sim@n[nt,,]); bC <- bio(simCF@n[dim(simCF@n)[1],,])
gf <- c("Atlantic Cod","Haddock","Pollock","White Hake","Silver Hake")
cat("\n=== DIAGNOSTIC 3: counterfactual (seals constant 1978) vs factual @2024 ===\n")
print(data.frame(species=sp, factual=round(bF,1), counterfactual=round(bC,1),
                 cf_over_factual=round(bC/pmax(bF,0.01),2))[match(gf,sp),], row.names=FALSE)
cat("Cod recovers more WITHOUT the seal increase?",
    ifelse(bC[sp=="Atlantic Cod"] > bF[sp=="Atlantic Cod"],
           "YES — mechanism present","NO — check mechanism"),"\n")
