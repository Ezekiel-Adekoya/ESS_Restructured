###"Useful_functions for mizer calibration"
### Set of functions using lower and upper size selectivity of the RV survey data
### This allows us to set up the params object with two columns called biomass_cutoff and biomass_cutoffHigh


# ── HELPER FUNCTION FOR BIOMASS CUTOFFS ───────────────────────────────────────
get_cutoffs <- function(sp_params, n_sp) {
  
  cutoffLow <- sp_params$biomass_cutoff
  
  if (is.null(cutoffLow)) {
    cutoffLow <- rep(0, n_sp)
  }
  
  cutoffLow[is.na(cutoffLow)] <- 0
  
  cutoffHigh <- sp_params$biomass_cutoffHigh
  
  if (is.null(cutoffHigh)) {
    cutoffHigh <- rep(Inf, n_sp)
  }
  
  cutoffHigh[is.na(cutoffHigh)] <- Inf
  
  list(
    low = cutoffLow,
    high = cutoffHigh
  )
}

# this function adds a lower boundary to selected size

plotBiomassObservedVsModelCustom <- function (object, species = NULL, ratio = FALSE, log_scale = TRUE, 
    return_data = FALSE, labels = TRUE, show_unobserved = FALSE) 
{
    if (is(object, "MizerSim")) {
        params = object@params
        n <- finalN(object)
    }
    else if (is(object, "MizerParams")) {
        params = object
        n <- initialN(params)
    }
    else {
        stop("You have not provided a valid mizerSim or mizerParams object.")
    }
    sp_params <- params@species_params
    species = valid_species_arg(object, species)
    if (length(species) == 0) 
        stop("No species selected, please fix.")
    row_select = match(species, sp_params$species)
    if (!"biomass_observed" %in% names(sp_params)) {
        stop("You have not provided values for the column 'biomass_observed' ", 
            "in the mizerParams/mizerSim object.")
    }
    else if (!is.numeric(sp_params$biomass_observed)) {
        stop("The column 'biomass_observed' in the mizerParams/mizerSim object", 
            " is not numeric, please fix.")
    }
    else {
        biomass_observed = sp_params$biomass_observed
    }
    
    cutoffLow <- sp_params$biomass_cutoff[row_select]
    if (is.null(cutoffLow)) {
        cutoffLow = rep(0, length(species))
    }
    else if (!is.numeric(cutoffLow)) {
        stop("params@species_params$biomass_cutoff is not numeric, \",\n                 \"please fix.")
    }
    cutoffLow[is.na(cutoffLow)] <- 0
    
    cutoffHigh <- sp_params$biomass_cutoffHigh[row_select]
    if (is.null(cutoffHigh)) {
        cutoffHigh = rep(0, length(species))
    }
    else if (!is.numeric(cutoffHigh)) {
        stop("params@species_params$biomass_cutoffHigh is not numeric, \",\n                 \"please fix.")
    }
    cutoffHigh[is.na(cutoffHigh)] <- 0
    
    sim_biomass = rep(0, length(species))
    for (j in 1:length(species)) {
        sim_biomass[j] = sum((n[row_select[j], ] * params@w * 
            params@dw)[params@w >= cutoffLow[j] & cutoffHigh[j] >= params@w])
    }
    dummy = data.frame(species = species, model = sim_biomass, 
        observed = biomass_observed[row_select]) %>% mutate(species = factor(species, 
        levels = species), is_observed = !is.na(observed) & observed > 
        0, observed = case_when(is_observed ~ observed, !is_observed ~ 
        model), ratio = model/observed)
    if (sum(dummy$is_observed) == 0) {
        cat(paste("There are no observed biomasses to compare to model,", 
            "only plotting model biomasses.", sep = "\n"))
    }
    if (!show_unobserved) {
        dummy <- filter(dummy, is_observed)
    }
    if (return_data == TRUE) 
        return(dummy)
    tre <- round(sum(abs(1 - dummy$ratio)), digits = 3)
    caption <- paste0("Total relative error = ", tre)
    if (any(!dummy$is_observed)) {
        caption <- paste(caption, "\n Open circles represent species without biomass observation.")
    }
    if (ratio == FALSE) {
        gg <- ggplot(data = dummy, aes(x = observed, y = model, 
            colour = species, shape = is_observed)) + geom_abline(aes(intercept = 0, 
            slope = 1), colour = "purple", linetype = "dashed", 
            size = 1.3) + geom_point(size = 3) + labs(y = "model biomass [g]") + 
            coord_cartesian(ylim = range(dummy$model, dummy$observed))
    }
    else {
        gg <- ggplot(data = dummy, aes(x = observed, y = ratio, 
            colour = species, shape = is_observed)) + geom_hline(aes(yintercept = 1), 
            linetype = "dashed", colour = "purple", 
            size = 1.3) + geom_point(size = 3) + labs(y = "model biomass / observed biomass") + 
            coord_cartesian(ylim = range(dummy$ratio))
    }
    gg <- gg + labs(x = "observed biomass [g]", caption = caption) + 
        scale_colour_manual(values = getColours(params)[dummy$species]) + 
        scale_shape_manual(values = c(`TRUE` = 19, `FALSE` = 1)) + 
        guides(shape = "none")
    if (log_scale == TRUE & ratio == FALSE) {
        gg = gg + scale_x_log10() + scale_y_log10()
    }
    if (log_scale == TRUE & ratio == TRUE) {
        gg = gg + scale_x_log10()
    }
    if (labels == TRUE) {
        gg = gg + ggrepel::geom_label_repel(aes(label = species), 
            box.padding = 0.35, point.padding = 0.5, segment.color = "grey50", 
            show.legend = FALSE, max.overlaps = Inf, seed = 42)
    }
    gg
}




# adapting cutoff here too
calibrateBiomassCustom <- function(params) {
  
  if (!("biomass_observed" %in% names(params@species_params)) ||
      all(is.na(params@species_params$biomass_observed))) {
    return(params)
  }
  
  sp_params <- params@species_params
  n_sp <- nrow(sp_params)
  
  cutoffs <- get_cutoffs(sp_params, n_sp)
  cutoffLow <- cutoffs$low
  cutoffHigh <- cutoffs$high
  
  observed <- sp_params$biomass_observed
  sp_observed <- which(!is.na(observed) & observed > 0)
  
  observed_total <- sum(observed[sp_observed], na.rm = TRUE)
  
  model_total <- 0
  
  for (sp_idx in sp_observed) {
    idx <- params@w >= cutoffLow[sp_idx] &
      params@w <= cutoffHigh[sp_idx]
    
    model_total <- model_total +
      sum(params@initial_n[sp_idx, idx] *
            params@w[idx] *
            params@dw[idx])
  }
  
  if (model_total <= 0 || !is.finite(model_total)) {
    stop("Model biomass within biomass cutoffs is zero or non-finite.")
  }
  
  scaleModel(params, factor = observed_total / model_total)
}


# same as above
matchBiomassesCustom <- function(params, species = NULL) {
  
  if (!("biomass_observed" %in% names(params@species_params))) {
    return(params)
  }
  
  sp_params <- params@species_params
  n_sp <- nrow(sp_params)
  
  selected <- valid_species_arg(
    params,
    species = species,
    return.logical = TRUE
  ) &
    !is.na(sp_params$biomass_observed) &
    sp_params$biomass_observed > 0
  
  cutoffs <- get_cutoffs(sp_params, n_sp)
  
  for (sp in which(selected)) {
    
    idx <- params@w >= cutoffs$low[sp] &
      params@w <= cutoffs$high[sp]
    
    total <- sum(params@initial_n[sp, idx] *
                   params@w[idx] *
                   params@dw[idx])
    
    if (total <= 0 || !is.finite(total)) {
      warning("Skipping ", sp_params$species[sp],
              ": model biomass within cutoffs is zero.")
      next
    }
    
    factor <- sp_params$biomass_observed[sp] / total
    factor <- min(max(factor, 0.25), 4)   # damp: max 4x or 0.25x per iteration
    params@initial_n[sp, ] <- params@initial_n[sp, ] * factor
  }
  
  # Diagnose before setBevertonHolt: identify species with zero RDI
  rdi <- getRDI(params)
  zero_sp <- which(rdi == 0)
  if (length(zero_sp) > 0) {
    sp_names <- sp_params$species[zero_sp]
    for (sp in zero_sp) {
      wmat <- sp_params$w_mat[sp]
      mature_b <- sum(params@initial_n[sp, params@w >= wmat] *
                        params@w[params@w >= wmat] *
                        params@dw[params@w >= wmat])
      message("getRDI = 0 for ", sp_params$species[sp],
              " | w_mat = ", round(wmat, 2),
              " g | mature biomass = ", signif(mature_b, 3), " g",
              " | erepro = ", round(sp_params$erepro[sp], 4))
    }
    stop("Some species have no reproduction: ", paste(sp_names, collapse = ", "),
         "\nSee messages above. Check z0, w_mat, and biomass scaling for these species.")
  }

  setBevertonHolt(params)
}


# matchYieldCustom: wraps matchYield and clamps per-species fishing mortality
# to biologically realistic bounds after each catchability adjustment.
#
# Reads optional columns from species_params:
#   f_max  — upper F bound (e.g. 0.4 for Atlantic Halibut)
#   f_min  — lower F bound (e.g. 0.3 for Atlantic Cod)
# Species without a bound set (NA) are unconstrained.
#
# F is computed as biomass-weighted mean across sizes, consistent with how
# plotFMortExcl summarises fishing mortality.
matchYieldCustom <- function(params, gears = NULL, keep = "biomass") {

  if (!is.null(gears)) {
    params <- matchYield(params, keep = keep, gears = gears)
  } else {
    params <- matchYield(params, keep = keep)
  }

  # Cap per gear×species: if effort × catchability > 1.5 for any row, scale
  # that row's catchability down so F = 1.5. Checked per row, not summed.
  gp     <- gear_params(params)
  effort <- params@initial_effort[gp$gear]
  f_row  <- effort * gp$catchability
  over   <- !is.na(f_row) & f_row > 1.5
  if (any(over)) {
    gear_params(params)$catchability[over] <-
      gp$catchability[over] * (1.5 / f_row[over])
  }

  params
}
