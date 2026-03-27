library(tidyverse)
library(gt)
library(dplyr)

# # Species lookup table
# species_lookup_subset <- tibble::tribble(
#   ~CODE, ~SPECIES_NAME,
#   10,  "Atlantic Cod",
#   11,  "Haddock",
#   12,  "White Hake",
#   14,  "Silver Hake",
#   16,  "Pollock",
#   23,  "Redfishes Unseparated",
#   31,  "Greenland Halibut",
#   40,  "American Plaice",
#   41,  "Witch Flounder",
#   42,  "Yellowtail Flounder",
#   43,  "Winter Flounder",
#   60,  "Atlantic Herring",
#   61,  "Shad American",
#   64,  "Capelin",
#   70,  "Mackerel",
#   160, "Argentine",
#   200, "Barndoor Skate",
#   201, "Thorny Skate",
#   204, "Winter Skate",
#   220, "Spiny Dogfish",
#   300, "Longhorn Sculpin",
#   400, "Monkfish",
#   592, "Sharks (Unspecified)",
#   610, "Sand Lance",
#   701, "Butterfish",
#   2211, "Pandalus Borealis",
#   2522, NA,
#   4511, "Short-Fin Squid",
#   6600, "Holothuroidea",
#   6611, "Sea Cucumber",
#   30,  "Atlantic Halibut"
# )


biomass_length<- read.csv("Ess_data/long_df_for_PCA.csv")
# # Join species names into biomass_length
# biomass_length <- biomass_length %>%
#   left_join(species_lookup_subset, by = c("SPECIES" = "CODE"))

# Check it worked
# biomass_length %>%
#   select(Year, SPECIES, Species BIOMASS) %>%
#   head(20) %>%
#   as.data.frame()

write.csv(biomass_length, "Ess_data/biomass_Qadj_length_with_names.csv", row.names = FALSE)


# Read, join names, build length table — all in one pipeline
length_table <- read.csv("Ess_data/biomass_Qadj_length_with_names.csv") %>%
  #left_join(species_lookup_subset, by = c("SPECIES" = "CODE")) %>%
  filter(LENGTH != -99) %>%
  # mutate(length_class = cut(
  #   LENGTH,
  #   breaks = seq(0, max(LENGTH) + 10, by = 10),
  #   labels = paste0(seq(1, max(LENGTH) + 1, by = 10), "-",
  #                   seq(10, max(LENGTH) + 10, by = 10)),
  #   right = TRUE, include.lowest = TRUE
  # )) %>%
  mutate(log10length_class = cut(
    log10(LENGTH),
    breaks = seq(
      log10(min(LENGTH, na.rm = TRUE)),
      log10(max(LENGTH, na.rm = TRUE)),
      length.out = 4   # → 3 bins
    ),
    include.lowest = TRUE
  )) %>%
  group_by(log10length_class, species = Species) %>%
  summarise(biomass = sum(BIOMASS, na.rm = TRUE), .groups = "drop") %>%
  group_by(log10length_class) %>%
  mutate(
    total_biomass  = sum(biomass),
    pct_of_length  = biomass / total_biomass * 100
  ) %>%
  ungroup()

# Pivot wide
length_table_wide <- length_table %>%
  select(log10length_class, species, pct_of_length) %>%
  pivot_wider(names_from = species, values_from = pct_of_length, values_fill = 0) %>%
  arrange(log10length_class) %>%
  left_join(
    length_table %>%
      group_by(log10length_class) %>%
      summarise(Total_Biomass = mean(total_biomass), .groups = "drop"),
    by = "log10length_class"
  ) %>%
  mutate(Total_Biomass = scales::comma(round(Total_Biomass)))

# gt table
length_table_wide %>%
  gt(rowname_col = "log10length_class") %>%
  tab_header(
    title    = md("**Biomass Contribution (%) by Log10 Length Class**"),
    subtitle = "Each cell = species % of total biomass at that log10 length class | Final column = total biomass"
  ) %>%
  fmt_number(columns = where(is.numeric), decimals = 1) %>%
  data_color(columns = where(is.numeric), palette = "Blues") %>%
  tab_style(
    style     = list(cell_fill(color = "#2c3e50"), cell_text(color = "white", weight = "bold")),
    locations = cells_stub()
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#2c3e50"), cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels()
  ) %>%
  tab_options(table.font.size = 12, heading.align = "left") %>%
  gtsave("biomass_length_table.html")

  # cat("Log10 Length class:", len, "\n")
  # print(length_table %>%
  #         filter(log10length_class == levels(log10length_class)[len]) %>%
  #         arrange(desc(pct_of_length)) %>%
  #         select(species, pct_of_length) %>%
  #         head(5))
  # cat("\n")


log10lengthclasses <- levels(length_table$log10length_class)
#lengthclasses <- levels(length_table$log10length_class)

newdf<-filter(length_table_wide,log10length_class==log10lengthclasses[1]) %>%
  select(-Total_Biomass) %>%
  pivot_longer(cols = -log10length_class,names_to = "Species", values_to = "Pct") %>%
  filter(is.finite(Pct)) %>%
  arrange(desc(Pct))  



for(len in 2:30) {
new<-filter(length_table_wide,log10length_class==log10lengthclasses[len]) %>%
  select(-Total_Biomass) %>%
  pivot_longer(cols = -log10length_class,names_to = "Species", values_to = "Pct") %>%
  filter(is.finite(Pct)) %>%
  arrange(desc(Pct))
newdf<-rbind(newdf,new) 
}

new_length <- newdf|>
  group_by(log10length_class) |>
  mutate(Rank=row_number(), CumPct=cumsum(Pct)) |>
  filter(is.finite(Pct)) |>
  mutate(Species = str_to_sentence(str_squish(str_replace_all(Species, "\\.+", " "))))


#len<- lengthclasses[1]

len <- log10lengthclasses[1]

make_length_plot <- function(len, df=new_length, col="steelblue") {

  cut_rank <- new_length |> filter(log10length_class==len) |>
    summarise(min(Rank[CumPct >= 95])) |> pull()
  df <- new_length |> filter(log10length_class==len) |>
    mutate(Species=factor(Species, levels=rev(unique(Species))),
           Above95=Rank <= cut_rank)
  ypos <- as.numeric(df$Species[df$Rank==cut_rank][1]) - 0.5
  
  ggplot(df, aes(Pct, Species)) +
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=ypos, ymax=Inf, fill=col, alpha=0.06) +
    geom_col(aes(alpha=Above95), fill=col, colour="white", linewidth=0.2, width=0.72) +
    scale_alpha_manual(values=c("TRUE"=0.85,"FALSE"=0.45), guide="none") +
    geom_hline(yintercept=ypos, colour=col, linetype="dashed", linewidth=0.8) +
    annotate("label", x=Inf, y=ypos, label="95% cumulative biomass",
             colour=col, fill="white",
             hjust=1.1, vjust=-0.2, size=2.5, fontface="bold") +
    scale_x_continuous(labels=percent_format(accuracy=1),
                       expand=expansion(mult=c(0,.28))) +
    labs(title=paste0(c("Length class = "),len,c( " cm")), x=NULL, y=NULL) +
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


  make_length_plot(log10lengthclasses[1]) 
  make_length_plot(log10lengthclasses[2]) 
  make_length_plot(log10lengthclasses[3]) 



  # plot_layout(ncol=2) 
  # plot_annotation(title = "Species contribution to total biomass by log10 length class")



##### DIFFERENT APPROACH: SPECIES CONTRIBITION IN TERMS OF LMAX BIN, RATHER THAN LENGTH BIN (species in mizer are strongly defined by their aymptotic size)
# for the purpose of species selection excluding 2018 onwars due to logistics issus with survey; but we can use in other analyses comparisons with with model outputs
# have effort forcing up to 2024
species_80_table <- function(data=filter(biomass_length,Year >=1993 & Year <=2018),
                             Species = Species,
                              LENGTH = LENGTH,
                             BIOMASS = BIOMASS,
                             Lmax_bin_width = 20,
                             coverage = 0.8) {
  species_ranked <- data %>%
    group_by(Species) %>%
    summarise(
      Lmax = max(LENGTH, na.rm = TRUE),
      biomass = sum(BIOMASS, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Lmax_bin = cut(
        Lmax,
        breaks = seq(10,200,by=Lmax_bin_width,
          #breaks = seq(floor(log10(min(Lmax, na.rm = TRUE))), ceiling(log10(max(Lmax, na.rm = TRUE))), by = log_bin_width),
          include.lowest = TRUE
        ))
    ) %>%
    group_by(Lmax_bin) %>%
    arrange(desc(biomass), .by_group = TRUE) %>%
    mutate(
      prop_biomass = biomass / sum(biomass),
      cumprop_biomass = cumsum(prop_biomass),
      keep = cumprop_biomass <= coverage |
        lag(cumprop_biomass, default = 0) < coverage
    ) %>%
    ungroup()
  
  species_ranked %>%
    filter(keep,!is.na(Lmax_bin)) %>%
    group_by(Lmax_bin) %>%
    summarise(
      n_species = n(),
      biomass_captured = sum(prop_biomass),
      species = paste(Species, collapse = ", "),
      .groups = "drop"
    )
} 
  

out1<-species_80_table(data=filter(biomass_length,Year >=1978 & Year <= 1992),coverage = 0.8)
out2<-species_80_table(data=filter(biomass_length,Year >=1993 & Year <= 2018),coverage = 0.8)

####### GT TABLES: Above/Below Portrait - Individual Species Contributions by Lmax Bin

# Extract species contributions for PRE-MORATORIUM (top)
pre_species_detail <- out1 |>
  separate_rows(species, sep = ", ") |>
  mutate(Species = str_trim(str_to_sentence(species))) |>
  select(Lmax_bin, Species, biomass_captured) |>
  arrange(Lmax_bin, desc(biomass_captured))

# Extract species contributions for POST-MORATORIUM (bottom)
post_species_detail <- out2 |>
  separate_rows(species, sep = ", ") |>
  mutate(Species = str_trim(str_to_sentence(species))) |>
  select(Lmax_bin, Species, biomass_captured) |>
  arrange(Lmax_bin, desc(biomass_captured))

# Create PRE-MORATORIUM table (portrait orientation, top)
pre_table <- pre_species_detail |>
  pivot_wider(
    id_cols = Lmax_bin,
    names_from = Species,
    values_from = biomass_captured,
    values_fill = 0
  ) |>
  gt(rowname_col = "Lmax_bin") |>
  tab_header(
    title = "PRE-MORATORIUM (1978-1992)",
    subtitle = "Species contribution to 80% cumulative biomass by maximum length bin"
  ) |>
  data_color(
    columns = where(is.numeric),
    palette = "Blues",
    domain = c(0, max(pre_species_detail$biomass_captured, na.rm = TRUE)),
    na_color = "white"
  ) |>
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) |>
  tab_style(
    style = list(cell_fill(color = "#1f77b4"), cell_text(color = "white", weight = "bold")),
    locations = cells_stub()
  ) |>
  tab_options(
    table.font.size = 9,
    heading.align = "center",
    column_labels.border.bottom.width = px(2)
  )

# Create POST-MORATORIUM table (portrait orientation, bottom)
post_table <- post_species_detail |>
  pivot_wider(
    id_cols = Lmax_bin,
    names_from = Species,
    values_from = biomass_captured,
    values_fill = 0
  ) |>
  gt(rowname_col = "Lmax_bin") |>
  tab_header(
    title = "POST-MORATORIUM (1993+)",
    subtitle = "Species contribution to 80% cumulative biomass by maximum length bin"
  ) |>
  data_color(
    columns = where(is.numeric),
    palette = "Reds",
    domain = c(0, max(post_species_detail$biomass_captured, na.rm = TRUE)),
    na_color = "white"
  ) |>
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) |>
  tab_style(
    style = list(cell_fill(color = "#d62728"), cell_text(color = "white", weight = "bold")),
    locations = cells_stub()
  ) |>
  tab_options(
    table.font.size = 9,
    heading.align = "center",
    column_labels.border.bottom.width = px(2)
  )

# Display both tables
pre_table
post_table

####### determine stable period for calibration of steady state model;

# Extract all species that appear in the 80% coverage analysis (out1 and out2)
species_in_coverage <- unique(c(
  unlist(strsplit(out1$species, ", ")),
  unlist(strsplit(out2$species, ", "))
))

# Clean biomass_length data FIRST
biomass_length_clean <- biomass_length |>
  mutate(Species = str_replace(Species, "; LIMANDA", "")) |>
  mutate(Species = str_replace(Species, "REDFISH UNSEPARATED", "REDFISH")) |>
  mutate(Species = str_replace(Species, "NORTHERN SAND LANCE", "SAND LANCE"))

# Clean species names in coverage list
species_in_coverage_clean <- species_in_coverage |>
  str_replace("; LIMANDA", "") |>
  str_replace("REDFISH UNSEPARATED", "REDFISH") |>
  str_replace("NORTHERN SAND LANCE", "SAND LANCE")

# Create a lookup table of Lmax for each species (from CLEANED biomass_length data)
species_lmax <- biomass_length_clean |>
  group_by(Species) |>
  summarise(Lmax = max(LENGTH, na.rm = TRUE), .groups = "drop") |>
  filter(Species %in% species_in_coverage_clean) |>
  arrange(Lmax)

# Update species_biomass_standardized to use shortened names
species_biomass_standardized_clean <- species_biomass_standardized |>
  mutate(Species = str_replace(Species, "; LIMANDA", "")) |>
  mutate(Species = str_replace(Species, "REDFISH UNSEPARATED", "REDFISH")) |>
  mutate(Species = str_replace(Species, "NORTHERN SAND LANCE", "SAND LANCE"))

# Use species from 80% coverage analysis ordered by Lmax
species_biomass_standardized_clean |>
  filter(Species %in% species_lmax$Species) |>
  group_by(Year, Species) |>
  summarise(standardized_biomass = mean(standardized_biomass, na.rm = TRUE), .groups = "drop") |>
  mutate(Species = factor(Species, levels = species_lmax$Species)) |>
  ggplot(aes(x = Year, y = standardized_biomass)) +
  geom_line(linewidth = 1, color = "#2E86AB") +
  geom_point(size = 2, color = "#2E86AB", alpha = 0.7, fill = "white", shape = 21, stroke = 0.8) +
  geom_hline(yintercept = 1, linetype = "solid", color = "#A23B72", linewidth = 0.9, alpha = 0.6) +
  geom_vline(xintercept = 1991.5, linetype = "dashed", color = "#A23B72", linewidth = 0.8, alpha = 0.5) +
  facet_wrap(~Species, scales = "free_y", ncol = 5) +
  labs(
    title = "Standardized Biomass by Species",
    subtitle = "1978-2022 | Relative to Pre-Moratorium (1978-1992) Geometric Mean | Species ordered by increasing Lmax | Dashed line = moratorium (1992)",
    x = "Year",
    y = "Standardized Biomass"
  ) +
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020)) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 7, face = "bold", color = "#2c3e50"),
    strip.background = element_rect(fill = "#f5f5f5", color = "#d0d0d0", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(face = "bold", size = 12, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 10, color = "#666", margin = margin(b = 10)),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9, face = "bold"),
    panel.border = element_rect(color = "#d0d0d0", fill = NA, linewidth = 0.4)
  )

