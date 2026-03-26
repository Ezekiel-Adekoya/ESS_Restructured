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

make_length_plot <- function(len,df=new_length) {

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
    annotate("label", x=Inf, y=ypos, label="80% cumulative biomass",
             colour=col, fill="white", label.size=0,
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

species_80_table <- function(data=filter(biomass_length,Year >=1993 ),
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
  

out1<-species_80_table(data=filter(biomass_length,Year >=1978 & Year <=1992),coverage = 0.8)
out2<-species_80_table(data=filter(biomass_length,Year >=1993),coverage = 0.8)

####### determine stable period for calibration of steady state model;

species_biomass_standardized |>
  ggplot(aes(x = Year, y = standardized_biomass, color = Species)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5, alpha = 0.6) +
  facet_wrap(~Species, scales = "free_y", ncol = 4) +
  geom_segment(aes(x = 1978, xend = 1992, y = 1, yend = 1), 
               color = "red", linewidth = 0.6, alpha = 0.7, inherit.aes = FALSE) +
  geom_vline(xintercept = 1991.5, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  labs(
    title = "Standardized Biomass by Species (Relative to Pre-Moratorium Geometric Mean)",
    subtitle = "Red line = pre-moratorium baseline | Dashed line = moratorium (1992)",
    x = "Year",
    y = "Standardized Biomass",
    color = "Species"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text.x = element_text(size = 9, face = "bold")
  )


### USE GEOEMTRIC MEAN TO CALIBRATE STEADY STATE MODEL, RATHER THAN ARITHMETIC MEAN, BECAUSE IT IS LESS INFLUENCED BY OUTLIERS (e.g. 1990-1992)