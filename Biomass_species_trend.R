library(tidyverse)

# 1. Load and clean data
biomass_aj <- read.csv("Alida_cleaned_Qaj_Biomass.csv", check.names = FALSE) %>%
  #select(-any_of("NA")) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(Year)) %>%
  { if ("X" %in% names(.)) select(., -X) else . } %>%
  select(Year, where(~!all(is.na(.x)))) %>%
  select(-matches("^NA\\."))

# 2. Compute total biomass per year and build long-format data
year_totals <- biomass_aj %>%
  mutate(across(-Year, ~replace_na(.x, 0))) %>%
  mutate(total = rowSums(across(-Year))) %>%
  select(Year, total)

# Keep species names as-is (no title case, matching Excel look)
df_long <- biomass_aj %>%
  pivot_longer(-Year, names_to = "species", values_to = "biomass") %>%
  mutate(biomass = replace_na(biomass, 0)) %>%
  left_join(year_totals, by = "Year") %>%
  #filter(total > 0) %>%
  mutate(
    species = factor(
      str_replace_all(species, "\\.", " "),
      levels = str_replace_all(names(biomass_aj)[-1], "\\.", " ")  # column order
    )) %>%
  filter(is.finite(biomass))

# Plot
ggplot(df_long, aes(x = Year, y = biomass, fill = species)) +
  geom_area(position = "fill", alpha = 0.9, colour = "white", linewidth = 0.15) +
  scale_x_continuous(breaks = seq(1970, 2022, 5),limits = c(1970, 2022), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual( values = colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(n_distinct(df_long$species))
  ) +
  labs(title = "Percentage of Total Annual Biomass", x = "Year", y = NULL, fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title        = element_text(face = "bold", hjust = 0.5, size = 13),
    axis.text.x       = element_text(size = 10),
    axis.text.y       = element_text(size = 10),
    legend.position   = "bottom",
    legend.key.size   = unit(0.35, "cm"),
    legend.text       = element_text(size = 8),
    legend.spacing.x  = unit(0.2, "cm"),
    panel.grid.minor  = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin       = margin(10, 15, 10, 10)
  ) +
  guides(fill = guide_legend(nrow = 8, byrow = TRUE))
ggsave("plot.jpeg", plot = p, width = 12, height = 8, dpi = 300)

