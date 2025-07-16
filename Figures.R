#Figures for NRB review paper----


#Ingestion plots----

# Studies that measured effects over time----

#Final combined fig
# Prepare data
ELM_plot_data <- ELM_summary_GLOVE_joined %>%
  filter(!is.na(Effect_general)) %>%
  mutate(
    Effect_general = factor(
      Effect_general,
      levels = c("Exposure only", "Sublethal", "Lethal")
    )
  )

write.csv(ELM_plot_data, "ELM_GLOVE_combined_ingest_data.csv")

# Color mapping
effect_colors <- c(
  "Exposure only" = "darkblue",
  "Sublethal" = "darkgoldenrod",
  "Lethal" = "darkred"
)

# Plot A (absolute counts) — with legend
plot_A <- ggplot(ELM_plot_data, aes(x = Decade, fill = Effect_general)) +
  geom_bar(position = "stack") +
  facet_wrap(~ Taxa, scales = "free_y") +
  scale_fill_manual(values = effect_colors) +
  labs(x = "Decade", y = "Number of Reports", fill = "Effect measured") +
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    plot.title = element_blank()
  )

# Plot B (proportions) — no legend
plot_B <- ggplot(ELM_plot_data, aes(x = Decade, fill = Effect_general)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Taxa, scales = "free_y") +
  scale_fill_manual(values = effect_colors, guide = "none") +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Decade", y = "Proportion of Reports") +
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_blank()
  )

# Combine with ggarrange
combined_plot <- ggarrange(
  plot_A, plot_B,
  nrow = 2,
  labels = c("A", "B"),
  font.label = list(size = 20, face = "bold"),
  common.legend = TRUE,
  legend = "bottom",
  align = "v"
)
final_plot <- annotate_figure(
  combined_plot,
  top = text_grob("Ingestion effects", color = "black", face = "bold", size = 22)
)

final_plot

# Save to PDF
ggsave("Plastic_Ingestion_Effects_final_nolabels.pdf", final_plot, width = 10, height = 10)







#Map figures ----
Rob_map <- ggplot(data = world) +
  geom_sf(fill = "aliceblue", color = "gray50", size = 0.2) +
  geom_sf(
    data = points_sf,
    aes(color = Effect_general, shape = Taxa),
    size = 3, alpha = 0.8
  ) +
  coord_sf(
    crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  ) +
  scale_color_manual(
    name = "Effect Type",
    values = c(
      "Exposure only" = "darkblue",
      "Sublethal" = "darkgoldenrod",
      "Lethal" = "darkred"
    )
  ) +
  scale_shape_manual(
    name = "Taxa",
    values = c(
      "Bird" = 16,             # circle
      "Fish" = 17,             # triangle
      "Mammal" = 15,           # square
      "Reptile and Amphibian" = 18  # diamond
    )
  ) +
  theme_minimal() +
  labs(
    title = "Meso- and Macroplastic Ingestion Study Locations by Effect Type and Taxa",
    x = NULL, y = NULL
  ) +
  theme(
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 4, alpha = 1)),
    shape = guide_legend(override.aes = list(size = 4, alpha = 1))
  )

Rob_map

ggsave("Plastic_Ingestion_Effects_map_with_shape.pdf", Rob_map, width = 14, height = 8)




#Map figures for ingestion w marginal density plots ----

# Clean Lat/Lon and filter out NAs
df_points <- ELM_plot_data %>%
  mutate(
    Lat = as.numeric(Lat),
    Lon = as.numeric(Lon)
  ) %>%
  filter(!is.na(Lat), !is.na(Lon))

# Title text
map_title <- "Ingestion Study Locations by Effect Type and Taxa"

# Add title before ggMarginal
main_plot <- ggplot(data = world) +
  geom_sf(fill = "aliceblue", color = "gray50", size = 0.2) +
  geom_point(
    data = df_points,
    aes(x = Lon, y = Lat, color = Effect_general, shape = Taxa),
    size = 2.5, alpha = 0.8
  ) +
  scale_color_manual(
    name = "Effect Type",
    values = c(
      "Exposure only" = "darkblue",
      "Sublethal" = "darkgoldenrod",
      "Lethal" = "darkred"
    ),
    limits = c("Exposure only", "Sublethal", "Lethal")
  ) +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90)) +
  labs(x = NULL, y = NULL, title = map_title) +  # Title goes here
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = NA, color = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 18),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1)))

# Add marginal densities
map_with_densities <- ggMarginal(
  main_plot,
  type = "density",
  margins = "both",
  size = 8,
  alpha = 0.2,
  groupColour = TRUE,
  groupFill = TRUE
)

# Show final plot
map_with_densities


ggsave("Plastic_Ingestion_Effects_map_with_shape_title_MargDens.pdf", map_with_densities, width = 14, height = 12)







#Entanglement plots----

entangle_filtered <- ELM_entangle_data %>%
  filter(Taxa %in% c("Bird", "Mammal", "Reptile and Amphibian"))

# Plot A — no legend

plot_A <- ggplot(entangle_filtered, aes(x = Decade, fill = Effect_general)) +
  geom_bar(position = "stack") +
  facet_wrap(~ Taxa, scales = "free_y") +
  scale_fill_manual(
    values = effect_colors,
    name = "Effect type"  # Set legend title here
  ) +
  labs(x = "Decade", y = "Number of Reports") +
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  # Hide legend for this plot
  )

# Plot B — with legend
plot_B <- ggplot(entangle_filtered, aes(x = Decade, fill = Effect_general)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Taxa, scales = "free_y") +
  scale_fill_manual(
    values = effect_colors,
    name = "Effect type"  # Set legend title here
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Decade", y = "Proportion of Reports") +
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Combine with ggarrange
combined_plot <- ggarrange(
  plot_A, plot_B,
  nrow = 2,
  labels = c("A", "B"),
  font.label = list(size = 20, face = "bold"),
  common.legend = TRUE,
  legend = "bottom",
  align = "v"
)
final_plot <- annotate_figure(
  combined_plot,
  top = text_grob("Entanglement effects", color = "black", face = "bold", size = 22)
)

final_plot


# Save to PDF
ggsave("Plastic_Entanglement_Effects_final_nolabels.pdf", final_plot, width = 10, height = 10)





#Map figures for entanglement w marginal density plots ----

entangle_summ_filtered <- ELM_entangle_summary_df %>%
  filter(Taxa %in% c("Bird", "Mammal", "Reptile and Amphibian"))

write.csv(ELM_entangle_summary_df, "ELM_entangle_summary_data.csv")

map_title <- "Entanglement Study Locations by Effect Type and Taxa"


# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Clean Lat/Lon and filter out NAs
df_points <- entangle_summ_filtered %>%
  mutate(
    Lat = as.numeric(Lat),
    Lon = as.numeric(Lon)
  ) %>%
  filter(!is.na(Lat), !is.na(Lon))

# Set the factor order
df_points$Effect_general <- factor(
  df_points$Effect_general,
  levels = c("Exposure only", "Sublethal", "Lethal")
)

# Base map plot
main_plot <- ggplot(data = world) +
  geom_sf(fill = "aliceblue", color = "gray50", size = 0.2) +
  geom_point(
    data = df_points,
    aes(x = Lon, y = Lat, color = Effect_general, shape = Taxa),
    size = 2.5, alpha = 0.8
  ) +
  scale_color_manual(
    name = "Effect Type",
    values = c(
      "Exposure only" = "darkblue",
      "Sublethal" = "darkgoldenrod",
      "Lethal" = "darkred"
    ),
    limits = c("Exposure only", "Sublethal", "Lethal")
  ) +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90)) +
  labs(x = NULL, y = NULL, title = map_title) +  # Remove axis titles
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = NA, color = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 18),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1)))

# Add marginal density plots grouped by Effect_general
map_with_densities <- ggMarginal(
  main_plot,
  type = "density",
  margins = "both",
  size = 8,
  alpha = 0.2,
  groupColour = TRUE,
  groupFill = TRUE
)

map_with_densities

ggsave("Plastic_Entanglement_Effects_map_with_shape_title_MargDens.pdf", map_with_densities, width = 14, height = 12)




 # Old code below here ----



# Count number of studies per year per taxa per effect tested
yearly_summary <- ELM_summary_GLOVE_joined %>%
  group_by(Year, Taxa, Effect_general) %>%
  summarise(n_studies = n_distinct(Reference), .groups = "drop")

# Plot
ggplot(yearly_summary, aes(x = Year, y = n_studies, color = Effect_general)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Taxa, scales = "free_y") +
  labs(
    title = "Trends in Plastic Ingestion Studies Over Time",
    x = "Year",
    y = "Number of Unique Studies",
    color = "Exposure tested?"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Studies that recorded negative impacts----

# Studies that measured effects (regardless of response) over time----
ggplot(
  filter(ELM_summary_df, 
         `Effect measured` != "Exposure" & 
           !is.na(`Effect demonstrated`)
  ),
  aes(x = Decade, fill = `Effect demonstrated`)
) +
  geom_bar(position = "stack") +
  facet_wrap(~ Taxa, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Yes" = "darkred",
      "No" = "darkblue"   
    )
  ) +
  labs(
    title = "Plastic Ingestion Effects by Decade and Taxa",
    x = "Decade",
    y = "Number of Studies",
    fill = "Negative effect recorded"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Summarize number of studies by Year, Taxa, and Effect demonstrated
yearly_summary <- ELM_summary_df %>%
  filter(`Effect measured` != "Exposure", !is.na(`Effect demonstrated`)) %>%
  group_by(Year, Taxa, `Effect demonstrated`) %>%
  summarise(n_studies = n_distinct(Study), .groups = "drop")



# Plot
ggplot(yearly_summary, aes(x = Year, y = n_studies, color = `Effect demonstrated`)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Taxa, scales = "free_y") +
  scale_color_manual(
    values = c(
      "Yes" = "darkred",
      "No" = "darkblue"   
    )
  ) +
  labs(
    title = "Negative Effects of Plastic Ingestion Reported Over Time",
    x = "Year",
    y = "Number of Unique Studies",
    color = "Negative effect recorded"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Filter out NA Effect_general and plot
ELM_summary_GLOVE_joined %>%
  filter(!is.na(Effect_general)) %>%
  mutate(Effect_general = factor(
    Effect_general,
    levels = c("Exposure only", "Sublethal", "Lethal"))
  ) %>%
  ggplot(aes(x = Decade, fill = Effect_general)) +
  geom_bar(position = "stack") +
  facet_wrap(~ Taxa, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Lethal" = "darkred",
      "Sublethal" = "darkgoldenrod",
      "Exposure only" = "darkblue"
    )
  ) +
  labs(
    title = "Plastic Ingestion Effects by Decade and Taxa",
    x = "Decade",
    y = "Number of Reports",
    fill = "Effect measured"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





ELM_summary_GLOVE_joined %>%
  filter(!is.na(Effect_general)) %>%
  mutate(
    Effect_general = factor(
      Effect_general,
      levels = c("Exposure only", "Sublethal", "Lethal")
    )
  ) %>%
  ggplot(aes(x = Decade, fill = Effect_general)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Taxa, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Exposure only" = "darkblue",
      "Sublethal" = "darkgoldenrod",
      "Lethal" = "darkred"
    )
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportional Plastic Ingestion Effects by Decade and Taxa",
    x = "Decade",
    y = "Proportion of Reports",
    fill = "Effect measured"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








