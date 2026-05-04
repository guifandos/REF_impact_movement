library(tidyverse)
library(readxl)
library(fastDummies)
library(vegan)
library(vegan3d)
library(StatMatch)
library(ggplot2)

rm(list = ls())
# Import data----
papers = read_csv("data/review_table.csv",
                  col_types = cols(Suitability = col_factor(levels = c("Yes","No"))))

papers_na <- papers[is.na(papers$Ecosystem),]

# Build Spatial_scale from Short/Medium/Large flags (same logic as 01_analysis_main.R)
papers <- papers %>%
  mutate(Spatial_scale = case_when(
    Short == "Yes" & Medium == "No"  & Large == "No"  ~ "Short_scale",
    Short == "No"  & Medium == "Yes" & Large == "No"  ~ "Medium_scale",
    Short == "No"  & Medium == "No"  & Large == "Yes" ~ "Large_scale",
    TRUE                                              ~ "Multi_scale"
  ))

papers_reduced <- papers %>%
  dplyr::select(`ID paper`, `Year of publication`, Country, Area, `Wallace bioregion`,`Number of species`, Energy, 
                How, Where, When, Why,  `Geographical scale`, `Ecological scale`, `Tracking method`, Ecosystem,
                Spatial_scale) 
papers_reduced_na <- papers_reduced %>% 
  drop_na(Energy)

# Change the 10-13 to 13 in number of species
papers_reduced <- papers_reduced %>%
  mutate(across(`Number of species`, str_replace, '10-13', '13')) %>% 
  mutate(across(`Tracking method`, str_replace, 'Radar', 'radar')) 

# Change doubt in how, where and hy by Yes; but check

papers_reduced <- papers_reduced %>%
  mutate(across(c(`How`, "Where", "Why", "When"), str_replace, 'Doubt', 'Yes'))

papers_final = papers_reduced %>% 
  dplyr::select(id_paper = `ID paper`,
                year_publication = "Year of publication", 
                country = "Country",
                area = "Area",
                wallace_bioregion = "Wallace bioregion",
                number_sp = "Number of species",
                energy = "Energy",
                how = "How",
                where = "Where",
                when = "When",
                why = "Why",
                geographical_scale = "Geographical scale",
                organizational_lvl = "Ecological scale",
                tracking_method = "Tracking method",
                ecosystem = "Ecosystem",
                spatial_scale = "Spatial_scale") %>% 
  mutate(spatial_ext = as.numeric(factor(geographical_scale, levels = c("local","regional","continental","global"))), 
         number_sp= as.numeric(number_sp), 
         year_publication= as.numeric(year_publication), 
         organizational_lvl_fact= organizational_lvl,
         organizational_lvl = as.numeric(factor(organizational_lvl, levels = c("individuals", "populations", "species", "communities", "ecosystems"))))

str(papers_final)

# Make waffle plots----

library(tidyverse)
library(waffle)
library(babynames)
library(extrafont)
library(MetBrewer)

extrafont::loadfonts()
theme_gg <-
  theme_minimal(base_family = "Rockwell") +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 15, color = "grey20"),
        plot.title = element_text(size = 21, face = "bold"),
        axis.text = element_text(size = 13, color = "grey50"),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

## Waffle for organizational level----

papers_final <- papers_final |> 
  mutate(organizational_lvl_fact = stringr::str_to_sentence(organizational_lvl_fact))

papers_organizational <- papers_final |> 
  mutate(organizational_lvl_fact = factor(organizational_lvl_fact, 
                                          levels = c("Individuals", "Populations", "Species", "Communities"), 
                                          ordered = TRUE)) |> 
  group_by(organizational_lvl_fact) |> 
  summarise(count = n(), .groups = "drop") |> 
  arrange(organizational_lvl_fact)

### Create the organizational level plot----
organizational_lvl_plot <- ggplot(papers_organizational) +
  geom_waffle(aes(fill = organizational_lvl_fact, values = count),
              color = "grey90", size = 0.2,
              n_rows = 5, alpha = 0.7, , make_proportional = TRUE) +  # Adjust rows for consistency
  scale_fill_met_d("Renoir", direction = 1) +  # Custom color palette
  coord_equal() +  # Ensure the waffles are square
  guides(fill = guide_legend(title = "Organizational Level of Research")) +  
  scale_x_continuous(labels = function(x) { paste0(x * 5, "%") }) +
  theme_minimal(base_family = "Arial") +  # Minimal theme for scientific publication
  theme(axis.text.y = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10, color = "grey30"),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom", 
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

## Waffle for tracking method----

unique(papers_final$tracking_method)
papers_final <- papers_final |> 
  mutate(tracking_method = stringr::str_to_sentence(tracking_method))

papers_tracking <- papers_final |> 
  mutate(tracking_method = factor(tracking_method, 
                                          levels = c("Bioacustic", 
                                                     "Biologgers",
                                                     "Radar",
                                                     "Marks","Isotopes", "No tracking", "Other"), 
                                          ordered = FALSE)) |> 
  group_by(tracking_method) |> 
  summarise(count = n(), .groups = "drop") |> 
  arrange(tracking_method)

### Create the organizational level plot----
tracking_plot <- ggplot(papers_tracking) +
  geom_waffle(aes(fill = tracking_method, values = count),
              color = "grey90", size = 0.2,
              n_rows = 5, alpha = 0.7, , make_proportional = TRUE) +  # Adjust rows for consistency
  scale_fill_met_d("Hokusai1", direction = 1) +  # Custom color palette
  coord_equal() +  # Ensure the waffles are square 
  guides(fill = guide_legend(title = "Tracking Method Used")) +  
  scale_x_continuous(labels = function(x) { paste0(x * 5, "%") }) +
  theme_minimal(base_family = "Arial") +  # Minimal theme for scientific publication
  theme(axis.text.y = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10, color = "grey30"),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom", 
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

## Combine waffle ----

library(patchwork)
combined <- (organizational_lvl_plot) / (tracking_plot)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/waffle_components.pdf", plot = combined,
       width = 8, height = 6, units = "in", device = cairo_pdf)
## Temporal plot depending on the energy----

# Modificar los datos
papers_final_waffle <- papers_final |> 
  mutate(fecha = as.Date(paste0(year_publication, "-01-01"))) |> 
  mutate(year = year(fecha)) |> 
  mutate(energy = factor(energy, 
                         levels = c("Eolic terrestrial", "Hydroelectric",
                                    "Offshore eolic", "Other","Solar and termosolar",
                                    "Tidal power"), ordered = FALSE)) |> 
  group_by(energy, year) |> 
  summarise(count = n(), .groups = "drop") |> 
  arrange(year, energy)

# Modificar los datos
papers_final_waffle <- papers_final_waffle |> 
  mutate(year = ifelse(year < 2012, "≤2011", as.character(year))) |>  # Agrupar años antes de 2012
  filter(year != "2023") |>  # Eliminar 2023
  mutate(energy = case_when(
    energy %in% c("Eolic terrestrial", "Offshore eolic") ~ "Wind Power",
    energy == "Hydroelectric" ~ "Hydropower",
    energy == "Solar and termosolar" ~ "Solar Energy",
    energy == "Tidal power" ~ "Tidal Power",
    energy == "Other" ~ "Other Renewable",
    TRUE ~ energy
  )) |>  
  mutate(energy = factor(energy, levels = c("Wind Power", "Hydropower", "Solar Energy", 
                                            "Tidal Power", "Other Renewable"))) |>  # Orden de niveles
  group_by(energy, year) |> 
  summarise(count = sum(count), .groups = "drop") |> 
  arrange(year, energy)

# Definir paleta de colores científica
color_palette <- MetBrewer::met.brewer("Tam", n = length(unique(papers_final_waffle$energy)))

# Crear el gráfico de Waffle
temporal_plot <- ggplot(papers_final_waffle) +
  geom_waffle(aes(fill = energy, values = count),
              alpha = 0.85, color = "white", size = 0.3, 
              n_rows = 3, flip = TRUE) +  # Más filas para mantener forma cuadrada
  scale_fill_manual(name = "Energy Type", values = color_palette) +
  scale_y_continuous(labels = function(x) x * 10, expand = c(0, 0), n.breaks = 5) +
  facet_wrap(~year, nrow = 1, strip.position = "bottom") +  # Gráfico vertical (una sola columna)
  coord_equal() +
  labs(#title = "Temporal trends in renewable energy publications that study the impact on animal movement",
       #subtitle = "Grouped before 2012 and excluding 2023",
       x = NULL, y = NULL) +
  #caption = "Source: Own data") +
  theme_minimal(base_family = "Times") +  
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 12, face = "bold"),  # Reducir título de la leyenda
        legend.text = element_text(size = 10, color = "black"),  # Leyenda más pequeña
        plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 14, color = "grey30"),
        axis.text = element_blank(),  # Ocultar texto del eje
        axis.title = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


ggsave("output/processed/waffle_temporal_v1.pdf", plot = temporal_plot,
       width = 8, height = 6, units = "in", device = cairo_pdf)

library(dplyr)

# Crear un data frame con el total por año
total_by_year <- papers_final_waffle %>%
  group_by(year) %>%
  summarise(total = sum(count))

# Crear dataframe con posición para etiquetas
label_positions <- papers_final_waffle %>%
  group_by(year) %>%
  summarise(total_count = sum(count), .groups = "drop") %>%
  mutate(
    y_pos = total_count / 3 + 1.8  # 3 = n_rows used in the geom_waffle call below
  )

library(ggplot2)
library(ggthemes)
library(waffle)

temporal_plot <- ggplot(papers_final_waffle) +
  geom_waffle(aes(fill = energy, values = count),
              alpha = 0.85, color = "white", size = 0.3, 
              n_rows = 3, flip = TRUE) +
  scale_fill_manual(name = "Energy Type", values = color_palette) +
  scale_y_continuous(labels = function(x) x * 10, expand = c(0, 0), n.breaks = 5) +
  facet_wrap(~year, nrow = 1, strip.position = "bottom") +
  geom_text(data = label_positions,
            aes(x =  1.8, y = y_pos,  label = paste0("n = ", total_count)),
            inherit.aes = FALSE,
            size = 4, color = "grey30", fontface = "bold") +
  coord_equal() +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(
    labels = function(x) x * 10,
    expand = expansion(mult = c(0, 0.1))  # 10% extra arriba
  ) +
  theme_minimal(base_family = "Times") +  
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10, color = "black"),
        plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 14, color = "grey30"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

temporal_plot
ggsave("output/figures/waffle_temporal.pdf", plot = temporal_plot,
       width = 8, height = 6, units = "in", device = cairo_pdf)

