# -----------------------------------------------------------------------------
# 06_map_bivariate.R
# Purpose: Build the bivariate world map combining study density (count of
#          papers per country) with each country's share of renewables in
#          its primary energy consumption.
# Inputs:  output/processed/df_plot_corr.csv,
#          output/processed/tabla_freq_country_corr.csv,
#          data/renewable_share_energy.csv
# Outputs: output/figures/map_bivariate.pdf,
#          output/figures/map_bivariate.png,
#          output/processed/map_bivariate_v1.pdf .. _v3.pdf (intermediate
#          drafts kept for the manuscript revision trail)
# Author:  Arrondo E., Fandos G. and co-authors
# Date:    2026
# R:       4.5.3. See ../sessionInfo.txt for the exact package versions.
# -----------------------------------------------------------------------------

library(readxl)
library(tidyverse)
library(dplyr)
library(maps)
library(ggplot2)
library(ggthemes)
library(sf)
library("rnaturalearth")
library("rnaturalearthdata")
library(ggrepel)
library(viridis)
library(biscale)
library(cowplot)
library(patchwork)


# Load world data and studies data----

world <- ne_countries(scale = "medium", returnclass = "sf")
world_coast <- ne_coastline(scale = "medium", returnclass = "sf")

country_data <-  read_csv2("output/processed/df_plot_corr.csv")
names(country_data)
country_data_freq <- country_data %>%
  dplyr::select(country_cor) %>%
  mutate_if(is.character,as.factor) %>% 
  group_by(country_cor) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


world_map <- inner_join(
  world,
  country_data_freq,
  by = join_by(admin == country_cor)
)

## Load renewable energy data----
# https://ourworldindata.org/renewable-energy
renewable_energy_data <-  read_csv("data/renewable_share_energy.csv")
names(renewable_energy_data)
ren_data <- renewable_energy_data %>% 
  filter(Year== max(Year)) %>% 
  drop_na(Code)

ren_data$Entity
world$admin

ren_data[ren_data == "United States"] <- "United States of America"
ren_data[ren_data == "Czechia"] <- "Czech Republic"

world_map2 <- inner_join(
  world,
  ren_data,
  by = join_by(admin == Entity)
) %>% 
  mutate(renewables_percent= `Renewables (% equivalent primary energy)`)

# Plot world map with the data------

## Combined data-----
world_map_studies <- inner_join(
  world,
  country_data_freq,
  by = join_by(admin == country_cor)
)

studies_ren <- inner_join(
  world_map_studies,
  ren_data,
  by = join_by(admin == Entity)
)

# Supongamos que tienes los datos de los estudios y energía renovable
# Estándarizar y escalar las dos variables: número de estudios y energía renovable
combined_data <- studies_ren %>%
  mutate(
    # Estandarización
    studies_z = (n - mean(n, na.rm = TRUE)) / sd(n, na.rm = TRUE),
    renewables_z = ( `Renewables (% equivalent primary energy)` - 
                       mean(`Renewables (% equivalent primary energy)`, na.rm = TRUE)) / 
      sd(`Renewables (% equivalent primary energy)`, na.rm = TRUE),
    
    # Escalado a rango 0-1
    studies_scaled = scales::rescale(studies_z, to = c(0, 1)),
    renewables_scaled = scales::rescale(renewables_z, to = c(0, 1)),
    
    # Calcular la diferencia entre estudios y energía renovable
    difference = studies_scaled - renewables_scaled
  )

# Crear mapa bivariante -----

# Número de clases después de cero
n_classes <- 3

# Función para generar breaks personalizados (0 + cuantiles)
custom_breaks <- function(var, n_classes = 3, style = "quantile") {
  var_pos <- var[var > 0 & !is.na(var)]
  
  # breaks mayores que 0
  if (style == "quantile") {
    extra_breaks <- quantile(var_pos, probs = seq(0, 1, length.out = n_classes + 1), na.rm = TRUE)
  } else if (style == "equal") {
    extra_breaks <- seq(min(var_pos), max(var_pos), length.out = n_classes + 1)
  } else {
    stop("Unsupported style")
  }
  
  # unir con 0 como primer break
  breaks <- unique(c(0, extra_breaks))
  breaks
}

# Crear cortes
x_breaks <- custom_breaks(combined_data$studies_scaled, n_classes = 3)
y_breaks <- custom_breaks(combined_data$renewables_scaled, n_classes = 3)


x_breaks <- c(0, 0.04347826, 0.18, 0.6, 1.00001)
y_breaks <- c(0, 0.04, 0.15, 0.6, 1.00001)

# Ajustar el último corte restando una milésima (puedes usar 1e-6 también)
x_breaks[length(x_breaks)] <- x_breaks[length(x_breaks)] + 0.01
y_breaks[length(y_breaks)] <- y_breaks[length(y_breaks)] + 0.01

# Clasificar valores
combined_data <- combined_data %>%
  mutate(
    x_class = cut(studies_scaled, breaks = x_breaks, labels = FALSE, include.lowest = TRUE, right = FALSE),
    y_class = cut(renewables_scaled, breaks = y_breaks, labels = FALSE, include.lowest = TRUE, right = FALSE),
    bi_class = paste0(x_class, "-", y_class)
  )

# Mapa bivariante ----

map <- ggplot() +
  # Países sin datos en gris claro
  geom_sf(data = world_map, fill = "#d9d9d9", color = "#d9d9d9", size = 0.01) +
  
  # Países con datos coloreados (los que tienen bi_class, es decir no NA)
  geom_sf(data = combined_data %>% filter(!is.na(bi_class)),
          aes(fill = bi_class), color = "grey40", size = 0.1) +
  
  # Escala biscale con paleta y dimensiones definidas
  bi_scale_fill(pal = "DkCyan2", dim = 4) +
  guides(fill = "none") +
  
  # Capa de línea de costa (opcional)
  #geom_sf(data = world_coast, color = "grey80", size = 0.001, fill = NA) +
  
  # Tema limpio y claro
  bi_theme()

map

# Crear leyenda
legend <- bi_legend(
  pal = "DkCyan2",
  dim = 4,
  xlab = "More studies",
  ylab = "More renewables",
  size = 6
)

# Combinar mapa y leyenda
final_plot <- map + inset_element(legend, left = 0.40, bottom = 0.05, right = 1, top = 0.35)

# Mostrar el mapa final
print(final_plot)


dir.create("output/processed", showWarnings = FALSE, recursive = TRUE)
ggsave("output/processed/map_bivariate_v1.pdf", plot = final_plot,
       width = 8, height = 6, units = "in", device = cairo_pdf)


# Mapa bivariante ----

map <- ggplot() + 
  
  # Países sin datos en gris claro
  geom_sf(data = world, fill = "#d9d9d9", color = "#d9d9d9", size = 0.01) +
  
  # Países con datos coloreados
  geom_sf(data = combined_data %>% filter(!is.na(bi_class)),
          aes(fill = bi_class), color = "grey40", size = 0.1) +
  
  # CAMBIO: Paleta más contrastante
  bi_scale_fill(pal = "BlueGold", dim = 4) +
  guides(fill = "none") +
  
  bi_theme()

# Crear leyenda con texto más grande
legend <- bi_legend(
  pal = "BlueGold",
  dim = 4,
  xlab = "More studies",
  ylab = "More renewables",
  size = 10  # CAMBIO: Aumentado de 6 a 10
) +
  # Ajustes adicionales para legibilidad
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Combinar mapa y leyenda (leyenda más grande)
final_plot <- map + 
   inset_element(legend, left = 0.40, bottom = 0.05, right = 1, top = 0.35)

# Guardar
ggsave("output/processed/map_bivariate_v2.pdf", plot = final_plot,
       width = 10, height = 6, units = "in", device = cairo_pdf)


######
# Mapa bivariante ----

# Paleta Violeta-Dorado: sin connotaciones, daltonismo-friendly, alto contraste

custom_pal <- c(
  # -------- y=1 (low renewables) --------
  "1-1" = "#f0f0f0",
  "2-1" = "#d4c67a",
  "3-1" = "#c4a845",
  "4-1" = "#a68a00",   # High studies, low renewables = dorado oscuro
  
  # -------- y=2 --------
  "1-2" = "#d5c8e0",
  "2-2" = "#c0b48a",
  "3-2" = "#a89850",
  "4-2" = "#8a7500",
  
  # -------- y=3 --------
  "1-3" = "#a890bf",
  "2-3" = "#9a8890",
  "3-3" = "#857055",
  "4-3" = "#6b5a20",
  
  # -------- y=4 (high renewables) --------
  "1-4" = "#6a4c93",   # Low studies, high renewables = violeta intenso
  "2-4" = "#5f4a70",
  "3-4" = "#544840",
  "4-4" = "#3d3520"    # High both = marrón neutro
)

map <- ggplot() +
  # Fondo: todos los países en gris
  geom_sf(data = world, fill = "#e0e0e0", color = "white", linewidth = 0.15) +
  
  # Datos: países con valores
  geom_sf(data = combined_data %>% filter(!is.na(bi_class)),
          aes(fill = bi_class), color = "white", linewidth = 0.15) +
  
  scale_fill_manual(values = custom_pal, na.value = "#e0e0e0") +
  guides(fill = "none") +
  
  # Límites para excluir Antártida
  
  coord_sf(ylim = c(-56, 90), expand = FALSE) +
  
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(5, 5, 5, 5)
  )

# Leyenda grande y elegante
legend_data <- expand.grid(x = 1:4, y = 1:4) %>%
  mutate(bi_class = paste0(x, "-", y))

legend <- ggplot(legend_data, aes(x = x, y = y, fill = bi_class)) +
  geom_tile(color = "white", linewidth = 0.8) +
  scale_fill_manual(values = custom_pal) +
  labs(
    x = "More studies →",
    y = "More renewables →"
  ) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 12, hjust = 0.5, margin = margin(t = 8), 
                                color = "grey20", face = "bold"),
    axis.title.y = element_text(size = 12, hjust = 0.5, angle = 90, margin = margin(r = 8), 
                                color = "grey20", face = "bold"),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Leyenda MÁS GRANDE - posición ajustada
final_plot <- map + 
  inset_element(legend, left = 0.32, bottom = -0.1, right = 1.1, top = 0.3)

print(final_plot)

# Guardar con clip desactivado para que la leyenda pueda salir del área
ggsave("output/processed/map_bivariate_v3.pdf", plot = final_plot,
       width = 10, height = 6, units = "in", device = cairo_pdf)

## Definitivo -----

# Paleta Violeta-Dorado
custom_pal <- c(
  "1-1" = "#f0f0f0",
  "2-1" = "#d4c67a",
  "3-1" = "#c4a845",
  "4-1" = "#a68a00",
  
  "1-2" = "#d5c8e0",
  "2-2" = "#c0b48a",
  "3-2" = "#a89850",
  "4-2" = "#8a7500",
  
  "1-3" = "#a890bf",
  "2-3" = "#9a8890",
  "3-3" = "#857055",
  "4-3" = "#6b5a20",
  
  "1-4" = "#6a4c93",
  "2-4" = "#5f4a70",
  "3-4" = "#544840",
  "4-4" = "#3d3520"
)

# Mapa
map <- ggplot() +
  geom_sf(data = world, fill = "#e0e0e0", color = "grey40", linewidth = 0.1) +
  geom_sf(data = combined_data %>% filter(!is.na(bi_class)),
          aes(fill = bi_class), color = "grey40", linewidth = 0.1) +
  scale_fill_manual(values = custom_pal, na.value = "#e0e0e0") +
  guides(fill = "none") +
  coord_sf(ylim = c(-56, 90), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Leyenda más compacta
legend_data <- expand.grid(x = 1:4, y = 1:4) %>%
  mutate(bi_class = paste0(x, "-", y))

legend <- ggplot(legend_data, aes(x = x, y = y, fill = bi_class)) +
  geom_tile(color = "grey40", linewidth = 0.5) +
  scale_fill_manual(values = custom_pal) +
  labs(
    x = "More studies →",
    y = "More renewables →"
  ) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 10, hjust = 0.5, margin = margin(t = 6), 
                                color = "grey20", face = "bold"),
    axis.title.y = element_text(size = 10, hjust = 0.5, angle = 90, margin = margin(r = 6), 
                                color = "grey20", face = "bold"),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Combinar: leyenda más pequeña
final_plot <- map + legend + 
  plot_layout(widths = c(6, 1))  # Ratio 6:1 en lugar de 4:1

print(final_plot)

# Guardar (figura final del manuscrito)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/map_bivariate.pdf", plot = final_plot,
       width = 12, height = 6, units = "in", device = cairo_pdf)
ggsave("output/figures/map_bivariate.png", plot = final_plot,
       width = 12, height = 6, units = "in", dpi = 600)
