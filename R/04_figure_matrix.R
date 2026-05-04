library(tidyverse)
library(forcats)

# 1) Agrupa tecnologías, separando onshore/offshore
papers_check <- papers_final %>%
  mutate(
    tech = fct_collapse(
      energy,
      "Wind (onshore)"  = "Eolic terrestrial",
      "Wind (offshore)" = "Offshore eolic",
      "Hydropower"      = "Hydroelectric",
      "Solar"           = "Solar and termosolar",
      "Tidal"           = "Tidal power",
      "Other"           = "Other"
    ),
    tech = fct_relevel(tech, "Wind (onshore)", "Wind (offshore)", "Hydropower", "Solar", "Tidal", "Other")
  )

# 2) Pasa a formato largo y cuenta "Yes" por componente
tbl_counts <- papers_check %>%
  pivot_longer(cols = c(how, where, why, when),
               names_to = "component", values_to = "value") %>%
  filter(value == "Yes") %>%
  count(tech, component, name = "N") %>%
  mutate(component = recode(component,
                            how = "How", where = "Where", why = "Why", when = "When")) %>%
  complete(tech, component, fill = list(N = 0)) %>%
  group_by(tech) %>%
  mutate(pct_within_tech = 100 * N / sum(N)) %>%
  ungroup()

# 3) Tabla ancha (para comparar con el manuscrito)
tbl_wide <- tbl_counts %>%
  select(tech, component, N) %>%
  pivot_wider(names_from = component, values_from = N) %>%
  mutate(Total_row_sum = How + Where + Why + When)

print(tbl_wide)

# 4) Si quieres chequear totales globales por componente:
totals_by_component <- tbl_counts %>%
  group_by(component) %>%
  summarise(N = sum(N), .groups = "drop")

print(totals_by_component)

# 5) Export opcional
dir.create("output/processed", showWarnings = FALSE, recursive = TRUE)
write.csv(tbl_counts, "output/processed/check_counts_tech_component_long.csv", row.names = FALSE)
write.csv(tbl_wide,   "output/processed/check_counts_tech_component_wide.csv", row.names = FALSE)


library(tidyverse)
library(ggplot2)
library(MetBrewer)

# --- 1) Paste your tbl_wide output as a tibble (from print(tbl_wide)) ---
tbl_wide <- read.csv("output/processed/check_counts_tech_component_wide.csv")


N_total_studies <- 135  # el N del PRISMA / revisión

mat <- tbl_wide %>%
  select(-Total_row_sum) %>%
  pivot_longer(cols = c(How, Where, Why, When),
               names_to = "component", values_to = "N") %>%
  mutate(
    pct_total = 100 * N / N_total_studies,
    # etiqueta: N y (% del total)
    label = paste0(N, "\n(", sprintf("%.1f", pct_total), "%)")
  ) %>%
  mutate(
    tech = factor(tech, levels = c("Wind (onshore)", "Wind (offshore)",
                                   "Hydropower", "Solar", "Tidal", "Other")),
    component = factor(component, levels = c("How", "Where", "Why", "When"))
  )

p <- ggplot(mat, aes(x = component, y = tech, fill = N)) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(aes(label = label), size = 3.8, lineheight = 0.95) +
  # Paleta secuencial elegante, publicable y estable
  scale_fill_gradient(
    name = "N studies",
    low  = "#F3F5F7",   # gris muy claro
    high = "#1F4E79"    # azul sobrio
  ) +
  scale_fill_gradient(
    name = "N studies",
    low = "#F7F3ED",
    high = "#8C5A2B"
  ) +
  scale_fill_gradient(
    name = "N studies",
    low = "#F1F7F3",
    high = "#1B7C59"
  ) +
  scale_fill_gradient(
    name = "N studies",
    low  = "#F5F1EA",  # beige claro
    high = "#1F3B5C"   # azul sobrio
  ) +
  labs(
    x = "Movement component",
    y = NULL,
    #title = "Evidence matrix of movement studies across renewable energy technologies",
    #subtitle = "Cell colour shows N studies; labels show N and % of total studies (N = 135)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    plot.title.position = "plot",
    legend.position = "right"
  )

p

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/Figure_matrix_REF_movement_counts.png", p, width = 8.2, height = 4.8, dpi = 300)
ggsave("output/figures/Figure_matrix_REF_movement_counts.tiff", p, width = 8.2, height = 4.8, dpi = 300, compression = "lzw")



p <- ggplot(mat, aes(x = component, y = tech, fill = N)) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(
    aes(label = label),
    size = 3.8,
    lineheight = 0.95,
    color = "black"
  ) +
  scale_fill_gradient(
    name = "Number of studies",
    low  = "#F5F1EA",  # beige claro
    high = "#1F3B5C"   # azul sobrio
  ) +
  labs(
    x = "Movement component",
    y = NULL,
    #title = "Evidence matrix of movement studies across renewable energy technologies",
    #subtitle = "Cell colour indicates the number of studies; labels show N and percentage of total studies (N = 135)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    plot.title.position = "plot",
    legend.position = "right"
  )

# Mostrar figura
p

p <- ggplot(mat, aes(x = component, y = tech, fill = N)) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(
    aes(
      label = label,
      color = ifelse(N >= 15, "white", "black")
    ),
    size = 3.8,
    lineheight = 0.95
  ) +
  scale_color_identity() +
  scale_fill_gradient(
    name = "Number of studies",
    low  = "#F5F1EA",  # beige claro
    high = "#1F3B5C"   # azul sobrio
  ) +
  labs(
    x = "Movement component",
    y = NULL,
    #title = "Evidence matrix of movement studies across renewable energy technologies",
    #subtitle = "Cell colour indicates the number of studies; labels show N and percentage of total studies (N = 135)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    plot.title.position = "plot",
    legend.position = "right"
  )

# Mostrar figura
p


ggsave("output/figures/Figure_matrix_REF_movement_counts.png", p, width = 8.2, height = 4.8, dpi = 300)
ggsave("output/figures/Figure_matrix_REF_movement_counts.tiff", p, width = 8.2, height = 4.8, dpi = 300, compression = "lzw")
