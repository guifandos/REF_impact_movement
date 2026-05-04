
library(tidyverse)
library(waffle)
library(babynames)
library(extrafont)
library(MetBrewer)
library(forcats)
library(dplyr)
library(forcats)

# Primero, un pequeño helper para categorizar las energías
papers_final <- papers_final %>%
  mutate(energy_grouped = fct_collapse(energy,
                                       Eolic = c("Eolic terrestrial", "Offshore eolic"),
                                       Tidal = "Tidal power",
                                       Hydro = "Hydroelectric",
                                       Solar = c("Solar and termosolar"),
                                       Other = "Other"))


resumen_componente <- function(data, componente){
  n_total <- nrow(data)  # Total de estudios en el dataset
  n_yes <- sum(data[[componente]] == "Yes", na.rm = TRUE)  # Total de estudios con "Yes"
  data %>%
    filter(.data[[componente]] == "Yes") %>%
    group_by(energy_grouped, geographical_scale) %>%
    summarise(
      n = n(),
      perc_total = round(n / n_total * 100, 1),
      perc_yes = round(n / n_yes * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(n))
}


# Crear los resúmenes para cada componente y añadir el nombre del componente como columna
how_tbl <- resumen_componente(papers_final, "how") %>% mutate(componente = "how")
when_tbl <- resumen_componente(papers_final, "when") %>% mutate(componente = "when")
where_tbl <- resumen_componente(papers_final, "where") %>% mutate(componente = "where")
why_tbl <- resumen_componente(papers_final, "why") %>% mutate(componente = "why")

# Unir todos los resúmenes en una sola tabla
resumen_total <- bind_rows(how_tbl, when_tbl, where_tbl, why_tbl)

# Ver resultado
resumen_total

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(resumen_total, "output/tables/summary_component_percentage.csv")

# Frecuencia general de cada componente
papers_final %>%
  select(how, where, when, why) %>%
  summarise(across(everything(), ~mean(.x == "Yes", na.rm = TRUE) * 100))

papers_final %>%
  select(how, where, when, why) %>%
  summarise(across(everything(),
                   list(percent = ~mean(.x == "Yes", na.rm = TRUE) * 100,
                        n_yes = ~sum(.x == "Yes", na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

how_summary <- papers_final %>%
  filter(how == "Yes") %>%
  group_by(energy_grouped, geographical_scale) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))


resumen_componente(papers_final, "how")
resumen_componente(papers_final, "when")
resumen_componente(papers_final, "why")
resumen_componente(papers_final, "where")


how_summary <- resumen_componente(papers_final, "how")

# Waffle por tipo de energía (suma todos los niveles de escala)
how_energy_summary <- how_summary %>%
  group_by(energy_grouped) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  arrange(desc(n))

# Crear gráfico waffle (cada cuadrado = 1 artículo)
ggplot(how_energy_summary) +
  geom_waffle(aes(fill = energy_grouped, values = n),
              color = "grey90", size = 0.25,
              n_rows = 5) +
  coord_equal() +
  MetBrewer::scale_fill_met_d("Renoir", direction = 1) +
  theme_void() +
  labs(
    fill = "Tipo de energía",
    title = "Distribución de estudios con componente 'how'",
    subtitle = "Número de estudios por tipo de energía"
  )


# Crear una categoría combinada
how_summary <- how_summary %>%
  mutate(cat_comb = paste(energy_grouped, geographical_scale, sep = " - "))


ggplot(how_summary) +
  geom_waffle(aes(fill = cat_comb, values = n),
              color = "white", size = 0.25,
              n_rows = 5) +
  coord_equal() +
  MetBrewer::scale_fill_met_d("Renoir", guide = guide_legend(ncol = 1)) +
  theme_void() +
  labs(
    fill = "Energía × Escala",
    title = "Distribución de estudios con componente 'how'",
    subtitle = "Combinación de tipo de energía y escala geográfica"
  )

ggplot(how_summary) +
  geom_waffle(aes(fill = energy_grouped  , values = n),
              color = "white", size = 0.25,
              n_rows = 5) +
  facet_wrap(~geographical_scale) +
  coord_equal() +
  MetBrewer::scale_fill_met_d("Renoir") +
  theme_void() +
  labs(
    fill = "Tipo de energía",
    title = "Distribución de estudios con 'how' por escala geográfica"
  )



# Paso 3: generar resumen del componente "when"
when_summary <- resumen_componente(papers_final, "when")

# Paso 4: resumen total por tipo de energía, completando con n = 0 para "Other"
when_energy_summary <- when_summary %>%
  group_by(energy_grouped) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  complete(energy_grouped = factor(energy_levels, levels = energy_levels), fill = list(n = 0)) %>%
  arrange(desc(n))

# Paso 5: gráfico waffle con escala manual y "Other" en gris
ggplot(when_energy_summary) +
  geom_waffle(aes(fill = energy_grouped, values = n),
              color = "grey90", size = 0.25,
              n_rows = 5) +
  coord_equal() +
  scale_fill_manual(values = energy_colors, drop = FALSE) +
  theme_void() +
  labs(
    fill = "Tipo de energía",
    title = "Distribución de estudios con componente 'when'",
    subtitle = "Número de estudios por tipo de energía"
  )

