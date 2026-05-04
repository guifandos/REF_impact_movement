library(tidyverse)
library(forcats)

# Agrupar energías como antes
papers_final <- papers_final %>%
  mutate(energy_grouped = fct_collapse(energy,
                                       Eolic = c("Eolic terrestrial", "Offshore eolic"),
                                       Tidal = "Tidal power",
                                       Hydro = "Hydroelectric",
                                       Solar = c("Solar and termosolar"),
                                       Other = "Other"))

# Total general de estudios
n_total <- nrow(papers_final)

# Crear resumen
resumen_spatial_scale <- papers_final %>%
  group_by(spatial_scale, energy_grouped) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(spatial_scale) %>%
  mutate(
    total_escala = sum(n),
    perc_within_scale = round(n / total_escala * 100, 1)
  ) %>%
  ungroup() %>%
  mutate(
    perc_total = round(n / n_total * 100, 1)
  ) %>%
  select(spatial_scale, energy_grouped, n, perc_within_scale, perc_total) %>%
  arrange(desc(perc_total))

# Ver resultados
print(resumen_spatial_scale)

# Guardar CSV
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(resumen_spatial_scale, "output/tables/summary_spatial_scale.csv", row.names = FALSE)

# Resumen por escala espacial
summary_spatial_scale <- papers_final %>%
  count(spatial_scale) %>%
  mutate(
    perc_total = round(n / n_total * 100, 1)
  ) %>%
  arrange(desc(n))

# Ver resultado
print(summary_spatial_scale)
