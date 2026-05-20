# -----------------------------------------------------------------------------
# helpers/summary_organizational_level.R
# Purpose: Auxiliary summary used during exploration. Tabulates papers by
#          renewable-energy technology and organisational level
#          (individual / population / species / community / ecosystem).
#          Not part of the figures published in the manuscript.
# Inputs:  papers_final (in memory; created by sourcing
#          ../01_analysis_main.R in the same R session)
# Outputs: none persisted (interactive use)
# Author:  Arrondo E., Fandos G. and co-authors
# Date:    2026
# R:       4.5.3. See ../../sessionInfo.txt for the exact package versions.
# -----------------------------------------------------------------------------

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
resumen_organizational_lvl <- papers_final %>%
  group_by(organizational_lvl_fact, energy_grouped) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(organizational_lvl_fact) %>%
  mutate(
    total_escala = sum(n),
    perc_within_scale = round(n / total_escala * 100, 1)
  ) %>%
  ungroup() %>%
  mutate(
    perc_total = round(n / n_total * 100, 1)
  ) %>%
  select(organizational_lvl_fact, energy_grouped, n, perc_within_scale, perc_total) %>%
  arrange(desc(perc_total))

# Ver resultados
print(resumen_organizational_lvl)

# Guardar CSV
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(resumen_organizational_lvl, "output/tables/summary_organizational_lvl.csv", row.names = FALSE)

# Resumen por escala espacial
summary_spatial_scale <- papers_final %>%
  count(spatial_scale) %>%
  mutate(
    perc_total = round(n / n_total * 100, 1)
  ) %>%
  arrange(desc(n))

# Ver resultado
print(summary_spatial_scale)
