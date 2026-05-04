library(tidyverse)
library(forcats)

# -----------------------------
# S1: Technology × Component × Spatial scale (counts)
# -----------------------------

# 1) Harmonise technology names (separating onshore/offshore) and (optionally) scale labels
papers_s1 <- papers_final %>%
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
    tech = fct_relevel(tech, "Wind (onshore)", "Wind (offshore)", "Hydropower", "Solar", "Tidal", "Other"),
    # Adjust these if your dataset uses different scale names
    scale = as.factor(geographical_scale)
  )

# 2) Long format for the four movement components
s1_long <- papers_s1 %>%
  pivot_longer(cols = c(how, where, why, when),
               names_to = "component", values_to = "value") %>%
  filter(value == "Yes") %>%
  mutate(
    component = recode(component,
                       how = "How", where = "Where", why = "Why", when = "When")
  ) %>%
  count(tech, component, scale, name = "N")

# 3) Ensure all combinations exist (fill missing with 0)
#    Define the scale order you want in the table (edit levels to match your data)
scale_levels <- levels(papers_s1$scale)

s1_long_complete <- s1_long %>%
  complete(
    tech,
    component = c("How", "Where", "Why", "When"),
    scale = scale_levels,
    fill = list(N = 0)
  )

# 4) Wide format: one row per technology; columns = Component_Scale (e.g., How_Local)
s1_wide <- s1_long_complete %>%
  mutate(col = paste(component, scale, sep = "_")) %>%
  select(tech, col, N) %>%
  pivot_wider(names_from = col, values_from = N)

# 5) Optional: order columns as How_* then Where_* then Why_* then When_*
#    (works if scale_levels are in your preferred order)
ordered_cols <- c(
  paste0("How_",   scale_levels),
  paste0("Where_", scale_levels),
  paste0("Why_",   scale_levels),
  paste0("When_",  scale_levels)
)
s1_wide <- s1_wide %>%
  select(tech, any_of(ordered_cols))

# 6) Save outputs
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(s1_long_complete, "output/tables/Table_S1_tech_component_scale_long.csv", row.names = FALSE)
write.csv(s1_wide,          "output/tables/Table_S1_tech_component_scale_wide.csv", row.names = FALSE)

# 7) Print preview
print(s1_wide)

## Table S2 #####

library(tidyverse)
library(forcats)

# -----------------------------
# S2: Technology × Component × Biological level (counts)
# -----------------------------
# ASSUMPTION:
# - You have ONE column that codes the biological level per study, e.g. `biological_level`
#   with values like: "Individual", "Population", "Species", "Community", "Ecosystem", etc.
# If your column has a different name, change `bio_col` below.

bio_col <- "organizational_lvl_fact"  # <-- CHANGE THIS if needed

# 1) Prepare data: harmonise technology + biological level
papers_s2 <- papers_final %>%
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
    tech = fct_relevel(tech, "Wind (onshore)", "Wind (offshore)", "Hydropower", "Solar", "Tidal", "Other"),
    bio = as.factor(.data[[bio_col]])
  )

# Optional: set an explicit order if your levels include these
preferred_bio_levels <- c("Genetic", "Individual", "Population", "Species", "Community", "Ecosystem")
bio_levels <- intersect(preferred_bio_levels, levels(papers_s2$bio))
if (length(bio_levels) == 0) bio_levels <- levels(papers_s2$bio)
papers_s2 <- papers_s2 %>% mutate(bio = factor(bio, levels = bio_levels))

# 2) Long format for movement components and count "Yes"
s2_long <- papers_s2 %>%
  pivot_longer(cols = c(how, where, why, when),
               names_to = "component", values_to = "value") %>%
  filter(value == "Yes") %>%
  mutate(
    component = recode(component,
                       how = "How", where = "Where", why = "Why", when = "When")
  ) %>%
  count(tech, component, bio, name = "N")

# 3) Complete missing combinations with 0
s2_long_complete <- s2_long %>%
  complete(
    tech,
    component = c("How", "Where", "Why", "When"),
    bio = bio_levels,
    fill = list(N = 0)
  )

# 4) Wide format: one row per technology; columns = Component_Bio (e.g., How_Individual)
s2_wide <- s2_long_complete %>%
  mutate(col = paste(component, bio, sep = "_")) %>%
  select(tech, col, N) %>%
  pivot_wider(names_from = col, values_from = N)

# 5) Optional: order columns by component then bio level
ordered_cols <- c(
  as.vector(outer("How",   bio_levels, paste, sep = "_")),
  as.vector(outer("Where", bio_levels, paste, sep = "_")),
  as.vector(outer("Why",   bio_levels, paste, sep = "_")),
  as.vector(outer("When",  bio_levels, paste, sep = "_"))
)
s2_wide <- s2_wide %>%
  select(tech, any_of(ordered_cols))

# 6) Save outputs
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(s2_long_complete, "output/tables/Table_S2_tech_component_biolevel_long.csv", row.names = FALSE)
write.csv(s2_wide,          "output/tables/Table_S2_tech_component_biolevel_wide.csv", row.names = FALSE)

# 7) Print preview
print(s2_wide)

