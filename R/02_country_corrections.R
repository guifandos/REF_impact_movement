# 02_country_corrections.R
# Apply manual country-name corrections to df_plot.csv and tabla_freq_country.csv.
#
# Background: the original analysis stored the corrected country column in two
# spreadsheets edited by hand (df_plot_corr.csv, tabla_freq_country_corr.csv).
# This script reproduces that mapping programmatically so the workflow is fully
# scriptable.
#
# Some entries in the original spreadsheets had `country_cor` left empty;
# those cases are preserved here as NA for faithful reproduction. Papers with
# NA `country_cor` drop out of the country-level map via inner_join in
# 06_map_bivariate.R. See data/README.md for known data quality issues.
#
# Run after 01_analysis_main.R has produced output/processed/df_plot.csv and
# output/processed/tabla_freq_country.csv.

library(tidyverse)

# Mapping table: original country string -> standardised name matching
# rnaturalearth `admin`. NA means "leave unmapped" (mirrors the original
# manual spreadsheet, where some rows had no correction filled in).
country_corrections <- tribble(
  ~country,                          ~country_cor,
  "USA",                             "United States of America",
  "United States",                   "United States of America",
  "Wyoming",                         "United States of America",
  "Florida",                         "United States of America",
  "Steuben Maine",                   "United States of America",
  "New Jersey y Massachusetts",      "United States of America",
  "North Carolina and Georgia",      "United States of America",
  "UK",                              "United Kingdom",
  "Great Britain",                   "United Kingdom",
  "England",                         "United Kingdom",
  "Scotland",                        "United Kingdom",
  "Scotland/UK",                     "United Kingdom",
  "United Kindom",                   "United Kingdom",
  "United Kingdon (Scotland)",       "United Kingdom",
  "Belgiun",                         "Belgium",
  "Sweeden",                         "Sweden",
  "North Sea",                       "Sweden",
  "Germany, Denmark",                "Denmark",
  "Lesotho",                         "South Africa"
)

apply_corrections <- function(x) {
  # Default: country_cor = country (no change). Override where mapping exists.
  out <- x
  hits <- match(x, country_corrections$country)
  out[!is.na(hits)] <- country_corrections$country_cor[hits[!is.na(hits)]]
  out
}

# df_plot ---------------------------------------------------------------------
df_plot <- read_csv2("output/processed/df_plot.csv",
                     show_col_types = FALSE)

df_plot_corr <- df_plot %>%
  mutate(country_cor = apply_corrections(country), .after = country)

write_csv2(df_plot_corr, "output/processed/df_plot_corr.csv")

# tabla_freq_country ----------------------------------------------------------
table_freq <- read_csv2("output/processed/tabla_freq_country.csv",
                        show_col_types = FALSE)

table_freq_corr <- table_freq %>%
  mutate(country = apply_corrections(country)) %>%
  group_by(wallace_bioregion, country, energy) %>%
  summarise(n = sum(n), .groups = "drop")

write_csv2(table_freq_corr, "output/processed/tabla_freq_country_corr.csv")
