# REF impact on animal movement — analysis code and data

[![License: MIT (code)](https://img.shields.io/badge/code%20licence-MIT-blue.svg)](LICENSE)
[![License: CC BY 4.0 (data)](https://img.shields.io/badge/data%20licence-CC%20BY%204.0-lightgrey.svg)](LICENSE-data)
<!-- Zenodo badge will be inserted here once the first release is archived. -->

This repository contains the data and the R analysis pipeline supporting:

> Arrondo E., Fandos G., Tucker M.A., Gallagher C.A., Delgado M.M., Scacco M.,
> de los Reyes J.M., Payo-Payo A., Morant J., da Silva J.P., Assandri G.,
> Börger L., Oltra J., Fernández-Gómez L., Ortega Z., Bota G., Gómez-Catasus J.,
> Tena E., García-Alfonso M. & Pérez-García J.M. (2026).
> *Impacts of renewable energy facilities on animal movement: A global
> integrative review.* 

The pipeline reproduces the main figure (matrix of REF × movement components),
the world map of study density vs. renewable-energy share, the waffle plots of
organisational level / tracking method / temporal trend, and Supplementary
Tables S1 and S2.

## Repository layout

```
REF_impact_movement/
├── R/                            # Analysis pipeline (run in numeric order)
│   ├── 01_analysis_main.R        # Load and clean review table, build papers_final
│   ├── 02_country_corrections.R  # Standardise country names for the world map
│   ├── 03_table_S1_S2.R          # Supplementary Tables S1 and S2
│   ├── 04_figure_matrix.R        # Figure: REF × movement components matrix
│   ├── 05_waffle.R               # Waffle plots (components, temporal trend)
│   ├── 06_map_bivariate.R        # Bivariate world map (study density vs. REF share)
│   └── helpers/                  # Auxiliary summaries and NMDS exploration
│
├── data/                         # Input data (CC BY 4.0)
│   ├── review_table.csv          # Consolidated systematic-review table
│   ├── gdp_per_capita_maddison.csv
│   └── renewable_share_energy.csv
│
├── output/
│   ├── figures/                  # Final figures (PDF, PNG, TIFF)
│   ├── tables/                   # Final supplementary tables (CSV)
│   └── processed/                # Regeneratable intermediate files
│
├── REF_impact_movement.Rproj     # RStudio project file
├── sessionInfo.txt               # R version and package versions used
├── LICENSE                       # Code licence (MIT)
├── LICENSE-data                  # Data licence (CC BY 4.0)
└── CITATION.cff                  # Machine-readable citation metadata
```

## How to reproduce the analyses

1. Clone the repository and open `REF_impact_movement.Rproj` in RStudio. All
   paths in the scripts are relative to the project root.

2. Install the required R packages. The pipeline was developed under R 4.5.3
   (see `sessionInfo.txt` for exact versions). The minimum set is:

   ```r
   install.packages(c(
     "tidyverse", "readxl", "fastDummies", "vegan", "vegan3d", "StatMatch",
     "forcats", "treemapify", "viridis", "ggrepel", "patchwork", "cowplot",
     "biscale", "sf", "rnaturalearth", "rnaturalearthdata", "ggthemes",
     "maps", "waffle", "MetBrewer"
   ))
   ```

3. Run the scripts in `R/` in numeric order. Each script either reads from
   `data/` or from intermediate files produced by an earlier step in the
   pipeline.

   | Step | Script                       | Reads from                                  | Writes to                                     |
   | ---- | ---------------------------- | ------------------------------------------- | --------------------------------------------- |
   | 01   | `01_analysis_main.R`         | `data/review_table.csv`                     | `output/processed/papers_reduced.csv`, `output/processed/df_plot.csv`, `output/processed/tabla_freq_country.csv` |
   | 02   | `02_country_corrections.R`   | `output/processed/df_plot.csv`, `output/processed/tabla_freq_country.csv` | `output/processed/df_plot_corr.csv`, `output/processed/tabla_freq_country_corr.csv` |
   | 03   | `03_table_S1_S2.R`           | `papers_final` (in memory from step 01)     | `output/tables/Table_S1_*.csv`, `output/tables/Table_S2_*.csv` |
   | 04   | `04_figure_matrix.R`         | `papers_final` (in memory from step 01)     | `output/figures/Figure_matrix_REF_movement_counts.{png,tiff}` |
   | 05   | `05_waffle.R`                | `data/review_table.csv` (rebuilt internally) | `output/figures/waffle_components.pdf`, `output/figures/waffle_temporal.pdf` |
   | 06   | `06_map_bivariate.R`         | `output/processed/df_plot_corr.csv`, `data/renewable_share_energy.csv` | `output/figures/map_bivariate.pdf` |

   Steps 03 and 04 expect the object `papers_final` to be in the current R
   session, which is created by step 01. Run them in the same R session, or
   re-source step 01 first.

   The auxiliary scripts in `R/helpers/` produce additional summary tables
   used during exploration; they also depend on `papers_final` being in
   memory.

## Data sources

`data/review_table.csv` is the consolidated systematic-review table produced
by the PRISMA workflow described in the manuscript. The raw per-reviewer
extraction spreadsheets are not redistributed here. Researchers requiring
access to those raw files for replication purposes can contact the
corresponding author.

`data/gdp_per_capita_maddison.csv` and `data/renewable_share_energy.csv` are
redistributed from Our World in Data under their CC BY 4.0 licence; see
`LICENSE-data` for full attribution.

See `data/README.md` for the data dictionary of `review_table.csv`.

## Computational environment

R version: 4.5.3 (2026-03-11). The exact package versions used to generate
the published figures are listed in `sessionInfo.txt`.

## Citation

If you use this code or data, please cite both the manuscript and the
archived release of this repository (DOI to be assigned at the first Zenodo
release). See `CITATION.cff` for machine-readable metadata.

## Licence

- Source code (R scripts in `R/`): MIT licence — see `LICENSE`.
- Data files in `data/` and `output/`: Creative Commons Attribution 4.0
  International (CC BY 4.0) — see `LICENSE-data`.

## Contact

Corresponding author: Guillermo Fandos — gfandos@ucm.es
