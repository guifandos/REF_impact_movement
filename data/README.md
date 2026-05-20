# Data dictionary

This directory holds the input data needed to reproduce the analyses.
Files are released under the Creative Commons Attribution 4.0 International
licence; see ../LICENSE-data for the full text and attribution requirements.

Three input files are stored here:

  - review_table.csv             Consolidated systematic-review table.
  - gdp_per_capita_maddison.csv  Per-country GDP per capita (external).
  - renewable_share_energy.csv   Share of primary energy from renewables
                                 (external).


-------------------------------------------------------------------------------
review_table.csv
-------------------------------------------------------------------------------

Consolidated table of papers screened during the systematic review. One row
per paper accepted into the review (rows with Suitability = "Yes"). The raw
extraction sheets compiled by individual reviewers are not redistributed in
this repository; this file is the canonical input for the analysis pipeline.

Column descriptions (in column order):

  reviewer (character)
      Internal code of the reviewer who extracted the paper. Kept for
      provenance only; not used by the analysis.

  ID paper (integer)
      Unique identifier within this review. Values are non-consecutive
      because they preserve the original IDs assigned during screening.

  Paper title (character)
      Title of the source publication.

  Year of publication (integer)
      Year the paper was published.

  Suitability (factor: "Yes" / "No")
      Eligibility flag from screening. Only "Yes" rows are retained
      downstream by the analysis pipeline.

  Country (character)
      Country (or sub-national region) of the study, as reported by
      reviewers. Standardised in R/02_country_corrections.R for mapping.

  Area (character)
      Free-text description of the study area.

  Wallace bioregion (character)
      Wallace biogeographic realm. One of: Afrotropical, Australasian,
      Indo-Malay, Nearctic, Neotropical, Oceanian, Palearctic, Panamanian,
      Saharo-Arabian, Sino-Japanese.

  Taxon (character)
      Higher taxonomic group (e.g. birds, mammals, fishes, insects).

  Specie/s name/s (character)
      Latin binomials of focal species, comma-separated.

  Number of species (integer)
      Number of focal species in the study.

  Energy (character)
      Renewable-energy technology. One of: "Eolic terrestrial", "Offshore
      eolic", "Hydroelectric", "Solar and termosolar", "Tidal power", or
      "Other".

  How (factor: "Yes" / "No" / "Doubt")
      Whether the paper addresses the *how* component of movement (mode,
      gait, behaviour). "Doubt" is recoded to "Yes" by 01_analysis_main.R
      before downstream tabulation.

  Where (factor: "Yes" / "No" / "Doubt")
      Whether the paper addresses the *where* component (spatial location,
      route). Same recoding as above.

  Why (factor: "Yes" / "No" / "Doubt")
      Whether the paper addresses the *why* component (motivation,
      drivers). Same recoding as above.

  When (factor: "Yes" / "No" / "Doubt")
      Whether the paper addresses the *when* component (timing,
      phenology). Same recoding as above.

  Geographical scale (factor)
      One of: "local", "regional", "continental", "global".

  Ecological scale (factor)
      One of: "individuals", "populations", "species", "communities",
      "ecosystems".

  Tracking method (character)
      Method used to record movement (e.g. biologgers, radar,
      bioacoustics, no tracking).

  Ecosystem (character)
      Habitat type (e.g. forests, oceans/coasts, grasslands).

  time period data (character)
      Period over which movement data were collected, free-text. Not
      used by the analyses reproduced here.

  Short (factor: "Yes" / "No")
      Short spatial scale flag. Used to derive Spatial_scale in the
      analysis pipeline.

  Medium (factor: "Yes" / "No")
      Medium spatial scale flag.

  Large (factor: "Yes" / "No")
      Large spatial scale flag.

  Comments (character)
      Reviewer comments, free-text. Optional.


Recoding applied by the pipeline
--------------------------------

The "Doubt" entries in How, Where, Why and When are recoded to "Yes" in
R/01_analysis_main.R before downstream tabulation. This is the behaviour
used in the published manuscript and is preserved for reproducibility.


Known data-quality issues
-------------------------

  - A small number of papers have an empty corrected country name in the
    internal mapping used by R/02_country_corrections.R. These rows drop
    out of the bivariate world map (R/06_map_bivariate.R) via the inner
    join with rnaturalearth. This faithfully reproduces the figure as
    published.

  - The "time period data" column is free-text and not used in the
    analyses reproduced here.


-------------------------------------------------------------------------------
gdp_per_capita_maddison.csv
-------------------------------------------------------------------------------

Per-country GDP per capita time series from the Maddison Project Database
(version 2020), redistributed via Our World in Data.

Source:
  Bolt, J. & van Zanden, J.L. (2020). Maddison-style estimates of the
  evolution of the world economy. A new 2020 update. Maddison Project
  Working Paper WP-15.

Mirror used for download:
  https://ourworldindata.org/grapher/maddison-data-gdp-per-capita-in-2011us

Licence: CC BY 4.0.

Column descriptions:

  Entity (character)
      Country or region name.

  Code (character)
      ISO 3166-1 alpha-3 country code.

  Year (integer)
      Calendar year.

  GDP per capita (numeric)
      GDP per capita in 2011 international dollars.

  900793-annotations (character)
      Optional annotations from the original dataset.


-------------------------------------------------------------------------------
renewable_share_energy.csv
-------------------------------------------------------------------------------

Share of primary energy from renewables, by country and year, from Our
World in Data.

Source:
  Our World in Data. Renewable Energy.
  https://ourworldindata.org/renewable-energy
  Based on the Energy Institute Statistical Review of World Energy and
  on Ember.

Licence: CC BY 4.0.

Column descriptions:

  Entity (character)
      Country or region name.

  Code (character)
      ISO 3166-1 alpha-3 country code. May be empty for aggregate regions.

  Year (integer)
      Calendar year.

  Renewables (% equivalent primary energy) (numeric)
      Share of primary energy consumption from renewables, in percent.


-------------------------------------------------------------------------------
Provenance of intermediate files
-------------------------------------------------------------------------------

Files in ../output/processed/ are regenerated by the analysis pipeline.
They are not part of the source data and should not be edited manually.
If you need to start from scratch, delete the contents of
../output/processed/ and re-run the R/ scripts in numeric order.
