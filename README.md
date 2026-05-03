# mosquitoalert_italy

**R functions and scripts for the analysis of citizen science mosquito surveillance data in Italy**

> Developed within the framework of **PRIN 2020 — CUP E43C22000360001**

---

## Overview

This repository contains a modular R toolkit for acquiring, processing, validating, and visualising data collected through the [Mosquito Alert](https://www.mosquitoalert.com/) citizen science platform, with a focus on Italian territory. The pipeline covers the full analytical workflow: from raw data download and spatial grid construction, through sampling effort estimation and data quality checks, to an interactive Shiny dashboard for exploratory analysis.

The codebase was produced in the context of a national research project (PRIN 2020) aimed at characterising the spatiotemporal distribution of *Aedes albopictus* and other mosquito species across Italy, accounting for citizen-science-specific biases (spatial, temporal, and behavioural).

---

## Repository structure

```
mosquitoalert_italy/
│
├── 1.0_create_grid.R                        # Spatial grid generation and clipping
├── 2.0_download_ma_verified_records.R       # Public verified records from Zenodo/GitHub
├── 2.1_download_ma_private.R                # Private data acquisition (Google Drive, password-protected)
├── 2.2_unnest_ma_verified_records.R         # Unnesting and English translation of survey responses
├── 2.3_user_locations_ma_verified_records.R # User location preprocessing and yearly splitting
├── 3.0_SE_ma_verified_records.R             # Biweekly sampling effort (SE) computation
├── 4.0_check_time_ma_verified_records.R     # Temporal validation via photo EXIF metadata
├── 4.1_check_indoor_ma_verified_records.R   # Indoor/outdoor classification validation
├── 4.2_check_spatial_cor_ma_verified_records.R # Spatial autocorrelation diagnostics
└── shiny_app/                               # Interactive Shiny dashboard
    └── app.R
```

---

## Functions

### `create_grid()`
**File:** `1.0_create_grid.R`

Generates a regular spatial grid from a set of GPS point observations, clips it to a national or regional boundary (shapefile), and assigns a unique `TigacellID` to each cell based on centroid coordinates. The grid provides the spatial unit of analysis throughout the pipeline.

**Key parameters:** `input_points`, `shape_file_path`, `epsg_code`, `sample_by`, `sample_n`, `save_path`

---

### `download.ma_verified_records()`
**File:** `2.0_download_ma_verified_records.R`

Downloads publicly available Mosquito Alert verified records directly from the [MosquitoAlert/Data](https://github.com/MosquitoAlert/Data) GitHub repository. Supports multi-year queries and filtering by month, ISO-3 country code, and report type (`adult`, `bite`, `site`).

**Key parameters:** `destination.folder`, `years`, `month`, `nation`, `report.type`

---

### `download.ma_private()`
**File:** `2.1_download_ma_private.R`

Downloads password-protected private data (user locations, raw reports) from Google Drive, extracts the target file from the ZIP archive using 7-Zip, standardises coordinate column names, optionally filters by bounding box, converts to `sf`, and spatially joins records to the analysis grid.

**Key parameters:** `grid`, `file_zip_link`, `file_inside_zip_name`, `password`, `bbox`, `save_result`, `save_path`

---

### `unnest_ma_verified_records()`
**File:** `2.2_unnest_ma_verified_records.R`

Processes nested JSON survey responses embedded in each verified record. Unnests per-question answer lists, pivots to wide format, and applies a curated multilingual translation dictionary to standardise question labels and answers into English. Filters records to Italy (`country == "ITA"`).

**Key parameters:** `records`, `out_rds`

---

### `user.locations.ma_verified_records()`
**File:** `2.3_user_locations_ma_verified_records.R`

Filters user location tracks to the set of participants identified in the participation dataset, computes temporal covariates (`year`, `week`, `yday`, `biweek`), and splits the data by year into separate objects in the global R environment. Optionally saves yearly `.Rds` files.

**Key parameters:** `user_locations`, `user_participation`, `save_path`

---

### `SE.ma_verified_records()`
**File:** `3.0_SE_ma_verified_records.R`

Computes biweekly **Sampling Effort (SE)** for each Tigacell by integrating user location tracks with pre-calculated daily SE values. The function merges location data with participation records, aggregates SE by `(TigacellID, biweek, year)`, and joins the result to the verified records dataset. Missing SE values are imputed with the grand mean.

**Key parameters:** `user_participation`, `user_locations_list`, `sampling_effort`, `records`

---

### `check.time.ma_verified_records()`
**File:** `4.0_check_time_ma_verified_records.R`

Validates the temporal accuracy of citizen science reports by comparing the `creation_time` of each record against the capture timestamp extracted from the associated photo's EXIF metadata (`DateTimeOriginal` / `CreateDate`). Runs iterated random sampling and reports a weighted mean agreement rate with a 95% confidence interval.

**Key parameters:** `records`, `n_photos`, `time_unit`, `time_value`, `folder`, `n_iter`

---

### `check.indoor.ma_verified_records()`
**File:** `4.1_check_indoor_ma_verified_records.R`

Assesses the spatial accuracy of self-reported indoor/outdoor location by comparing user-declared answers (`answer_13_1`) against three independent classification methods:

| Method | Description |
|--------|-------------|
| `"dilated"` | Direct raster extraction from a morphologically dilated building footprint layer |
| `"buffer"` | Mean raster coverage within a user-defined buffer around each point |
| `"photo_exif"` | GPS coordinates extracted from photo EXIF metadata |

Returns weighted mean agreement and 95% CI across iterations.

**Key parameters:** `db_spatial`, `buil_raster`, `method`, `n_points`, `n_iter`, `buffer_dist`, `threshold`, `photo_folder`

---

### `check.spatial.cor.ma_verified_records()`
**File:** `4.2_check_spatial_cor_ma_verified_records.R`

Tests whether residual spatial autocorrelation remains after accounting for sampling effort. Fits a Bayesian negative binomial mixed model (`brms`) with `TigacellID` as a random intercept and standardised SE as a fixed effect, then applies **Moran's I** to Pearson residuals using a *k*-nearest-neighbour spatial weights matrix. Returns the fitted model, Moran's I test result, MCMC trace plots, posterior interval plots, and a Moran scatterplot.

**Key parameters:** `records`, `grid`, `k_neighbors`, `chains`, `cores`, `seed`

---

### Shiny dashboard (`app.R`)

An interactive web application providing exploratory visualisation of Mosquito Alert data for Italy. Available views:

- **Nuovi Registrati vs Totali** — Time series of newly registered vs. total active users, aggregatable by day / week / biweek / month / year, filterable by region, province, and municipality.
- **Media Record per Utente** — Mean records per user per time period with bootstrapped 95% confidence intervals.
- **Tabella Frequenze** — Frequency table of report counts per user, stratified by geographic unit.
- **Kernel Density Map** — Spatial KDE of adult mosquito records per species, masked to sampled Tigacells.
- **Evenness (Pielou's J)** — Pixel-level Pielou's evenness index between *Aedes albopictus* and *Culex* sp., masked by user sampling effort and grid coverage.

---

## Dependencies

The following R packages are used across the pipeline (auto-installed if absent):

| Domain | Packages |
|--------|----------|
| Spatial | `sf`, `sp`, `terra`, `raster`, `spdep` |
| Data wrangling | `dplyr`, `tidyr`, `purrr`, `data.table`, `lubridate`, `stringr` |
| I/O | `httr`, `jsonlite`, `googledrive`, `utils`, `tools` |
| Statistical modelling | `brms`, `MASS` |
| Visualisation | `ggplot2`, `bayesplot`, `plotly`, `leaflet`, `shiny` |
| Metadata | `exifr` |

---

## Data sources

| Dataset | Access | Description |
|---------|--------|-------------|
| Verified records | Public — [MosquitoAlert/Data](https://github.com/MosquitoAlert/Data) | Expert-validated adult, bite, and breeding site reports |
| User locations | Private — Google Drive | Anonymised GPS tracks of app users (small-cell resolution) |
| Raw reports | Private — Google Drive | Unfiltered citizen reports including metadata |
| Administrative boundaries | Public — GADM v4.1 | Italian regions, provinces, and municipalities |
| Building footprint raster | External | Binary/probabilistic indoor classification layer |

---

## Funding and attribution

This work was supported by the Italian Ministry of University and Research (MUR) under the **PRIN 2020** programme, grant **CUP E43C22000360001**.

If you use this code, please cite the repository and the associated publication (when available).

---

## License

This repository is released under the [MIT License](LICENSE).

---

## Contact

**Eleonora Longo** — [@EleonoraLongo](https://github.com/EleonoraLongo) — [eleonora.longo@unitn.it](mailto:eleonora.longo@unitn.it)  
For questions regarding data access or the research project, please open an issue or contact the repository owner directly.
