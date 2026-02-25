# BEMPdata

**R package for the Bangladesh Environmental Mobility Panel**

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18775710.svg)](https://doi.org/10.5281/zenodo.18775710)
[![Shiny app](https://img.shields.io/badge/Shiny-Data%20Explorer-blue?logo=r)](https://b68gkn-janfreihardt.shinyapps.io/bemp-explorer/)
<!-- badges: end -->

`BEMPdata` provides functions to download and work with the
[Bangladesh Environmental Mobility Panel (BEMP)](https://doi.org/10.5281/zenodo.18229498),
a household panel survey on environmental migration along the Jamuna River in
Bangladesh (2021–2024). The dataset follows 1,691 households across 20 survey
datasets and 24,279 completed surveys.

Wave data are hosted on Zenodo and downloaded on demand (~6 MB for all CSVs).
Files are cached locally so subsequent calls are instant.

## Interactive data explorer

An interactive app for browsing the codebook, inspecting variable distributions,
and downloading data subsets is available at:

**<https://b68gkn-janfreihardt.shinyapps.io/bemp-explorer/>**

Or launch it locally after installing the package:

```r
BEMPdata::run_app()
```

## Installation

```r
# Install from GitHub
remotes::install_github("janfreihardt/BEMPdata")
```

## Quick start

```r
library(BEMPdata)

# See available waves
wave_overview

# Download the baseline wave
w1 <- get_wave("w1")

# Download the Wave 6 migrant questionnaire in Stata format
w6m <- get_wave("w6_M", format = "dta")

# Find variables by keyword
lookup_variable("income")

# Get the codebook for one wave
cb <- get_codebook("w1")

# Check / clear local cache
bemp_cache_info()
```

## Wave structure

| Round | Type | Files |
|-------|------|-------|
| 1 | In-person | `w1`, `w1_V` (village profile) |
| 2–5 | Phone | `w2` – `w5` |
| 6 | In-person | `w6_M` (migrant), `w6_N` (non-migrant) |
| 7–11 | Phone | `w7` – `w11` |
| 12 | In-person | `w12_M`, `w12_N`, `w12_V` |
| 13 | Phone | `w13` |
| 14 | In-person | `w14_M`, `w14_N`, `w14_V` |

## Data source

Freihardt, J. et al. (2026). *Panel data on (im)mobility, socio-economic, and
political impacts of riverbank erosion and flooding in Bangladesh* [Dataset].
Zenodo. <https://doi.org/10.5281/zenodo.18229498>

## Citation

If you use this package, please cite:

> Freihardt, J. (2026). *BEMPdata: R package for the Bangladesh Environmental Mobility Panel*. Zenodo. <https://doi.org/10.5281/zenodo.18775710>

For the underlying dataset:

> Freihardt, J. et al. (2026). *Panel data on (im)mobility, socio-economic, and political impacts of riverbank erosion and flooding in Bangladesh* [Dataset]. Zenodo. <https://doi.org/10.5281/zenodo.18229498>

## License

Package code: MIT. Dataset: CC BY 4.0 (see Zenodo deposit).
