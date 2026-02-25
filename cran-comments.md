# CRAN submission comments — BEMPdata 0.1.0

## Test environments

* Windows 11, R 4.5.2 (local)
* Ubuntu (via GitHub Actions / R-hub)

## R CMD CHECK results

0 errors | 0 warnings | 0 notes

## Notes on internet access

The core functions `get_wave()` and `get_codebook()` download data from a
permanent Zenodo repository (DOI: 10.5281/zenodo.18229498). All examples using
these functions are wrapped in `\dontrun{}`. All tests that require internet
access use `skip_on_cran()` and `skip_if_offline()`. The bundled datasets
(`codebook`, `wave_overview`) are fully functional offline.

## Reverse dependencies

None (initial submission).
