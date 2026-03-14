# CRAN submission comments — BEMPdata 0.2.0

## Test environments

* Windows 11, R 4.5.2 (local)
* win-builder, R 4.5.2 Patched (2026-02-25)
* win-builder, R Under development (unstable) (2026-02-25)

## R CMD CHECK results

0 errors | 0 warnings | 1 note

* checking for future file timestamps: unable to verify current time
  (transient network issue in the check environment; not reproducible locally)

## Possibly misspelled words in DESCRIPTION

All flagged words are intentional:

* *BEMP*: acronym for the Bangladesh Environmental Mobility Panel (the survey name)
* *Jamuna*: Bangladeshi name for the Brahmaputra River (standard usage)
* *Zenodo*: the data repository where the datasets are hosted
* *codebook*: standard social-science term for a variable documentation file
* *im* (part of *(im)mobility*): standard abbreviation used in migration literature
* *socio* (part of *socio-economic*): standard English prefix

## Notes on internet access

The core functions `get_wave()` and `get_codebook()` download data from a
permanent Zenodo repository (DOI: 10.5281/zenodo.18229497). All examples using
these functions are wrapped in `\dontrun{}`. All tests that require internet
access use `skip_on_cran()` and `skip_if_offline()`. The bundled datasets
(`codebook`, `wave_overview`) are fully functional offline.

## Reverse dependencies

None (initial submission).
