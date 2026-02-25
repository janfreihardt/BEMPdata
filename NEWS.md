# BEMPdata 0.2.0

* Add interactive Shiny data explorer (`run_app()`), deployed at
  <https://b68gkn-janfreihardt.shinyapps.io/bemp-explorer/>.
  Includes a codebook browser, variable inspector, and download assistant.

# BEMPdata 0.1.0

* Initial release.
* `get_wave()`: download any of the 20 BEMP wave datasets from Zenodo with local caching.
* `get_codebook()`: download per-wave or merged codebook.
* `lookup_variable()`: keyword search across the bundled cross-wave codebook.
* `bemp_cache_info()` / `bemp_cache_clear()`: cache management.
* Bundled data: `wave_overview` (wave metadata) and `codebook` (merged codebook, 32,248 rows).
