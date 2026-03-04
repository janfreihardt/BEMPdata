# BEMPdata 0.2.1

* Address CRAN review feedback: replace `\dontrun{}` with `\donttest{}` for
  download functions (`get_wave()`, `get_codebook()`), `if (interactive())`
  for interactive functions (`run_app()`, `bemp_cache_clear()`), and unwrap
  `lookup_variable()` examples (uses only bundled data).
* Add Zenodo URL to `DESCRIPTION`.
* Fix `get_codebook(wave = "all")` error when wave codebooks have differing
  numbers of value-label columns.

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
