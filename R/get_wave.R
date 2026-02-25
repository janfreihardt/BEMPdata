#' Download and return a BEMP wave dataset
#'
#' Downloads the requested wave dataset from Zenodo on first use and returns
#' it as a tibble. All wave files share a single zip archive per format, so
#' the first call downloads every wave at once (~6 MB for CSV, ~14 MB for
#' Stata); subsequent calls are instant because files are cached locally.
#'
#' @param wave Character. Wave identifier. Use lowercase with an underscore
#'   suffix for migrant (`_M`), non-migrant (`_N`), or village-profile (`_V`)
#'   sub-questionnaires. Examples: `"w1"`, `"w6_M"`, `"w12_N"`, `"w14_V"`.
#'   See [wave_overview] for the full list.
#' @param format Character. `"csv"` (default) or `"dta"` (Stata). CSV returns
#'   a [tibble][readr::read_csv]; DTA returns a labelled tibble via
#'   [haven::read_dta] with value labels attached.
#' @param refresh Logical. Re-download from Zenodo even if already cached.
#'   Default `FALSE`.
#'
#' @return A tibble with one row per survey respondent.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Baseline in-person wave
#' w1 <- get_wave("w1")
#'
#' # Wave 6, migrant questionnaire (accepts upper or lower case)
#' w6m <- get_wave("w6_M")
#'
#' # Village profile, Wave 14, in Stata format with value labels
#' w14v <- get_wave("w14_V", format = "dta")
#' }
get_wave <- function(wave, format = "csv", refresh = FALSE) {
  wave   <- .normalise_wave(wave)
  format <- match.arg(format, c("csv", "dta"))

  if (!wave %in% .valid_waves) {
    stop(
      "'", wave, "' is not a valid wave identifier.\n",
      "Valid values: ", paste(toupper(.valid_waves), collapse = ", "),
      "\nSee ?wave_overview for details."
    )
  }

  zip_name  <- paste0("bemp_quantitative_data_as_", format, ".zip")
  zip_subdir <- paste0("bemp_quantitative_data_as_", format)
  file_name <- paste0("bemp_", wave, ".", format)

  cached_path <- file.path(.cache_dir(), zip_subdir, file_name)

  if (!file.exists(cached_path) || refresh) {
    .download_and_unzip(
      url      = .zenodo_url(zip_name),
      dest_dir = .cache_dir(),
      label    = paste0("BEMP wave data (all waves, ", toupper(format), ")")
    )
  }

  if (!file.exists(cached_path)) {
    stop(
      "Expected file not found after extraction:\n  ", cached_path,
      "\nIf the problem persists, try get_wave(..., refresh = TRUE) or ",
      "file an issue at https://github.com/janfreihardt/BEMPdata/issues"
    )
  }

  if (format == "csv") {
    readr::read_csv(cached_path, show_col_types = FALSE)
  } else {
    haven::read_dta(cached_path)
  }
}
