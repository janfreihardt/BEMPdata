#' Show information about the local BEMPdata cache
#'
#' Reports the location and size of the local cache directory where downloaded
#' BEMP files are stored.
#'
#' @return Invisibly returns the cache directory path. Called for its
#'   side effect of printing cache information.
#'
#' @export
#'
#' @examples
#' bemp_cache_info()
bemp_cache_info <- function() {
  d <- tools::R_user_dir("BEMPdata", which = "cache")

  if (!dir.exists(d)) {
    message("Cache directory does not exist yet (no data downloaded).")
    message("It will be created at: ", d)
    return(invisible(d))
  }

  files <- list.files(d, recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    message("Cache is empty: ", d)
    return(invisible(d))
  }

  total_bytes <- sum(file.info(files)$size, na.rm = TRUE)
  total_mb    <- round(total_bytes / 1024^2, 1)

  subdirs <- list.dirs(d, full.names = FALSE, recursive = FALSE)

  message("BEMPdata cache directory: ", d)
  message("Total size: ", total_mb, " MB (", length(files), " files)")
  message("Contents:")
  for (s in subdirs) {
    sf <- list.files(file.path(d, s))
    message("  ", s, "/ (", length(sf), " files)")
  }

  invisible(d)
}


#' Clear the local BEMPdata cache
#'
#' Deletes all locally cached BEMP files. The next call to [get_wave()] or
#' [get_codebook()] will re-download from Zenodo.
#'
#' @param confirm Logical. Set to `TRUE` to skip the interactive confirmation
#'   prompt (useful in scripts). Default `FALSE`.
#'
#' @return Invisibly returns `TRUE` if the cache was cleared, `FALSE` if
#'   the user declined.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' bemp_cache_clear(confirm = TRUE)
#' }
bemp_cache_clear <- function(confirm = FALSE) {
  d <- tools::R_user_dir("BEMPdata", which = "cache")

  if (!dir.exists(d)) {
    message("Cache directory does not exist; nothing to clear.")
    return(invisible(FALSE))
  }

  if (!confirm) {
    ans <- readline(paste0(
      "This will delete all cached BEMP files in:\n  ", d,
      "\nProceed? [y/N] "
    ))
    if (!tolower(trimws(ans)) %in% c("y", "yes")) {
      message("Cancelled.")
      return(invisible(FALSE))
    }
  }

  unlink(d, recursive = TRUE)
  message("Cache cleared.")
  invisible(TRUE)
}
