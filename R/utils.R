# Internal helpers: Zenodo URLs, cache management, download ----------------

.bemp_zenodo_record <- "18229498"
.bemp_zenodo_base   <- paste0(
  "https://zenodo.org/records/", .bemp_zenodo_record, "/files"
)

# All valid wave identifiers
.valid_waves <- c(
  "w1", "w1_v",
  "w2", "w3", "w4", "w5",
  "w6_m", "w6_n",
  "w7", "w8", "w9", "w10", "w11",
  "w12_m", "w12_n", "w12_v",
  "w13",
  "w14_m", "w14_n", "w14_v"
)

.zenodo_url <- function(filename) {
  paste0(.bemp_zenodo_base, "/", filename, "?download=1")
}

.cache_dir <- function() {
  d <- tools::R_user_dir("BEMPdata", which = "cache")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}

.download_and_unzip <- function(url, dest_dir, label) {
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  message("Downloading ", label, " from Zenodo (one-time download)...")
  tmp <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp), add = TRUE)
  utils::download.file(url, tmp, quiet = FALSE, mode = "wb")
  message("Extracting...")
  utils::unzip(tmp, exdir = dest_dir)
  invisible(dest_dir)
}

# Normalise user-supplied wave string: accept "W6_M", "w6_m", "w6M", etc.
.normalise_wave <- function(wave) {
  w <- tolower(trimws(wave))
  # Accept "w6M" -> "w6_m" style without underscore
  w <- gsub("^(w[0-9]+)([mnv])$", "\\1_\\2", w)
  w
}
