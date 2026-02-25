# data-raw/build_data.R
# Run this script once (source("data-raw/build_data.R") from the package root)
# to build the bundled data objects in data/.
#
# Requires: readr, usethis

library(readr)

# ── 1. wave_overview ──────────────────────────────────────────────────────────

wave_overview <- data.frame(
  wave = c(
    "w1", "w1_V",
    "w2", "w3", "w4", "w5",
    "w6_M", "w6_N",
    "w7", "w8", "w9", "w10", "w11",
    "w12_M", "w12_N", "w12_V",
    "w13",
    "w14_M", "w14_N", "w14_V"
  ),
  survey_round = c(
    1L, 1L,
    2L, 3L, 4L, 5L,
    6L, 6L,
    7L, 8L, 9L, 10L, 11L,
    12L, 12L, 12L,
    13L,
    14L, 14L, 14L
  ),
  type = c(
    "in-person", "in-person",
    "phone", "phone", "phone", "phone",
    "in-person", "in-person",
    "phone", "phone", "phone", "phone", "phone",
    "in-person", "in-person", "in-person",
    "phone",
    "in-person", "in-person", "in-person"
  ),
  questionnaire = c(
    "main", "village profile",
    "main", "main", "main", "main",
    "migrant", "non-migrant",
    "main", "main", "main", "main", "main",
    "migrant", "non-migrant", "village profile",
    "main",
    "migrant", "non-migrant", "village profile"
  ),
  stringsAsFactors = FALSE
)

usethis::use_data(wave_overview, overwrite = TRUE)


# ── 2. codebook ───────────────────────────────────────────────────────────────
# Download the codebook zip from Zenodo, read all CSVs, combine.

zenodo_base <- "https://zenodo.org/records/18229498/files"
cb_zip_url  <- paste0(zenodo_base, "/bemp_codebooks_as_csv.zip?download=1")

tmp_zip <- tempfile(fileext = ".zip")
tmp_dir <- tempfile()
dir.create(tmp_dir)
on.exit({ unlink(tmp_zip); unlink(tmp_dir, recursive = TRUE) }, add = TRUE)

message("Downloading codebook zip from Zenodo...")
download.file(cb_zip_url, tmp_zip, mode = "wb")
unzip(tmp_zip, exdir = tmp_dir)

cb_files <- list.files(tmp_dir, pattern = "_codebook\\.csv$",
                        full.names = TRUE, recursive = TRUE)

read_cb <- function(path) {
  df <- read_csv(path, show_col_types = FALSE,
                 col_types = readr::cols(.default = readr::col_character()))

  wave_id <- sub("^bemp_(.+)_codebook\\.csv$", "\\1", basename(path))
  wave_id <- tolower(wave_id)

  # Rename columns to snake_case
  nms <- names(df)
  nms <- gsub(" ", "_", nms)
  nms <- gsub("[^A-Za-z0-9_]", "", nms)
  nms <- tolower(nms)
  # Prefix purely numeric column names (value codes)
  nms <- ifelse(grepl("^[0-9]+$", nms), paste0("val_", nms), nms)
  names(df) <- nms

  df$wave <- wave_id
  df[, c("wave", setdiff(names(df), "wave"))]
}

codebook <- dplyr::bind_rows(lapply(cb_files, read_cb))

usethis::use_data(codebook, overwrite = TRUE)

message("Done. Bundled data objects written to data/")
