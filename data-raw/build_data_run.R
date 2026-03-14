library(readr)

# ── wave_overview ─────────────────────────────────────────────────────────────
wave_overview <- data.frame(
  wave = c("w1","w1_V","w2","w3","w4","w5","w6_M","w6_N",
           "w7","w8","w9","w10","w11","w12_M","w12_N","w12_V",
           "w13","w14_M","w14_N","w14_V"),
  survey_round = c(1L,1L,2L,3L,4L,5L,6L,6L,7L,8L,9L,10L,11L,12L,12L,12L,13L,14L,14L,14L),
  type = c("in-person","in-person","phone","phone","phone","phone",
           "in-person","in-person","phone","phone","phone","phone","phone",
           "in-person","in-person","in-person","phone","in-person","in-person","in-person"),
  questionnaire = c("main","village profile","main","main","main","main",
                    "migrant","non-migrant","main","main","main","main","main",
                    "migrant","non-migrant","village profile","main",
                    "migrant","non-migrant","village profile"),
  stringsAsFactors = FALSE
)
save(wave_overview, file = "data/wave_overview.rda", compress = "bzip2")
cat("wave_overview.rda saved\n")

# ── codebook ──────────────────────────────────────────────────────────────────
cb_zip_url <- "https://zenodo.org/records/18229498/files/bemp_codebooks_as_csv.zip?download=1"
tmp_zip <- tempfile(fileext = ".zip")
tmp_dir <- tempfile()
dir.create(tmp_dir)

message("Downloading codebook zip from Zenodo...")
download.file(cb_zip_url, tmp_zip, mode = "wb", quiet = FALSE)
unzip(tmp_zip, exdir = tmp_dir)
unlink(tmp_zip)

cb_files <- list.files(tmp_dir, pattern = "_codebook\\.csv$",
                        full.names = TRUE, recursive = TRUE)
# Exclude desktop.ini / non-csv artefacts
cb_files <- cb_files[grepl("_codebook\\.csv$", cb_files)]
cat("Found", length(cb_files), "codebook files\n")

read_cb <- function(path) {
  df <- read_csv(path, show_col_types = FALSE,
                 col_types = readr::cols(.default = readr::col_character()))
  wave_id <- sub("^bemp_(.+)_codebook\\.csv$", "\\1", basename(path))
  wave_id <- tolower(wave_id)
  nms <- names(df)
  nms <- gsub(" ", "_", nms)
  # Preserve negative single-digit value codes as val_negX before stripping sign
  nms <- ifelse(grepl("^-[0-9]$", nms), paste0("val_neg", sub("^-", "", nms)), nms)
  nms <- gsub("[^A-Za-z0-9_]", "", nms)
  nms <- tolower(nms)
  nms <- ifelse(grepl("^[0-9]+$", nms), paste0("val_", nms), nms)
  names(df) <- nms
  df$wave <- wave_id
  df[, c("wave", setdiff(names(df), "wave"))]
}

tabs    <- lapply(cb_files, read_cb)
codebook <- dplyr::bind_rows(tabs)
cat("Codebook rows:", nrow(codebook), "\n")
save(codebook, file = "data/codebook.rda", compress = "bzip2")
cat("codebook.rda saved\n")
unlink(tmp_dir, recursive = TRUE)
cat("Done.\n")
