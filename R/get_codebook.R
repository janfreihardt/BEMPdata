utils::globalVariables("codebook")

#' Get the codebook for one or all BEMP waves
#'
#' Returns the codebook for a specific wave, or the merged codebook across all
#' waves. Codebooks are downloaded from Zenodo on first use (~4 MB) and cached
#' locally. Alternatively, use the pre-built [codebook] object that ships with
#' the package for offline use.
#'
#' @param wave Character. Wave identifier (e.g. `"w1"`, `"w6_M"`) or `"all"`
#'   (default) to return the merged codebook for every wave. See [wave_overview].
#' @param refresh Logical. Re-download from Zenodo even if already cached.
#'   Default `FALSE`.
#'
#' @return A tibble with one row per variable. Key columns:
#'   - `wave`: wave identifier
#'   - `variable_name`, `variable_label`
#'   - `block`, `question`, `question_type`
#'   - `question_text`, `question_text_bn` (Bangla)
#'   - `appears_in_waves`: cross-wave appearance string
#'   - Value label columns (`val_0`, `val_1`, ...)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Codebook for the baseline wave
#' cb_w1 <- get_codebook("w1")
#'
#' # Merged codebook (all waves)
#' cb_all <- get_codebook()
#' }
get_codebook <- function(wave = "all", refresh = FALSE) {
  wave <- .normalise_wave(wave)

  if (wave != "all" && !wave %in% .valid_waves) {
    stop(
      "'", wave, "' is not a valid wave identifier.\n",
      "Valid values: ", paste(c("all", toupper(.valid_waves)), collapse = ", ")
    )
  }

  cb_subdir   <- "bemp_codebooks_as_csv"
  sentinel    <- file.path(.cache_dir(), cb_subdir, "bemp_w1_codebook.csv")

  if (!file.exists(sentinel) || refresh) {
    .download_and_unzip(
      url      = .zenodo_url("bemp_codebooks_as_csv.zip"),
      dest_dir = .cache_dir(),
      label    = "BEMP codebooks"
    )
  }

  if (wave == "all") {
    files <- list.files(
      file.path(.cache_dir(), cb_subdir),
      pattern   = "_codebook\\.csv$",
      full.names = TRUE
    )
    tabs <- lapply(files, .read_codebook_file)
    do.call(rbind, tabs)
  } else {
    wave_filename <- gsub("_([mnv])$", "_\\U\\1", wave, perl = TRUE)
    f <- file.path(.cache_dir(), cb_subdir,
                   paste0("bemp_", wave_filename, "_codebook.csv"))
    if (!file.exists(f)) {
      stop("Codebook file not found: ", f)
    }
    .read_codebook_file(f)
  }
}

# Read one codebook CSV and add a `wave` column with cleaned column names
.read_codebook_file <- function(path) {
  df <- readr::read_csv(path, show_col_types = FALSE)

  # Derive wave id from filename: bemp_w6_M_codebook.csv -> "w6_m"
  wave_id <- sub("^bemp_(.+)_codebook\\.csv$", "\\1", basename(path))
  wave_id <- tolower(wave_id)

  # Rename columns to snake_case where possible; leave value-code columns
  # (numeric names like "0", "1", ...) prefixed with "val_"
  old <- names(df)
  new <- old
  new <- gsub(" ", "_", new)
  new <- gsub("[^A-Za-z0-9_]", "", new)
  new <- tolower(new)
  new <- sub("^variable_name$",    "variable_name",     new)
  new <- sub("^variable_label$",   "variable_label",    new)
  new <- sub("^item_source$",      "item_source",       new)
  new <- sub("^appears_in_waves$", "appears_in_waves",  new)
  new <- sub("^question_type$",    "question_type",     new)
  new <- sub("^question_text$",    "question_text",     new)
  new <- sub("^question_text_bn$", "question_text_bn",  new)
  new <- sub("^item_text_bn$",     "item_text_bn",      new)
  # Prefix purely numeric column names (value codes)
  new <- ifelse(grepl("^[0-9]+$", new), paste0("val_", new), new)
  names(df) <- new

  df$wave <- wave_id
  # Move wave column to front
  df[, c("wave", setdiff(names(df), "wave"))]
}


#' Search for variables across all BEMP waves
#'
#' Searches the bundled [codebook] for variables whose name, label, or
#' question text matches a pattern. Useful for finding a variable when you
#' know a keyword but not the exact variable name.
#'
#' @param pattern Character. A regular expression (case-insensitive). For
#'   simple keyword searches just supply a word, e.g. `"income"`.
#' @param fields Character vector. Which fields to search. One or more of
#'   `"name"`, `"label"`, `"question"`. Default: all three.
#'
#' @return A tibble with columns `wave`, `variable_name`, `variable_label`,
#'   `block`, and `question` for all matching variables, sorted by wave.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Find all income-related variables
#' lookup_variable("income")
#'
#' # Search only variable labels for migration-related items
#' lookup_variable("migrat", fields = "label")
#' }
lookup_variable <- function(pattern,
                             fields = c("name", "label", "question")) {
  fields <- match.arg(fields, choices = c("name", "label", "question"),
                      several.ok = TRUE)

  cb <- codebook

  col_map <- c(
    name     = "variable_name",
    label    = "variable_label",
    question = "question"
  )

  cols_to_search <- col_map[fields]
  # Keep only columns that actually exist in the bundled codebook
  cols_to_search <- cols_to_search[cols_to_search %in% names(cb)]

  if (length(cols_to_search) == 0) {
    stop("None of the requested fields found in the codebook.")
  }

  mask <- Reduce(`|`, lapply(cols_to_search, function(col) {
    grepl(pattern, cb[[col]], ignore.case = TRUE, perl = TRUE)
  }))

  out_cols <- intersect(
    c("wave", "variable_name", "variable_label", "block", "question"),
    names(cb)
  )
  cb[mask, out_cols, drop = FALSE]
}
