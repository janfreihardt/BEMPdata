library(shiny)
library(bslib)
enableBookmarking("url")
library(DT)
library(ggplot2)
library(dplyr)
library(BEMPdata)

# ── Bundled reference data ────────────────────────────────────────────────────
cb <- BEMPdata::codebook
wo <- BEMPdata::wave_overview

# ── Derived lookups ───────────────────────────────────────────────────────────

# Wave choices for UI: display as "w6_M  [in-person, migrant]"
wave_choices <- setNames(
  wo$wave,
  paste0(wo$wave, "  [", wo$type, ", ", wo$questionnaire, "]")
)

# All thematic blocks (for codebook browser filter)
all_blocks <- sort(unique(cb$block[!is.na(cb$block) & nzchar(cb$block)]))

# Waves where -55 / -66 special missing codes are used (from codebook)
waves_with_55 <- unique(cb$wave[which(nchar(cb$val_55) > 0)])
waves_with_66 <- unique(cb$wave[which(nchar(cb$val_66) > 0)])

# Return the variable_name for the resp_type column of a given wave, or NULL
get_resp_type_var <- function(wave_id) {
  rows <- cb[cb$wave == tolower(wave_id) &
               grepl("resp_type", cb$variable_label, ignore.case = TRUE), ]
  if (nrow(rows) == 0) return(NULL)
  rows$variable_name[1]
}

# Strip wave prefix from variable labels (e.g. "w1_happy" → "happy")
strip_wave_prefix <- function(x) {
  sub("^w[0-9]+(_[mnv])?_", "", x, ignore.case = TRUE)
}

# Build variable dropdown label: "var_name: var_label[-item_text] (Question title)"
make_var_choice_label <- function(var_name, var_label, item_text, question) {
  item_sfx <- ifelse(
    grepl("[0-9]$", strip_wave_prefix(var_label)) &
      !is.na(item_text) & nzchar(item_text),
    paste0("-", item_text), ""
  )
  q_part <- ifelse(!is.na(question) & nzchar(question),
                   paste0(" (", question, ")"), "")
  paste0(var_name, ": ", var_label, item_sfx, q_part)
}

# Natural sort key: pad digit runs so "sourc2" < "sourc10" (not "sourc10" < "sourc2")
nat_sort_key <- function(x) {
  sapply(x, function(s) {
    parts <- strsplit(s, "(?<=[0-9])(?=[^0-9])|(?<=[^0-9])(?=[0-9])", perl = TRUE)[[1]]
    paste(ifelse(grepl("^[0-9]+$", parts),
                 formatC(as.integer(parts), width = 10, flag = "0"),
                 parts),
          collapse = "")
  }, USE.NAMES = FALSE)
}

# Chronological wave sort order (N before M within each round, V last)
wave_sort_order <- c(
  "w1", "w1_v",
  "w2", "w3", "w4", "w5",
  "w6_n", "w6_m",
  "w7", "w8", "w9", "w10", "w11",
  "w12_n", "w12_m", "w12_v",
  "w13",
  "w14_n", "w14_m", "w14_v"
)

# Map lowercase wave IDs → display labels with proper case (e.g. "w6_n" → "w6_N")
wave_label_map <- setNames(wo$wave, tolower(wo$wave))

# Waves eligible for multi-wave merge (individual-level: have a resp_code ID variable)
mw_eligible    <- unique(cb$wave[grepl("resp_code$", cb$variable_label)])
mw_wave_choices <- wave_choices[tolower(wave_choices) %in% mw_eligible]

# Look up resp_code variable name for a given wave
get_resp_code_var <- function(wave_id) {
  rows <- cb[cb$wave == tolower(wave_id) & grepl("resp_code$", cb$variable_label), ]
  if (nrow(rows) == 0) return(NULL)
  rows$variable_name[1]
}

# Parse appears_in_waves string → named character vector  wave_id → variable_name
# e.g. "w1_q7 (n = 2012), w2_q67 (n = 1543)" → c(w1 = "w1_q7", w2 = "w2_q67")
parse_appears <- function(appears_str) {
  if (is.na(appears_str) || !nzchar(appears_str)) return(character(0))
  parts    <- trimws(strsplit(appears_str, ",\\s*")[[1]])
  var_names <- trimws(sub("\\s*\\(.*\\)$", "", parts))
  # Derive wave_id from variable_name prefix (e.g. "w6_m_q6" → "w6_m")
  wave_ids  <- sub("_q[0-9].*$", "", tolower(var_names))
  setNames(var_names, wave_ids)
}

# Variables tracked across ≥ 2 waves (one representative row per concept)
tracked_vars <- local({
  has_appears <- !is.na(cb$appears_in_waves) & nzchar(cb$appears_in_waves)
  multi_wave  <- has_appears &
    lengths(regmatches(cb$appears_in_waves,
                       gregexpr(",", cb$appears_in_waves))) >= 1
  sub_cb <- cb[multi_wave, ]
  sub_cb <- sub_cb[!duplicated(sub_cb$appears_in_waves), ]
  # Drop rows with no real label (label == variable name, or empty/NA)
  has_label <- !is.na(sub_cb$variable_label) &
    nzchar(sub_cb$variable_label) &
    sub_cb$variable_label != sub_cb$variable_name
  sub_cb <- sub_cb[has_label, ]
  sub_cb[order(nat_sort_key(sub_cb$variable_label)), ]
})


# Choices for the Panel Dynamics variable selector
# Label: "Question title (clean_label, N waves)"
pd_choices <- setNames(
  tracked_vars$appears_in_waves,
  {
    q_titles <- ifelse(
      "question" %in% names(tracked_vars) &
        !is.na(tracked_vars$question) & nzchar(tracked_vars$question),
      tracked_vars$question,
      tracked_vars$variable_label
    )
    n_waves <- lengths(regmatches(tracked_vars$appears_in_waves,
                                  gregexpr(",", tracked_vars$appears_in_waves))) + 1L
    clean_labels <- strip_wave_prefix(tracked_vars$variable_label)
    item_texts <- ifelse(
      grepl("[0-9]$", clean_labels) &
        "item_text" %in% names(tracked_vars) &
        !is.na(tracked_vars$item_text) & nzchar(tracked_vars$item_text),
      tracked_vars$item_text,
      NA_character_
    )
    inner <- ifelse(!is.na(item_texts),
                    paste0(clean_labels, ", ", item_texts),
                    clean_labels)
    paste0(q_titles, "  (", inner, ", ", n_waves, " waves)")
  }
)

# ── Helper functions ──────────────────────────────────────────────────────────

# Return value-label data frame for a variable (code, label columns)
# val_55 / val_66 are stored with the minus stripped from the CSV column name
# "-55"/"-66" but represent actual data codes -55 / -66.
SPECIAL_NEG <- c(val_55 = -55L, val_66 = -66L)

get_value_labels <- function(wave_id, var_name) {
  row <- cb[cb$wave == tolower(wave_id) &
              cb$variable_name == var_name, , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  all_pos  <- grep("^val_[0-9]+$",    names(row), value = TRUE)
  neg_cols <- grep("^val_neg[0-9]+$", names(row), value = TRUE)
  # Columns that look positive but are actually special negatives (-55, -66)
  spec_cols <- intersect(all_pos, names(SPECIAL_NEG))
  pos_cols  <- setdiff(all_pos, spec_cols)
  if (length(pos_cols) == 0 && length(neg_cols) == 0 &&
      length(spec_cols) == 0) return(NULL)
  codes  <- c(as.integer(sub("val_", "", pos_cols)),
              SPECIAL_NEG[spec_cols],
              -as.integer(sub("val_neg", "", neg_cols)))
  labels <- as.character(c(unlist(row[1, pos_cols]),
                            unlist(row[1, spec_cols]),
                            unlist(row[1, neg_cols])))
  out <- data.frame(code = codes, label = labels,
                    stringsAsFactors = FALSE, row.names = NULL)
  out[!is.na(out$label) & nzchar(out$label), ]
}

# Return codebook row(s) for a variable
get_cb_row <- function(wave_id, var_name) {
  cb[cb$wave == tolower(wave_id) & cb$variable_name == var_name, , drop = FALSE]
}

# Shared ggplot2 theme
bemp_theme <- function() {
  theme_minimal(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", size = 11),
      plot.subtitle    = element_text(color = "grey40", size = 10),
      axis.text.x      = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      legend.position  = "none"
    )
}

BEMP_BLUE <- "#2166ac"

# Fixed respondent-type group labels keyed by numeric code
RESP_TYPE_LABELS <- c(
  "1" = "Household head",
  "2" = "Youth",
  "3" = "Female",
  "4" = "Left-behind member of migrated respondent"
)

# HTML-escape text, then wrap matches of `term` in <mark> tags
hl <- function(x, term) {
  x <- htmltools::htmlEscape(as.character(x))
  if (!nzchar(trimws(term))) return(x)
  # Escape special regex characters in the search term
  pattern <- paste0("(", gsub("([.+*?^${}()|\\[\\]\\\\])", "\\\\\\1", term), ")")
  gsub(pattern,
       '<mark style="background:#fff3cd;padding:0 1px">\\1</mark>',
       x, ignore.case = TRUE, perl = TRUE)
}
