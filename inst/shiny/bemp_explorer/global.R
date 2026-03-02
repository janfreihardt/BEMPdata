library(shiny)
library(bslib)
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

# ── Helper functions ──────────────────────────────────────────────────────────

# Return value-label data frame for a variable (code, label columns)
get_value_labels <- function(wave_id, var_name) {
  row <- cb[cb$wave == tolower(wave_id) &
              cb$variable_name == var_name, , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  val_cols <- grep("^val_[0-9]+$", names(row), value = TRUE)
  if (length(val_cols) == 0) return(NULL)
  codes  <- as.integer(sub("val_", "", val_cols))
  labels <- as.character(unlist(row[1, val_cols]))
  out <- data.frame(code = codes, label = labels, stringsAsFactors = FALSE)
  out[!is.na(out$label) & nzchar(out$label), ]
}

# Return codebook row(s) for a variable
get_cb_row <- function(wave_id, var_name) {
  cb[cb$wave == tolower(wave_id) & cb$variable_name == var_name, , drop = FALSE]
}

# Shared ggplot2 theme
bemp_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "grey40"),
      axis.text.x   = element_text(angle = 30, hjust = 1),
      panel.grid.minor = element_blank()
    )
}

BEMP_BLUE <- "#2166ac"
