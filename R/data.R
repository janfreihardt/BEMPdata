#' Merged BEMP codebook across all waves
#'
#' A data frame containing the combined codebook for all 20 BEMP wave datasets.
#' Each row describes one variable in one wave. Built from the per-wave codebook
#' CSV files hosted on Zenodo; regenerate with `source("data-raw/build_data.R")`.
#'
#' @format A data frame with the following key columns:
#' \describe{
#'   \item{wave}{Wave identifier, e.g. `"w1"`, `"w6_m"`, `"w14_v"`.}
#'   \item{variable_name}{Variable name as it appears in the dataset.}
#'   \item{variable_label}{Short English label for the variable.}
#'   \item{block}{Thematic block within the questionnaire.}
#'   \item{question}{Short question description.}
#'   \item{question_type}{Question type (e.g. single-choice, numeric).}
#'   \item{question_text}{Full question text in English.}
#'   \item{question_text_bn}{Full question text in Bangla.}
#'   \item{appears_in_waves}{String listing all waves in which this variable
#'     appears, with observation counts.}
#'   \item{val_0, val_1, ...}{Value labels for coded response options.}
#' }
#'
#' @source Zenodo record 18229498, `bemp_codebooks_as_csv.zip`
"codebook"


#' Overview of BEMP survey waves
#'
#' A small reference table describing each of the 20 BEMP wave datasets:
#' survey round, mode (in-person / phone), and questionnaire type.
#'
#' @format A data frame with 20 rows and 4 columns:
#' \describe{
#'   \item{wave}{Wave identifier (character), e.g. `"w1"`, `"w6_M"`.}
#'   \item{survey_round}{Integer survey round number (1–14).}
#'   \item{type}{Survey mode: `"in-person"` or `"phone"`.}
#'   \item{questionnaire}{Questionnaire type: `"main"`, `"migrant"`,
#'     `"non-migrant"`, or `"village profile"`.}
#' }
#'
#' @examples
#' wave_overview
#' wave_overview[wave_overview$type == "in-person", ]
"wave_overview"
