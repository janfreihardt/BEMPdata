#' Launch the BEMP Data Explorer Shiny app
#'
#' Opens an interactive data explorer with three tabs:
#' \describe{
#'   \item{Codebook Browser}{Search and filter variables across all waves by
#'     keyword, thematic block, or wave. Click any row to see the full question
#'     text and value labels.}
#'   \item{Variable Inspector}{Select a wave and variable to view its
#'     distribution (bar chart for categorical, histogram for numeric) and
#'     summary statistics.}
#'   \item{Download Assistant}{Select a wave and a subset of variables,
#'     preview the data, and download as CSV.}
#' }
#'
#' @param ... Additional arguments passed to [shiny::runApp()], e.g.
#'   `port = 4321` or `launch.browser = FALSE`.
#'
#' @return Called for its side effect of launching a Shiny app.
#' @export
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
run_app <- function(...) {
  app_dir <- system.file("shiny", "bemp_explorer", package = "BEMPdata")
  if (!nzchar(app_dir)) {
    stop("Could not find the app directory. Try reinstalling BEMPdata.")
  }
  shiny::runApp(app_dir, ...)
}
