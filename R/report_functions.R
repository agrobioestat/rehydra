#' Generate an Automated rehydra Report
#'
#' Render an HTML, Word, or PDF report summarizing the full drought-memory
#' analysis.
#'
#' @param analysis A `rehydra_analysis` object, usually from
#'   [summarize_rehydra()].
#' @param output_format Report format: `"html"`, `"word"`, or `"pdf"`.
#' @param output_dir Output directory. Defaults to [tempdir()] for
#'   CRAN-friendly behavior.
#' @param output_file Optional output file name.
#' @param quiet Logical; passed to `rmarkdown::render()`.
#'
#' @details
#' The report includes:
#'
#' - data summary and quality checks
#' - cycle segmentation summary
#' - trajectory and index plots
#' - damage, recovery, resilience, and memory tables
#' - priming classification and genotype comparisons
#' - short interpretation and references
#'
#' @return Path to the rendered report file.
#'
#' @examples
#' \dontrun{
#' data(rehydra_data)
#' result <- summarize_rehydra(
#'   data = rehydra_data,
#'   time = time_days,
#'   genotype = genotype,
#'   treatment = treatment,
#'   replicate = replicate,
#'   variables = c(water_potential, stomatal_conductance)
#' )
#' rehydra_report(result, output_format = "html")
#' }
#' @export
rehydra_report <- function(
    analysis,
    output_format = c("html", "word", "pdf"),
    output_dir = tempdir(),
    output_file = NULL,
    quiet = TRUE
) {
  output_format <- match.arg(output_format)

  if (!inherits(analysis, "rehydra_analysis")) {
    rlang::abort("`analysis` must be a `rehydra_analysis` object from summarize_rehydra().")
  }

  .ensure_suggested("rmarkdown")

  template <- system.file(
    "rmarkdown/templates/rehydra_report/skeleton/skeleton.Rmd",
    package = "rehydra"
  )

  if (template == "") {
    rlang::abort("Report template was not found in the installed package.")
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  format_name <- switch(
    output_format,
    html = "html_document",
    word = "word_document",
    pdf = "pdf_document"
  )

  if (is.null(output_file)) {
    output_file <- paste0("rehydra_report.", ifelse(output_format == "word", "docx", output_format))
  }

  render_path <- rmarkdown::render(
    input = template,
    output_format = format_name,
    output_dir = output_dir,
    output_file = output_file,
    params = list(analysis = analysis),
    envir = new.env(parent = baseenv()),
    quiet = quiet
  )

  normalizePath(render_path, winslash = "/", mustWork = FALSE)
}
