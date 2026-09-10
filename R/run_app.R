#' Run the rehydra Shiny Application
#'
#' Launch the interactive Shiny interface bundled with `rehydra`.
#'
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return The Shiny app object invisibly after launch.
#'
#' @examples
#' \dontrun{
#' run_rehydra_app()
#' }
#' @export
run_rehydra_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to run the app.", call. = FALSE)
  }

  app_dir <- system.file("shiny", package = "rehydra")
  if (app_dir == "") {
    stop("Shiny application directory was not found in the package installation.", call. = FALSE)
  }

  invisible(shiny::runApp(appDir = app_dir, ...))
}
