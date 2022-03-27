#'Run a shiny appliaction demonstrating shiny functionality in RShinyHelpers
#' 
#' @examples
#' if (interactive()) {
#' demoApp()
#' }
#' @export
demoApp <- function() {
  appDir <- system.file("shiny-examples", "testing_app", package = "RShinyHelpers")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `RShinyHelpers`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
