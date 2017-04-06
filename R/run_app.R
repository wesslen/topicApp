#' @export
runExample <- function() {
  appDir <- system.file("shiny", "topicApp", package = "topicApp")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `topicApp`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
