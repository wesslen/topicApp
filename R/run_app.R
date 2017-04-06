#' @export
runApp <- function() {
  appDir <- system.file("app", package = "topicApp")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `topicApp`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
