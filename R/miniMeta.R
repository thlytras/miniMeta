#' Launch miniMeta in your browser
#'
#' This function lanuches miniMeta in your browser
#'
#' @import shiny
#' @import meta
#'
#' @export
miniMeta <- function() {
  appDir <- system.file("shiny", "miniMeta", package = "miniMeta")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `miniMeta`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
