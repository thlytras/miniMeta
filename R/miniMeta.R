#' Launch miniMeta in your browser
#'
#' This function lanuches miniMeta in your browser
#'
#' @import shiny
#' @import meta
#' 
#' @importFrom markdown markdownToHTML
#' @importFrom metafor forest
#'
#' @examples
#' \dontrun{
#' miniMeta()
#' }
#'
#' @export
miniMeta <- function() {
  appDir <- system.file("shiny", "www", package = "miniMeta")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `miniMeta`.", call. = FALSE)
  }
  addResourcePath("www", appDir)
  shinyApp(ui = miniMetaUI(), server = miniMetaServer)#, display.mode = "normal")
}
