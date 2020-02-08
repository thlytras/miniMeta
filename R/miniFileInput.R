#' File Upload Shiny UI control
#'
#' Based on the Shiny \code{\link[shiny]{fileInput}} function. 
#' This is similar, but shows only a button and no progress bar.
#' Hence the "mini" in the name.
#' 
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param label Display label for the control, or \code{NULL} for no label.
#' @param accept A character vector of MIME types; gives the browser 
#'     a hint of what kind of files the server is expecting.
#' @param width The width of the input, e.g. \code{'400px'}, or \code{'100\%'}; 
#'     see \code{\link[shiny]{validateCssUnit()}}.
#' 
#' @import shiny
#'
#' @keywords internal
#' @noRd
miniFileInput <- function(inputId, label, accept = NULL, width = NULL) {
  inputTag <- tags$input(id = inputId, name = inputId, type = "file", style = "display:none;")
  if (length(accept) > 0) inputTag$attribs$accept <- paste(accept, collapse = ",")
  div(style = paste("display:inline-block;", 
        if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";")),
    inputTag,
    tags$label(
      `for` = inputId,
      div(icon("folder-open-o"), label, 
      class = "btn btn-default action-button")
    )
  )
}

