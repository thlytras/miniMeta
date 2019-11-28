 
# based on the Shiny fileInput function
miniFileInput <- function(inputId, label, labelIcon = NULL, accept = NULL, width = NULL) {
  inputTag <- tags$input(id = inputId, name = inputId, type = "file", style = "display:none;")
  if (length(accept) > 0) inputTag$attribs$accept <- paste(accept, collapse = ",")
  div(style = paste("display:inline-block;",
        if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";")),
    inputTag,
    tags$label(
      `for` = inputId, div(icon("folder-open-o"), label, 
      class = "btn btn-default action-button")
    )
  )
}

