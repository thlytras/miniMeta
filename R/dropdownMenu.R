#' Dropdown menu Shiny UI control
#'
#' Creates a dropdown menu. The items correspond to \code{input} elements
#' and can be used in \code{\link[shiny]{observeEvent}} blocks to trigger
#' various actions
#' 
#' @param label Optional label for the dropdown menu
#' @param icon Optional icon for the dropdown menu
#' @param menu A named list of items. Each element can be either a simple
#'     character vector, or a list with elements \code{label} and \code{icon}.
#'     See example.
#' @param style CSS passed to the enclosing \code{div}
#' 
#' @import shiny
#'
#' @examples
#' \donttest{
#' dropdownMenu(label = "Preferences", icon = icon("bars"), 
#'   menu = list(
#'     edit = "edit item", 
#'     rename = list(label = "address", icon = icon("id-card"))
#'   )
#' )
#' }
#'
#' @keywords internal
#' @noRd
dropdownMenu <- function(label=NULL, icon=NULL, menu=NULL, style=NULL) {
  ul <- lapply(names(menu), function(id) {
    if (is.character(menu[[id]])) {
      tags$li(actionLink(id, menu[[id]]), style="padding: 0.5em 0 0.5em 0")
    } else {
      args <- menu[[id]]
      args$inputId <- id
      if ("span" %in% names(args)) {
        tags$li(span(args$span, class="action-button"), style="padding: 0.5em 0 0.5em 0")
      } else {
        tags$li(do.call(actionLink, args), style="padding: 0.5em 0 0.5em 0")
      }
    }
  })
  ul$class <- "dropdown-menu dropdown-menu-right"
  tags$div(
    class = "dropdown",
    style = style,
    tags$button(
      class = "btn btn-default dropdown-toggle",
      type = "button",
      `data-toggle` = "dropdown",
      `if`(!is.null(icon), icon, tags$span(class="caret")),
      label
    ),
    do.call(tags$ul, ul)
  )
}

