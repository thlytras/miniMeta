dropdownMenu <- function(label=NULL, icon=NULL, menu=NULL, style=NULL) {
  ul <- lapply(names(menu), function(id) {
    if (is.character(menu[[id]])) {
      tags$li(actionLink(id, menu[[id]]), style="padding: 0.5em 0 0.5em 0")
    } else {
      args <- menu[[id]]
      args$inputId <- id
      tags$li(do.call(actionLink, args), style="padding: 0.5em 0 0.5em 0")
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

# Here's an example:
#dropdownMenu(label = "Preferences", icon = icon("bars"),
#  menu = list(edit = "edit item", 
#    rename = list(label = "address", icon = icon("id-card"))))
