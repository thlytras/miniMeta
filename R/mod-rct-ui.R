#' UI function for the RCT module 
#'
#' Note that the UI parts common with the observational studies module
#' have been split off into helper functions (with "ui_" prefix)
#' found in the \code{components-modUniv-ui.R} script.
#'
#' @param id Module id
#' 
#' @import shiny
#' @import shinyWidgets
#' 
#' @keywords internal
#' @noRd
rct_moduleUI <- function(id) {
  ns <- NS(id)
  tabPanel("RCT module",
    ui_importer(ns),
    sidebarLayout(
      sidebarPanel(
        rctLoadDataUI(id = ns("loadData")),
        prettySwitch(ns("showOptions"), "Show analysis options", status="primary"),
        conditionalPanel(sprintf("input['%s']", ns("showOptions")),
          wellPanel(
            fluidRow(
              column(6, ui_effectMeasure_select(ns)),
              column(6,
                awesomeCheckbox(ns("opt_combFixed"), "Use fixed-effects model", value=FALSE),
                awesomeCheckbox(ns("opt_combRandom"), "Use random-effects model", value=TRUE)
              )
            ),
            fluidRow(
              column(6, selectInput(ns("opt_method"), "Method for pooling",
                c("Mantel-Hanszel"="MH", "Inverse variance"="Inverse", 
                "Peto method"="Peto"))),
              column(6, ui_heterogeneity_select(ns))
            ),
            fluidRow(
              column(6,
                selectInput(ns("opt_incr"), "Continuity correction for blank cells", c("TACC", "0.5"))
              ),
              column(6,
                awesomeCheckbox(ns("opt_hakn"), "Hartung and Knapp correction", value=FALSE)
              )
            )
          )
        ),
      width=6),

      ui_mainPanel(ns, mtype = 1)
    )
  )
}
 
