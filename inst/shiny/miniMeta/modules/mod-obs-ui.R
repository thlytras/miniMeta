# UI for Observational studies module, split into separate file to limit size.
# Called from mod-obs.R. All dependencies (other modules) are there.

obs_moduleUI <- function(id) {
  source("modules/include-mod-univ-ui.R", local=TRUE)
  ns <- NS(id)
  tabPanel("Observational studies module",
    ui_importer(ns),
    sidebarLayout(
      sidebarPanel(
        obsLoadDataUI(id = ns("loadData")),
        fluidRow(
          column(6, ui_effectMeasure_select(ns)),
          column(6, style = "margin-top: 15px;", prettySwitch(ns("showOptions"), "Show analysis options", status="primary"))
        ),
        conditionalPanel(sprintf("input['%s']", ns("showOptions")),
          wellPanel(
            fluidRow(
              column(6, offset=6,
                awesomeCheckbox(ns("opt_combFixed"), "Use fixed-effects model", value=FALSE),
                awesomeCheckbox(ns("opt_combRandom"), "Use random-effects model", value=TRUE),
                ui_heterogeneity_select(ns),
                awesomeCheckbox(ns("opt_hakn"), "Hartung and Knapp correction", value=FALSE)
              )
            )
          )
        ),
      width=6),

      ui_mainPanel(ns, mtype = 2)
    )
  )
}
 
