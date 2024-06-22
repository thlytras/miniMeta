#' Common UI component functions for RCT and observational modules
#'
#' These helper functions are directly called from the UI functions 
#' of the RCT and observational modules, and encode their common 
#' functionality, in order to avoid all code duplication.
#' 
#' @param ns Namespace id of the calling module
#' @param mtype Type of the module; 1 for RCT, 2 for observational
#' 
#' @importFrom colourpicker colourInput
#' @import shinyWidgets
#'
#' @name components-modUniv-ui
#'
#' @keywords internal
#' @noRd
NULL


#' @rdname components-modUniv-ui
#' @noRd
ui_importer <- function(ns) {
  prefMenu <- list(
    list(label="Save settings to browser", icon=icon("cloud-upload")),
    list(label="Load settings from browser", icon=icon("cloud-download")), 
    list(label="Clear settings from browser", icon=icon("eraser")),
    list(label="Save settings to file", icon=icon("file-download")),
    list(span=HTML("<label for=\"loadStateFromFile\"><i class=\"fa fa-file-upload\" style=\"margin-right:0.3em\"></i>Load settings from file</label>"))
  )
  names(prefMenu) <- c(ns("saveState"), ns("loadState"), ns("rmState"), ns("saveStateFile"), ns("loadStateFile"))
  fluidPage(br(),fluidRow(
    miniFileInput(ns("import"), "Import meta-analysis", accept = c('application/octet-stream')),
    downloadButton(ns("export"), "Export meta-analysis"),
    downloadButton(ns("exportSource"), "Export as source code", style="margin-left:1em; margin-right:1em"),
    dropdownMenu(label = "Settings", menu = prefMenu, 
          icon = icon("bars"), style = "display:inline-block; float:right")
  ), br())
}


#' @rdname components-modUniv-ui
#' @noRd
ui_heterogeneity_select <- function(ns) {
  selectInput(ns("opt_methodTau"), "Heterogeneity estimator", 
    c("DerSimonian-Laird"="DL", "Paule-Mandel"="PM", 
      "Restricted Maximum-Likelihood"="REML", 
      "Maximum Likelihood"="ML", "Hunter-Schmidt"="HS", "Sidik-Jonkman"="SJ", 
      "Hedges"="HE", "Empirical Bayes"="EB"))
}


#' @rdname components-modUniv-ui
#' @noRd
ui_effectMeasure_select <- function(ns) {
  selectInput(ns("opt_sm"), "Effect measure",
            c("Relative Risk"="RR", "Odds Ratio"="OR", "Risk Difference"="RD", 
            "Arcsine Difference"="ASD"))
}


#' @rdname components-modUniv-ui
#' @noRd
ui_mainPanel <- function(ns, mtype) {
  mainPanel(
    tabsetPanel(
      tabPanel("Forest plot", 
        splitLayout(
          downloadButton(ns("forestDownload"), "Download plot"),
          cellArgs = list(style = "padding: 6px; text-align:center")
        ),
        wellPanel(
          uiOutput(ns("forestPlotUI")),
          style="background:white"
        )
      ),
      tabPanel("Plot options",
        div(style="padding-top:0.5em"),
        prettySwitch(ns("plOpt_showDownloadOptions"), "Download options", FALSE, status="primary"),
        conditionalPanel(sprintf("input['%s']", ns("plOpt_showDownloadOptions")),
          plDownloadOptsUI(id = ns("downloadOpts"))
        ),
        prettySwitch(ns("plOpt_showContentOptions"), "Content options", FALSE, status="primary"),
        conditionalPanel(sprintf("input['%s']", ns("plOpt_showContentOptions")),
          wellPanel(
            if (mtype==1) awesomeCheckbox(ns("plOpt_inclAbsNum"), "Show absolute numbers by arm", TRUE),
            fluidRow(
              column(3, awesomeCheckbox(ns("plOpt_printI2"), HTML("I<sup>2</sup>"), TRUE)),
              column(3, awesomeCheckbox(ns("plOpt_printQ"), "Q", FALSE)),
              column(3, awesomeCheckbox(ns("plOpt_printPval"), "p-value", TRUE)),
              column(3, awesomeCheckbox(ns("plOpt_printTau2"), HTML("\u03c4<sup>2</sup>"), FALSE))
            ),
            awesomeCheckbox(ns("plOpt_showWeights"), "Show weights", TRUE),
            awesomeCheckbox(ns("plOpt_prediction"), "Plot a prediction interval (only for random-effects model)", TRUE)
          )
        ),
        prettySwitch(ns("plOpt_showFormattingOptions"), "Formatting options", FALSE, status="primary"),
        conditionalPanel(sprintf("input['%s']", ns("plOpt_showFormattingOptions")),
          wellPanel(
            fluidRow(
              column(4, colourpicker::colourInput(ns("plOpt_barCol"), "Study bar colour", "#000000")),
              column(4, colourpicker::colourInput(ns("plOpt_sqCol"), "Weight square colour", "#BEBEBE")),
              column(4, colourpicker::colourInput(ns("plOpt_diamCol"), "Diamond colour", "#000000"))
            ),
            funnelOptsUi(ns)
          )
        ),
        prettySwitch(ns("plOpt_showAdvancedOptions"), "Advanced options", FALSE, status="primary"),
        conditionalPanel(sprintf("input['%s']", ns("plOpt_showAdvancedOptions")),
          wellPanel(
            textAreaInput(ns("plOpt_advParInput"), 
              "Additional parameters for forest.meta()",
              placeholder="Enter a comma-separated list of parameters..."),
            verbatimTextOutput(ns("plOpt_advParOutput"))
          )
        )
      ),
      tabPanel("GRADE output",
        verbatimTextOutput(ns("uncpanel")),
        if (mtype==2) numericInput(ns("baseRisk"), "Basline risk (%)", value=5, min=0, max=100, step=1)
      ),
      tabPanel("Funnel plot",
        funnelTabUI(id = ns("funnel")),
        if (mtype==1) funnelTabUI(id = ns("labbe"))
      ),
      tabPanel("Help", includeMarkdown(system.file("shiny", "helptext.md", package = "miniMeta")))
    ), width=6
  )
}

