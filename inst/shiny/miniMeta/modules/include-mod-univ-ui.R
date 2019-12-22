source("modules/dropdownMenu.R")

ui_importer <- function(ns) {
  prefMenu <- list(
    list(label="Save preferences to browser", icon=icon("cloud-upload")),
    list(label="Load preferences from browser", icon=icon("cloud-download")), 
    list(label="Clear preferences from browser", icon=icon("eraser")))
  names(prefMenu) <- c(ns("saveState"), ns("loadState"), ns("rmState"))
  fluidPage(br(),fluidRow(
    miniFileInput(ns("import"), "Import meta-analysis", accept = c('application/octet-stream')),
    downloadButton(ns("export"), "Export meta-analysis"),
    downloadButton(ns("exportSource"), "Export as source code", style="margin-left: 2em; margin-right: 2em"),
    dropdownMenu(label = "Preferences", menu = prefMenu, 
        icon = icon("bars"), style = "display:inline-block; float:right")
  ), br())
}


ui_heterogeneity_select <- function(ns) {
  selectInput(ns("opt_methodTau"), "Heterogeneity estimator", 
    c("DerSimonian-Laird"="DL", "Paule-Mandel"="PM", 
      "Restricted Maximum-Likelihood"="REML", 
      "Maximum Likelihood"="ML", "Hunter-Schmidt"="HS", "Sidik-Jonkman"="SJ", 
      "Hedges"="HE", "Empirical Bayes"="EB"))
}


ui_effectMeasure_select <- function(ns) {
  selectInput(ns("opt_sm"), "Effect measure",
            c("Relative Risk"="RR", "Odds Ratio"="OR", "Risk Difference"="RD", 
            "Arcsine Difference"="ASD"))
}


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
        checkboxInput(ns("plOpt_showDownloadOptions"), "Download options", FALSE),
        conditionalPanel(sprintf("input['%s']", ns("plOpt_showDownloadOptions")),
          plDownloadOptsUI(id = ns("downloadOpts"))
        ),
        checkboxInput(ns("plOpt_showContentOptions"), "Content options", FALSE),
        conditionalPanel(sprintf("input['%s']", ns("plOpt_showContentOptions")),
          wellPanel(
            if (mtype==1) checkboxInput(ns("plOpt_inclAbsNum"), "Show absolute numbers by arm", TRUE),
            fluidRow(
              column(3, checkboxInput(ns("plOpt_printI2"), HTML("I<sup>2</sup>"), TRUE)),
              column(3, checkboxInput(ns("plOpt_printQ"), "Q", FALSE)),
              column(3, checkboxInput(ns("plOpt_printPval"), "p-value", TRUE)),
              column(3, checkboxInput(ns("plOpt_printTau2"), "τ^2", FALSE))
            ),
            checkboxInput(ns("plOpt_showWeights"), "Show weights", TRUE)
          )
        ),
        checkboxInput(ns("plOpt_showFormattingOptions"), "Formatting options", FALSE),
        conditionalPanel(sprintf("input['%s']", ns("plOpt_showFormattingOptions")),
          wellPanel(
            fluidRow(
              column(4, colourInput(ns("plOpt_barCol"), "Study bar colour", "#000000")),
              column(4, colourInput(ns("plOpt_sqCol"), "Weight square colour", "#BEBEBE")),
              column(4, colourInput(ns("plOpt_diamCol"), "Diamond colour", "#000000"))
            ),
            funnelOptsUi(ns)
          )
        ),
        checkboxInput(ns("plOpt_showAdvancedOptions"), "Advanced options", FALSE),
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
      tabPanel("Help", includeMarkdown("helptext.md"))
    ), width=6
  )
}

