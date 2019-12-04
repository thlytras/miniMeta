# UI for RCT module, split into separate file to limit size.
# Called from mod-rct.R. All dependencies (other modules) are there.

rct_moduleUI <- function(id) {
  ns <- NS(id)
  tabPanel("RCT module",
    fluidPage(br(),fluidRow(
      miniFileInput(ns("rctsImport"), "Import meta-analysis", accept = c('application/octet-stream')),
      downloadButton(ns("rctsExport"), "Export meta-analysis")
    ), br()),
    sidebarLayout(
      sidebarPanel(
        rctLoadDataUI(id = ns("rctLoadData")),
        checkboxInput(ns("rctsShowOptions"), "Show analysis options"),
        conditionalPanel(sprintf("input['%s']", ns("rctsShowOptions")),
          wellPanel(
            fluidRow(
              column(6,
                selectInput(ns("rctOpt_sm"), "Effect measure",
                  c("Relative Risk"="RR", "Odds Ratio"="OR", "Risk Difference"="RD", 
                  "Arcsine Difference"="ASD"))
              ),
              column(6,
                checkboxInput(ns("rctOpt_combFixed"), "Use fixed-effects model", value=FALSE),
                checkboxInput(ns("rctOpt_combRandom"), "Use random-effects model", value=TRUE)
              )
            ),
            fluidRow(
              column(6, selectInput(ns("rctOpt_method"), "Method for pooling",
                c("Mantel-Hanszel"="MH", "Inverse variance"="Inverse", 
                "Peto method"="Peto", "GLMM"="GLMM"))),
              column(6, selectInput(ns("rctOpt_methodTau"), "Heterogeneity estimator", 
                c("DerSimonian-Laird"="DL", "Paule-Mandel"="PM", 
                  "Restricted Maximum-Likelihood"="REML", 
                  "Maximum Likelihood"="ML", "Hunter-Schmidt"="HS", "Sidik-Jonkman"="SJ", 
                  "Hedges"="HE", "Empirical Bayes"="EB")))
            ),
            fluidRow(
              column(6,
                selectInput(ns("rctOpt_incr"), "Continuity correction for blank cells", c("TACC", "0.5"))
              ),
              column(6,
                checkboxInput(ns("rctOpt_hakn"), "Hartung and Knapp correction", value=FALSE)
              )
            )
          )
        ),
      width=6),
  
      mainPanel(
        tabsetPanel(
          tabPanel("Forest plot", 
            splitLayout(
              downloadButton(ns("rctsForestDownload"), "Download plot"),
              cellArgs = list(style = "padding: 6px; text-align:center")
            ),
            wellPanel(
              uiOutput(ns("rctsForestPlotUI")),
              style="background:white"
            )
          ),
          tabPanel("Plot options",
            checkboxInput(ns("rctPlOpt_showDownloadOptions"), "Download options", FALSE),
            conditionalPanel(sprintf("input['%s']", ns("rctPlOpt_showDownloadOptions")),
              plDownloadOptsUI(id = ns("rctDownloadOpts"))
            ),
            checkboxInput(ns("rctPlOpt_showContentOptions"), "Content options", FALSE),
            conditionalPanel(sprintf("input['%s']", ns("rctPlOpt_showContentOptions")),
              wellPanel(
                checkboxInput(ns("rctPlOpt_inclAbsNum"), "Show absolute numbers by arm", TRUE),
                fluidRow(
                  column(3, checkboxInput(ns("rctPlOpt_printI2"), HTML("I<sup>2</sup>"), TRUE)),
                  column(3, checkboxInput(ns("rctPlOpt_printQ"), "Q", FALSE)),
                  column(3, checkboxInput(ns("rctPlOpt_printPval"), "p-value", TRUE)),
                  column(3, checkboxInput(ns("rctPlOpt_printTau2"), "τ^2", FALSE))
                )
              )
            ),
            checkboxInput(ns("rctPlOpt_showFormattingOptions"), "Formatting options", FALSE),
            conditionalPanel(sprintf("input['%s']", ns("rctPlOpt_showFormattingOptions")),
              wellPanel(
              )
            ),
            checkboxInput(ns("rctPlOpt_showAdvancedOptions"), "Advanced options", FALSE),
            conditionalPanel(sprintf("input['%s']", ns("rctPlOpt_showAdvancedOptions")),
              wellPanel(
                textAreaInput(ns("rctPlOpt_advParInput"), 
                  "Additional parameters for forest.meta()",
                  placeholder="Enter a comma-separated list of parameters..."),
                verbatimTextOutput(ns("rctPlOpt_advParOutput"))
              )
            )
          ),
          tabPanel("GRADE output",
            verbatimTextOutput(ns("uncpanel"))
          ),
          tabPanel("Help", includeMarkdown("helptext.md"))
        ), width=6
      )
    )
  )
}
 
