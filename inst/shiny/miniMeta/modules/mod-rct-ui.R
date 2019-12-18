# UI for RCT module, split into separate file to limit size.
# Called from mod-rct.R. All dependencies (other modules) are there.

rct_moduleUI <- function(id) {
  ns <- NS(id)
  tabPanel("RCT module",
    fluidPage(br(),fluidRow(
      miniFileInput(ns("import"), "Import meta-analysis", accept = c('application/octet-stream')),
      downloadButton(ns("export"), "Export meta-analysis"),
      downloadButton(ns("exportSource"), "Export as source code", style="margin-left: 2em")
    ), br()),
    sidebarLayout(
      sidebarPanel(
        rctLoadDataUI(id = ns("loadData")),
        checkboxInput(ns("showOptions"), "Show analysis options"),
        conditionalPanel(sprintf("input['%s']", ns("showOptions")),
          wellPanel(
            fluidRow(
              column(6,
                selectInput(ns("opt_sm"), "Effect measure",
                  c("Relative Risk"="RR", "Odds Ratio"="OR", "Risk Difference"="RD", 
                  "Arcsine Difference"="ASD"))
              ),
              column(6,
                checkboxInput(ns("opt_combFixed"), "Use fixed-effects model", value=FALSE),
                checkboxInput(ns("opt_combRandom"), "Use random-effects model", value=TRUE)
              )
            ),
            fluidRow(
              column(6, selectInput(ns("opt_method"), "Method for pooling",
                c("Mantel-Hanszel"="MH", "Inverse variance"="Inverse", 
                "Peto method"="Peto"))),
              column(6, selectInput(ns("opt_methodTau"), "Heterogeneity estimator", 
                c("DerSimonian-Laird"="DL", "Paule-Mandel"="PM", 
                  "Restricted Maximum-Likelihood"="REML", 
                  "Maximum Likelihood"="ML", "Hunter-Schmidt"="HS", "Sidik-Jonkman"="SJ", 
                  "Hedges"="HE", "Empirical Bayes"="EB")))
            ),
            fluidRow(
              column(6,
                selectInput(ns("opt_incr"), "Continuity correction for blank cells", c("TACC", "0.5"))
              ),
              column(6,
                checkboxInput(ns("opt_hakn"), "Hartung and Knapp correction", value=FALSE)
              )
            )
          )
        ),
      width=6),
  
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
              plDownloadOptsUI(id = ns("rctDownloadOpts"))
            ),
            checkboxInput(ns("plOpt_showContentOptions"), "Content options", FALSE),
            conditionalPanel(sprintf("input['%s']", ns("plOpt_showContentOptions")),
              wellPanel(
                checkboxInput(ns("plOpt_inclAbsNum"), "Show absolute numbers by arm", TRUE),
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
            verbatimTextOutput(ns("uncpanel"))
          ),
          tabPanel("Funnel plot",
            funnelTabUI(id = ns("funnel")),
            funnelTabUI(id = ns("labbe"))
          ),
          tabPanel("Help", includeMarkdown("helptext.md"))
        ), width=6
      )
    )
  )
}
 
