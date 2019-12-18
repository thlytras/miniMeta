# UI for Observational studies module, split into separate file to limit size.
# Called from mod-obs.R. All dependencies (other modules) are there.

obs_moduleUI <- function(id) {
  ns <- NS(id)
  tabPanel("Observational studies module",
    fluidPage(br(),fluidRow(
      miniFileInput(ns("obsImport"), "Import meta-analysis", accept = c('application/octet-stream')),
      downloadButton(ns("obsExport"), "Export meta-analysis")
    ), br()),
    sidebarLayout(
      sidebarPanel(
        obsLoadDataUI(id = ns("obsLoadData")),
        fluidRow(
          column(6, selectInput(ns("obsOpt_sm"), "Effect measure",
            c("Relative Risk"="RR", "Odds Ratio"="OR", "Risk Difference"="RD", 
            "Arcsine Difference"="ASD"))),
          column(6, style = "margin-top: 15px;", checkboxInput(ns("obsShowOptions"), "Show analysis options"))
        ),
        conditionalPanel(sprintf("input['%s']", ns("obsShowOptions")),
          wellPanel(
            fluidRow(
              column(6, offset=6,
                checkboxInput(ns("obsOpt_combFixed"), "Use fixed-effects model", value=FALSE),
                checkboxInput(ns("obsOpt_combRandom"), "Use random-effects model", value=TRUE),
                selectInput(ns("obsOpt_methodTau"), "Heterogeneity estimator", 
                  c("DerSimonian-Laird"="DL", "Paule-Mandel"="PM", 
                    "Restricted Maximum-Likelihood"="REML", 
                    "Maximum Likelihood"="ML", "Hunter-Schmidt"="HS", "Sidik-Jonkman"="SJ", 
                    "Hedges"="HE", "Empirical Bayes"="EB")),
                checkboxInput(ns("obsOpt_hakn"), "Hartung and Knapp correction", value=FALSE)
              )
            )
          )
        ),
      width=6),
  
      mainPanel(
        tabsetPanel(
          tabPanel("Forest plot", 
            splitLayout(
              downloadButton(ns("obsForestDownload"), "Download plot"),
              cellArgs = list(style = "padding: 6px; text-align:center")
            ),
            wellPanel(
              uiOutput(ns("obsForestPlotUI")),
              style="background:white"
            )
          ),
          tabPanel("Plot options",
            checkboxInput(ns("obsPlOpt_showDownloadOptions"), "Download options", FALSE),
            conditionalPanel(sprintf("input['%s']", ns("obsPlOpt_showDownloadOptions")),
              plDownloadOptsUI(id = ns("obsDownloadOpts"))
            ),
            checkboxInput(ns("obsPlOpt_showContentOptions"), "Content options", FALSE),
            conditionalPanel(sprintf("input['%s']", ns("obsPlOpt_showContentOptions")),
              wellPanel(
                fluidRow(
                  column(3, checkboxInput(ns("obsPlOpt_printI2"), HTML("I<sup>2</sup>"), TRUE)),
                  column(3, checkboxInput(ns("obsPlOpt_printQ"), "Q", FALSE)),
                  column(3, checkboxInput(ns("obsPlOpt_printPval"), "p-value", TRUE)),
                  column(3, checkboxInput(ns("obsPlOpt_printTau2"), "τ^2", FALSE))
                ),
                checkboxInput(ns("obsPlOpt_showWeights"), "Show weights", TRUE)
              )
            ),
            checkboxInput(ns("obsPlOpt_showFormattingOptions"), "Formatting options", FALSE),
            conditionalPanel(sprintf("input['%s']", ns("obsPlOpt_showFormattingOptions")),
              wellPanel(
                fluidRow(
                  column(4, colourInput(ns("obsPlOpt_barCol"), "Study bar colour", "#000000")),
                  column(4, colourInput(ns("obsPlOpt_sqCol"), "Weight square colour", "#BEBEBE")),
                  column(4, colourInput(ns("obsPlOpt_diamCol"), "Diamond colour", "#000000"))
                ),
                funnelOptsUi(ns)
              )
            ),
            checkboxInput(ns("obsPlOpt_showAdvancedOptions"), "Advanced options", FALSE),
            conditionalPanel(sprintf("input['%s']", ns("obsPlOpt_showAdvancedOptions")),
              wellPanel(
                textAreaInput(ns("obsPlOpt_advParInput"), 
                  "Additional parameters for forest.meta()",
                  placeholder="Enter a comma-separated list of parameters..."),
                verbatimTextOutput(ns("obsPlOpt_advParOutput"))
              )
            )
          ),
          tabPanel("GRADE output",
            verbatimTextOutput(ns("uncpanel")),
            numericInput(ns("baseRisk"), "Basline risk (%)", value=5, min=0, max=100, step=1)
          ),
          tabPanel("Funnel plot", 
            funnelTabUI(id = ns("obsFunnel"))
          ),
          tabPanel("Help", includeMarkdown("helptext.md"))
        ), width=6
      )
    )
  )
}
 
