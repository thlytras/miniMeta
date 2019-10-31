library(shiny)
library(markdown)
library(rhandsontable)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("miniMeta"),
  span("A simple tool to run meta-analyses, with a focus on GRADE SoF tables"),
  tabsetPanel(
    tabPanel("RCT module",
      fluidPage(fluidRow(br(),
        actionButton("rctsImport", "Import meta-analysis"),
        actionButton("rctsExport", "Export meta-analysis"),
        br(), br()
      )),
      sidebarLayout(
        sidebarPanel(
          fileInput('rctsLoadExcel', 'Load an Excel file with abstracted data',
                    accept = c('application/vnd.ms-excel', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')),
          helpText("or place your values here:", style="font-weight:bold"),
          rHandsontableOutput("rctsTabWidget"),
          splitLayout(
            actionButton("addRowToRctsTabWidget", "Add rows"),
            actionButton("trimRctsTabWidget", "Clear empty rows"),
            downloadButton("rctsSaveExcel", "Save as Excel"),
            cellArgs = list(style = "padding: 6px; text-align:center")
          ),
          checkboxInput("rctsShowOptions", "Show options"),
          conditionalPanel("input.rctsShowOptions",
            wellPanel(
              selectInput("rctOpt_sm", "Effect measure",
                c("Relative Risk"="RR", "Odds Ratio"="OR", "Risk Difference"="RD", 
                "Arcsine Difference"="ASD")),
              selectInput("rctOpt_method", "Method for pooling",
                c("Mantel-Hanszel"="MH", "Inverse variance"="Inverse", 
                "Peto method"="Peto", "GLMM"="GLMM")),
              checkboxInput("rctOpt_combFixed", "Use fixed-effects model", value=FALSE),
              checkboxInput("rctOpt_combRandom", "Use random-effects model", value=TRUE),
              selectInput("rctOpt_methodTau", "Heterogeneity estimator", 
                c("DerSimonian-Laird"="DL", "Paule-Mandel"="PM", 
                "Restricted Maximum-Likelihood"="REML", 
                "Maximum Likelihood"="ML", "Hunter-Schmidt"="HS", "Sidik-Jonkman"="SJ", 
                "Hedges"="HE", "Empirical Bayes"="EB")),
              checkboxInput("rctOpt_hakn", "Hartung and Knapp correction", value=FALSE),
              selectInput("rctOpt_incr", "Continuity correction for blank cells", c("TACC", "0.5"))
            )
          ),
        width=6),
    
        mainPanel(
          tabsetPanel(
            tabPanel("Forest plot", 
              splitLayout(
                downloadButton("rctsForestDownload", "Download plot"),
                cellArgs = list(style = "padding: 6px; text-align:center")
              ),
              wellPanel(
                uiOutput("rctsForestPlotUI"),
                style="background:white"
              )
            ),
            tabPanel("Plot options",
              selectInput("rctPlotOpt_col", "Colour", c("black","red","blue"))
            ),
            tabPanel("GRADE output",
              verbatimTextOutput("uncpanel")
            ),
            tabPanel("Help", includeMarkdown("helptext.md"))
          ), width=6
        )
      )
    ),
    tabPanel("Observational studies module")
  )
))
