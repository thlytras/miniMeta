source("modules/mod-rctLoadData.R")
source("modules/mod-plDownloadOpts.R")
source("modules/miniFileInput.R")

source("modules/include.R")

library(meta)
# List of forest.meta() arguments, excluding some that we don't want the user to touch
forest_args <- formalArgs("forest.meta")
forest_args <- forest_args[!(forest_args %in% 
  c("...", "x", "comb.random", "comb.fixed", "layout", "new"))]


rct_moduleUI <- function(id) {
  ns <- NS(id)
  tabPanel("RCT module",
    fluidPage(br(),fluidRow(
      miniFileInput(ns("rctsImport"), "Import meta-analysis"),
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
            conditionalPanel(sprintf("input['%s']", ns("showFormattingOptions")),
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



rct_module <- function(input, output, session) {

  values <- reactiveValues(
    rctsImportReady = FALSE,
    dataset = NULL
  )
  
  rcts_dat <- callModule(module = rctLoadData, id="rctLoadData", 
        dataset = reactive(values$dataset))
  
  # REACTIVE: check validity of the data
  rcts_chk <- reactive({
    checkRCTValidity(rcts_dat())
  })
  
  # REACTIVE: run the meta-analysis
  m <- reactive({
    if (rcts_chk()) {
      optIncr <- input$rctOpt_incr; if (optIncr!="TACC") optIncr <- as.numeric(optIncr)
      grp <- trimws(as.character(rcts_dat()$group)); grp[grp==""] <- NA
      if (sum(is.na(grp))==0 & length(unique(grp))>1) {
        byVar <- factor(grp)
      } else {
        byVar <- NULL
      }
      return(metabin(e.e, n.e, e.c, n.c, data=rcts_dat(), studlab=Study, 
        method=input$rctOpt_method, method.tau=input$rctOpt_methodTau,
        comb.fixed=input$rctOpt_combFixed, comb.random=input$rctOpt_combRandom,
        byvar=byVar, incr=optIncr, sm=input$rctOpt_sm, hakn=input$rctOpt_hakn
      ))
    }
  })
  
  # REACTIVE: get all plot options in a list
  rcts_pltOpt <- reactive({
    lcols <- c("studlab")
    if (input$rctPlOpt_inclAbsNum) lcols <- c(lcols, "event.e", "n.e", "event.c", "n.c")
    plOpts <- list(
      leftcols=lcols,
      print.I2 = input$rctPlOpt_printI2, 
      print.Q = input$rctPlOpt_printQ,
      print.pval.Q = input$rctPlOpt_printPval,
      print.tau2 = input$rctPlOpt_printTau2
    )
    if (class(rcts_pltAdvOpt())!="try-error" && length(rcts_pltAdvOpt())>0) {
      plOpts <- rev(c(plOpts, rcts_pltAdvOpt()))
      plOpts <- rev(plOpts[!duplicated(names(plOpts))])
    }
    return(plOpts)
  })
  
  
  rctPlOpt_downloadOpts <- reactiveValues(
        fileType=NULL, width=NULL, height=NULL, pointsize=NULL,
        res=NULL, lwd=NULL, spacing=NULL)
  rctPlOpt_mod_downloadOpts <- callModule(module = plDownloadOpts, id="rctDownloadOpts", 
        setOpts = rctPlOpt_downloadOpts)
  
  observeEvent(rctPlOpt_mod_downloadOpts$trigger, {
    for (n in names(rctPlOpt_mod_downloadOpts)) {
      rctPlOpt_downloadOpts[[n]] <- rctPlOpt_mod_downloadOpts[[n]]
    }
  })
  
  # REACTIVE: parse all advanced plot options
  rcts_pltAdvOpt <- reactive({
    res <- readAdvParameters(input$rctPlOpt_advParInput)
    if (class(res)!="try-error" && length(res)>0) {
      res <- res[names(res) %in% forest_args]
    }
    res
  })
  
  forest_rct <- function(new=TRUE, pointsize=12, lwd=1, spacing=1) {
    cilayout("(", " - ")
    pars <- c(list(x=m(), new=new), rcts_pltOpt(),
      list(
        text.fixed = "Fixed-effects model",
        text.random = "Random-effects model",
        col.diamond = "black",
        fontsize = pointsize,
        plotwidth = sprintf("%.2fcm", 8*pointsize/12),
        colgap = sprintf("%.2fmm", 2*pointsize/12),
        lwd = lwd,
        spacing = spacing
      ))
    pars <- pars[!duplicated(names(pars))]
    do.call(forest, pars)
  }
    
  output$rctPlOpt_advParOutput <- renderText({
    if (class(rcts_pltAdvOpt())=="try-error") return(as.character(attr(rcts_pltAdvOpt(), "condition")))
    if (length(rcts_pltAdvOpt())==0) return("No extra parameters provided (or parameters unknown to forest.meta())")
    return(gsub("^list\\(|\\)$", "", deparse(rcts_pltAdvOpt())))
  })
  
  output$rctsForestPlotUI <- renderUI({
    cat("Rendering...\n")
    nr <- nrow(rcts_dat())
    cat(print(nr))
    cat("\n")
    if (!is.numeric(nr)) nr <- 5
    plotOutput(session$ns("rctsForestPlot"), height=paste0(12 + 1.1*nr, "em"), width="100%")
  })
  
  # REACTIVE: render the forest plot
  output$rctsForestPlot <- renderPlot({
    if (rcts_chk()) {
      cat("Drawing...\n")
      forest_rct(new=TRUE)
    }
  })
  
    
  # Download the forest plot
  output$rctsForestDownload <- downloadHandler(
    filename = function() {
      sprintf("forest.%s", gsub("cairo_", "", rctPlOpt_downloadOpts$fileType, fixed=TRUE))
    },
    content = function(file) {
      fileOptions <- list(filename=file, 
        width=rctPlOpt_downloadOpts$width, height=rctPlOpt_downloadOpts$height, 
        pointsize=rctPlOpt_downloadOpts$pointsize)
      if (rctPlOpt_downloadOpts$fileType %in% c("png", "tiff")) {
        fileOptions$res <- rctPlOpt_downloadOpts$res
        fileOptions$width <- fileOptions$width * fileOptions$res
        fileOptions$height <- fileOptions$height * fileOptions$res
        if (rctPlOpt_downloadOpts$fileType=="tiff") fileOptions$compression <- "lzw"
      }
      do.call(rctPlOpt_downloadOpts$fileType, fileOptions)
      forest_rct(pointsize=rctPlOpt_downloadOpts$pointsize, 
        spacing=rctPlOpt_downloadOpts$spacing, lwd=rctPlOpt_downloadOpts$lwd)
      dev.off()
    }
  )
  

  

  
  # Code to import meta-analysis
  observeEvent(input$rctsImport, {
    values$rctsImportReady <- FALSE
    if (is.null(input$rctsImport)) return()
    inFile <- input$rctsImport
    m <- readRDS(inFile$datapath)
    values$dataset <- NULL
    values$dataset <- m$data
    updateSelectInput(session, "rctOpt_sm", 
        selected = m$analysisOptions$sm)
    updateCheckboxInput(session, "rctOpt_combFixed",
        value = m$analysisOptions$combFixed)
    updateCheckboxInput(session, "rctOpt_combRandom",
        value = m$analysisOptions$combRandom)
    updateSelectInput(session, "rctOpt_method", 
        selected = m$analysisOptions$method)
    updateSelectInput(session, "rctOpt_methodTau", 
        selected = m$analysisOptions$methodTau)
    updateSelectInput(session, "rctOpt_incr", 
        selected = m$analysisOptions$incr)
    updateCheckboxInput(session, "rctOpt_hakn",
        value = m$analysisOptions$hakn)
    rctPlOpt_downloadOpts$fileType <- m$plotOptions$fileType
    rctPlOpt_downloadOpts$res <- m$plotOptions$res
    rctPlOpt_downloadOpts$width <- m$plotOptions$width
    rctPlOpt_downloadOpts$height <- m$plotOptions$height
    rctPlOpt_downloadOpts$lwd <- m$plotOptions$lwd
    rctPlOpt_downloadOpts$spacing <- m$plotOptions$spacing
    rctPlOpt_downloadOpts$pointsize <- m$plotOptions$pointsize
    updateCheckboxInput(session, "rctPlOpt_inclAbsNum", 
        value = m$plotOptions$inclAbsNum)
    updateCheckboxInput(session, "rctPlOpt_printI2", 
        value = m$plotOptions$printI2)
    updateCheckboxInput(session, "rctPlOpt_printQ", 
        value = m$plotOptions$printQ)
    updateCheckboxInput(session, "rctPlOpt_printPval", 
        value = m$plotOptions$printPval)
    updateCheckboxInput(session, "rctPlOpt_printTau2", 
        value = m$plotOptions$printTau2)
    updateTextAreaInput(session, "rctPlOpt_advParInput",
        value = m$plotOptions$advParInput)
    values$rctsImportReady <- TRUE
  })

  # Export meta-analysis
  output$rctsExport <- downloadHandler(
    filename = function() {
      "miniMeta_analysis.rds"
    },
    content = function(file) {
      m <- list(
        data = rcts_dat(),
        meta = m(),
        analysisOptions = sapply(c("sm", "combFixed", "combRandom", 
          "method", "methodTau", "incr", "hakn"), function(x) 
          input[[paste0("rctOpt_", x)]], simplify=FALSE
        ),
        plotOptions = c(reactiveValuesToList(rctPlOpt_downloadOpts),
          sapply(c("inclAbsNum", "printI2", 
            "printQ", "printPval", "printTau2", "advParInput"), function(x)
            input[[paste0("rctPlOpt_", x)]], simplify=FALSE
          )
        )
      )
      class(m) <- c("miniMeta", "list")
      saveRDS(m, file=file)
    }
  )

  # REACTIVE: render the output panel
  output$uncpanel <- renderPrint({
    if (rcts_chk()) {
      return(print(gradeRR(rcts_dat()[,-1], m())))
    } else {
      return(cat(paste(attr(rcts_chk(), "msg"), sep="", collapse="\n")))
    }
  })

}

