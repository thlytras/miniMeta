library(miniMeta)
library(meta)
library(metafor)
library(colourpicker)

# Load required modules
source("modules/mod-obsLoadData.R")
source("modules/mod-plDownloadOpts.R")
source("modules/miniFileInput.R")
source("modules/mod-funnel.R")

# Load the UI of this module from separate file
source("modules/mod-obs-ui.R")

# Server logic of the module follows
obs_module <- function(input, output, session) {

  # Helper functions for this module
  source("modules/mod-obs-include.R", local=TRUE)

#   flagFirstRun <- FALSE # Ugly hack to avoid Cairo backend error

  # List of forest.meta() arguments, excluding some that we don't want the user to touch
  forest_args <- formalArgs("forest.meta")
  forest_args <- forest_args[!(forest_args %in% 
    c("...", "x", "comb.random", "comb.fixed", "layout", "new"))]

  values <- reactiveValues(
    importReady = FALSE,
    dataset = NULL
  )
  
  dat <- callModule(module = obsLoadData, id="loadData", 
        dataset = reactive(values$dataset), 
        logMeasure = reactive(input$opt_sm %in% c("RR","OR")))
  
  # REACTIVE: check validity of the data
  chk <- reactive({
    return(TRUE)
  })
  
  # REACTIVE: run the meta-analysis
  m <- reactive({
    if (chk()) {
      grp <- trimws(as.character(dat()$group)); grp[grp==""] <- NA
      if (sum(is.na(grp))==0 & length(unique(grp))>1) {
        byVar <- factor(grp)
      } else {
        byVar <- NULL
      }
      te <- if (input$opt_sm %in% c("RR","OR")) log(dat()$TE) else dat()$TE
      return(metagen(te, abs(seTE), data=dat(), studlab=Study, 
        method.tau=input$opt_methodTau,
        comb.fixed=input$opt_combFixed, comb.random=input$opt_combRandom,
        byvar=byVar, sm=input$opt_sm, hakn=input$opt_hakn
      ))
    }
  })
  
  # REACTIVE: get all plot options in a list
  pltOpt <- reactive({
    lcols <- c("studlab")
    rcols <- c("effect","ci")
    if (input$plOpt_showWeights) {
      if (input$opt_combFixed) rcols <- c(rcols, "w.fixed")
      if (input$opt_combRandom) rcols <- c(rcols, "w.random")
    }
    plOpts <- list(
      leftcols=lcols,
      rightcols=rcols,
      print.I2 = input$plOpt_printI2, 
      print.Q = input$plOpt_printQ,
      print.pval.Q = input$plOpt_printPval,
      print.tau2 = input$plOpt_printTau2,
      col.diamond = input$plOpt_diamCol,
      col.diamond.lines = input$plOpt_diamCol,
      col.study = input$plOpt_barCol,
      col.square = input$plOpt_sqCol
    )
    if (class(pltAdvOpt())!="try-error" && length(pltAdvOpt())>0) {
      plOpts <- rev(c(plOpts, pltAdvOpt()))
      plOpts <- rev(plOpts[!duplicated(names(plOpts))])
    }
    return(plOpts)
  })
  
  
  plOpt_downloadOpts <- reactiveValues(
        fileType=NULL, width=NULL, height=NULL, pointsize=NULL,
        res=NULL, lwd=NULL, spacing=NULL)
  plOpt_mod_downloadOpts <- callModule(module = plDownloadOpts, id="downloadOpts", 
        setOpts = plOpt_downloadOpts)
  
  observeEvent(plOpt_mod_downloadOpts$trigger, {
    for (n in except(names(plOpt_mod_downloadOpts), "trigger")) {
      plOpt_downloadOpts[[n]] <- plOpt_mod_downloadOpts[[n]]
    }
  })
  
  # REACTIVE: parse all advanced plot options
  pltAdvOpt <- reactive({
    res <- parseArguments(input$plOpt_advParInput)
    if (class(res)!="try-error" && length(res)>0) {
      res <- res[names(res) %in% forest_args]
    }
    res
  })
  
  forest_obs <- function(new=TRUE, pointsize=12, lwd=1, spacing=1) {
    cilayout("(", " - ")
    pars <- c(list(x=m(), new=new), pltOpt(),
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
    
  output$plOpt_advParOutput <- renderText({
    if (class(pltAdvOpt())=="try-error") return(as.character(attr(pltAdvOpt(), "condition")))
    if (length(pltAdvOpt())==0) return("No extra parameters provided (or parameters unknown to forest.meta())")
    return(gsub("^list\\(|\\)$", "", deparse(pltAdvOpt())))
  })
  
  output$forestPlotUI <- renderUI({
    nr <- nrow(dat())
    if (!is.numeric(nr)) nr <- 5
#     if (!flagFirstRun) {
#       flagFirstRun <<- TRUE
#       return()
#     }
    plotOutput(session$ns("forestPlot"), height=paste0(12 + 1.1*nr, "em"), width="100%")
  })
  
  # REACTIVE: render the forest plot
  output$forestPlot <- renderPlot({
    if (chk()) {
      forest_obs(new=TRUE)
    }
  })
  
  
  # Download the forest plot
  output$forestDownload <- downloadHandler(
    filename = function() {
      sprintf("forest.%s", gsub("cairo_", "", plOpt_downloadOpts$fileType, fixed=TRUE))
    },
    content = function(file) {
      fileOptions <- list(filename=file, 
        width=plOpt_downloadOpts$width, height=plOpt_downloadOpts$height, 
        pointsize=plOpt_downloadOpts$pointsize)
      if (plOpt_downloadOpts$fileType %in% c("png", "tiff")) {
        fileOptions$res <- plOpt_downloadOpts$res
        fileOptions$width <- fileOptions$width * fileOptions$res
        fileOptions$height <- fileOptions$height * fileOptions$res
        if (plOpt_downloadOpts$fileType=="tiff") fileOptions$compression <- "lzw"
      }
      do.call(plOpt_downloadOpts$fileType, fileOptions)
      forest_obs(pointsize=plOpt_downloadOpts$pointsize, 
        spacing=plOpt_downloadOpts$spacing, lwd=plOpt_downloadOpts$lwd)
      dev.off()
    }
  )
  

  

  
  # Code to import meta-analysis
  observeEvent(input$import, {
#     values$importReady <- FALSE
    if (is.null(input$import)) return()
    inFile <- input$import
    m <- try(readRDS(inFile$datapath), silent=TRUE)
      # Has the file been read successfully?
    if (length(m)==1 && class(m)=="try-error") {
      showModal(modalDialog(title = "Whoops...", 
        "Error while trying to read this file.", br(), "Is it an actual miniMeta file?", 
        footer = modalButton("OK, got it"), size="s"))
      return()
    }
      # Does it appear to be a correct miniMeta file
    if (!is.miniMeta.obs(m)) {
      if (is.miniMeta.rct(m)) {
        showModal(modalDialog(title = "Notice:", 
          "This is indeed a miniMeta file, but it contains a meta-analysis of Randomized Controlled Trials (RCTs).",
          br(), "Please move to the RCT module and import it there. Thank you.", 
          footer = modalButton("OK, got it"), size="s"))
        return()
      }
      showModal(modalDialog(title = "Whoops...", 
        "This is a serialized R object, but it does not appear to be a valid miniMeta file.",
        footer = modalButton("OK, got it"), size="s"))
      return()
    }
    values$dataset <- NULL
    values$dataset <- m$data
    for (n in c("sm", "method", "methodTau")) {
      updateSelectInput(session, paste0("opt_", n), 
          selected = m$analysisOptions[[n]])
    }
    for (n in c("combFixed", "combRandom", "hakn")) {
      updateCheckboxInput(session, paste0("opt_", n), 
          value = m$analysisOptions[[n]])
    }
    for (n in c("inclAbsNum", "printI2", "printQ", "printPval", "printTau2")) {
      updateCheckboxInput(session, paste0("plOpt_", n), 
          value = m$plotOptions[[n]])
    }
    updateTextAreaInput(session, "plOpt_advParInput", value = m$plotOptions$advParInput)
    for (n in except(names(plOpt_downloadOpts), "trigger")) {
      plOpt_downloadOpts[[n]] <- m$plotOptions[[n]]
    }
#     values$importReady <- TRUE
  }, ignoreInit=TRUE)

  
  makeMiniMetaObject <- function() {
    m <- list(
      data = dat(),
      meta = m(),
      analysisOptions = sapply(c("sm", "combFixed", "combRandom", 
        "methodTau", "hakn"), function(x) 
        input[[paste0("opt_", x)]], simplify=FALSE
      ),
      plotOptions = c(reactiveValuesToList(plOpt_downloadOpts),
        sapply(c("printI2", "printQ", "printPval", "printTau2", 
          "showWeights",
          "diamCol", "barCol", "sqCol",
          "advParInput"), function(x)
          input[[paste0("plOpt_", x)]], simplify=FALSE
        ),
        reactiveValuesToList(funnelOptions)
      )
    )
    class(m) <- c("miniMeta", "list")
    return(m)
  }
  
  # Export meta-analysis
  output$export <- downloadHandler(
    filename = function() {
      "miniMeta_obs.rds"
    },
    content = function(file) {
      m <- makeMiniMetaObject()
      saveRDS(m, file=file)
    }
  )

  output$exportSource <- downloadHandler(
    filename = function() {
      "miniMeta_analysis.R"
    },
    content = function(file) {
      m <- makeMiniMetaObject()
      writeLines(as.source(m), file)
    }
  )

  
  # REACTIVE: render the output panel
  output$uncpanel <- renderPrint({
    bR <- as.numeric(input$baseRisk)
    if (!is.na(bR) && (bR<0 || bR>100)) bR <- NA
    if (is.na(bR)) return(cat("Missing baseline risk"))
    if (inherits(m(), "metagen")) {
      return(print(gradeObs(m(), bR)))
    } else {
      return(cat("No meta-analysis input"))
    }
  })

  funnelOptions <- reactiveValues(
    showStudlab = NULL, fileType = NULL, ptCol = NULL
  )
  
  observe({
    funnelOptions$showStudlab <- input$funOpt_showStudlab
    funnelOptions$fileType <- plOpt_downloadOpts$fileType
    funnelOptions$ptCol <- input$funOpt_ptCol
    funnelOptions$posStudlab <- input$funOpt_posStudlab
  })
  
  callModule(module = funnelTab, id="funnel", 
    meta = reactive(m()),
    options = funnelOptions
  )

}

