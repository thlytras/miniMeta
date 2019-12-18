library(miniMeta)
library(meta)
library(metafor)
library(colourpicker)

# Load required modules
source("modules/mod-rctLoadData.R")
source("modules/mod-plDownloadOpts.R")
source("modules/miniFileInput.R")
source("modules/mod-funnel.R")

# Load the UI of this module from separate file
source("modules/mod-rct-ui.R")

# Server logic of the module follows
rct_module <- function(input, output, session) {

  # Helper functions for this module
  source("modules/mod-rct-include.R", local=TRUE)

  flagFirstRun <- FALSE # Ugly hack to avoid Cairo backend error

  # List of forest.meta() arguments, excluding some that we don't want the user to touch
  forest_args <- formalArgs("forest.meta")
  forest_args <- forest_args[!(forest_args %in% 
    c("...", "x", "comb.random", "comb.fixed", "layout", "new"))]

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
    for (n in except(names(rctPlOpt_mod_downloadOpts), "trigger")) {
      rctPlOpt_downloadOpts[[n]] <- rctPlOpt_mod_downloadOpts[[n]]
    }
  })
  
  # REACTIVE: parse all advanced plot options
  rcts_pltAdvOpt <- reactive({
    res <- parseArguments(input$rctPlOpt_advParInput)
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
    nr <- nrow(rcts_dat())
    if (!is.numeric(nr)) nr <- 5
    if (!flagFirstRun) {
      flagFirstRun <<- TRUE
      return()
    }
    plotOutput(session$ns("rctsForestPlot"), height=paste0(12 + 1.1*nr, "em"), width="100%")
  })
  
  # REACTIVE: render the forest plot
  output$rctsForestPlot <- renderPlot({
    if (rcts_chk()) {
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
      if (rcts_chk()) {
        forest_rct(pointsize=rctPlOpt_downloadOpts$pointsize, 
          spacing=rctPlOpt_downloadOpts$spacing, lwd=rctPlOpt_downloadOpts$lwd)
      }
      dev.off()
    }
  )
  

  

  
  # Code to import meta-analysis
  observeEvent(input$rctsImport, {
#     values$rctsImportReady <- FALSE
    if (is.null(input$rctsImport)) return()
    inFile <- input$rctsImport
    m <- try(readRDS(inFile$datapath), silent=TRUE)
      # Has the file been read successfully?
    if (length(m)==1 && class(m)=="try-error") {
      showModal(modalDialog(title = "Whoops...", 
        "Error while trying to read this file.", br(), "Is it an actual miniMeta file?", 
        footer = modalButton("OK, got it"), size="s"))
      return()
    }
      # Does it appear to be a correct miniMeta file
    if (!is.miniMeta.rct(m)) {
      if (is.miniMeta.obs(m)) {
        showModal(modalDialog(title = "Notice:", 
          "This is indeed a miniMeta file, but it contains a meta-analysis of observational studies.",
          br(), "Please move to the Observational studies module and import it there. Thank you.", 
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
    for (n in c("sm", "method", "methodTau", "incr")) {
      updateSelectInput(session, paste0("rctOpt_", n), 
          selected = m$analysisOptions[[n]])
    }
    for (n in c("combFixed", "combRandom", "hakn")) {
      updateCheckboxInput(session, paste0("rctOpt_", n), 
          value = m$analysisOptions[[n]])
    }
    for (n in c("inclAbsNum", "printI2", "printQ", "printPval", "printTau2")) {
      updateCheckboxInput(session, paste0("rctPlOpt_", n), 
          value = m$plotOptions[[n]])
    }
    updateTextAreaInput(session, "rctPlOpt_advParInput", value = m$plotOptions$advParInput)
    for (n in except(names(rctPlOpt_downloadOpts), "trigger")) {
      rctPlOpt_downloadOpts[[n]] <- m$plotOptions[[n]]
    }
#     values$rctsImportReady <- TRUE
  }, ignoreInit=TRUE)

  
  # Export meta-analysis
  output$rctsExport <- downloadHandler(
    filename = function() {
      "miniMeta_RCTs.rds"
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
          ),
          reactiveValuesToList(funnelOptions)
        )
      )
      class(m) <- c("miniMeta", "list")
      saveRDS(m, file=file)
    }
  )

  # REACTIVE: render the output panel
  output$uncpanel <- renderPrint({
    if (rcts_chk()) {
      return(print(gradeRCT(rcts_dat()[,-1], m())))
    } else {
      return(cat(paste(attr(rcts_chk(), "msg"), sep="", collapse="\n")))
    }
  })
  
  funnelOptions <- reactiveValues(
    showStudlab = NULL, fileType = NULL, ptCol = NULL
  )
  
  observe({
    funnelOptions$showStudlab <- input$funOpt_showStudlab
    funnelOptions$fileType <- rctPlOpt_downloadOpts$fileType
    funnelOptions$ptCol <- input$funOpt_ptCol
    funnelOptions$posStudlab <- input$funOpt_posStudlab
  })
  
  callModule(module = funnelTab, id="rctFunnel", 
    meta = reactive(m()),
    options = funnelOptions
  )

  callModule(module = funnelTab, id="rctLabbe", labbe=TRUE,
    meta = reactive(m()),
    options = funnelOptions
  )

}

