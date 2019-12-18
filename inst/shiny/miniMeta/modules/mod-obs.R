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
    obsImportReady = FALSE,
    dataset = NULL
  )
  
  obs_dat <- callModule(module = obsLoadData, id="obsLoadData", 
        dataset = reactive(values$dataset), 
        logMeasure = reactive(input$obsOpt_sm %in% c("RR","OR")))
  
  # REACTIVE: check validity of the data
  obs_chk <- reactive({
#     checkRCTValidity(obs_dat())
    return(TRUE)
  })
  
  # REACTIVE: run the meta-analysis
  m <- reactive({
    if (obs_chk()) {
      grp <- trimws(as.character(obs_dat()$group)); grp[grp==""] <- NA
      if (sum(is.na(grp))==0 & length(unique(grp))>1) {
        byVar <- factor(grp)
      } else {
        byVar <- NULL
      }
      te <- if (input$obsOpt_sm %in% c("RR","OR")) log(obs_dat()$TE) else obs_dat()$TE
      return(metagen(te, abs(seTE), data=obs_dat(), studlab=Study, 
        method.tau=input$obsOpt_methodTau,
        comb.fixed=input$obsOpt_combFixed, comb.random=input$obsOpt_combRandom,
        byvar=byVar, sm=input$obsOpt_sm, hakn=input$obsOpt_hakn
      ))
    }
  })
  
  # REACTIVE: get all plot options in a list
  obs_pltOpt <- reactive({
    lcols <- c("studlab")
    plOpts <- list(
      leftcols=lcols,
      print.I2 = input$obsPlOpt_printI2, 
      print.Q = input$obsPlOpt_printQ,
      print.pval.Q = input$obsPlOpt_printPval,
      print.tau2 = input$obsPlOpt_printTau2
    )
    if (class(obs_pltAdvOpt())!="try-error" && length(obs_pltAdvOpt())>0) {
      plOpts <- rev(c(plOpts, obs_pltAdvOpt()))
      plOpts <- rev(plOpts[!duplicated(names(plOpts))])
    }
    return(plOpts)
  })
  
  
  obsPlOpt_downloadOpts <- reactiveValues(
        fileType=NULL, width=NULL, height=NULL, pointsize=NULL,
        res=NULL, lwd=NULL, spacing=NULL)
  obsPlOpt_mod_downloadOpts <- callModule(module = plDownloadOpts, id="obsDownloadOpts", 
        setOpts = obsPlOpt_downloadOpts)
  
  observeEvent(obsPlOpt_mod_downloadOpts$trigger, {
    for (n in except(names(obsPlOpt_mod_downloadOpts), "trigger")) {
      obsPlOpt_downloadOpts[[n]] <- obsPlOpt_mod_downloadOpts[[n]]
    }
  })
  
  # REACTIVE: parse all advanced plot options
  obs_pltAdvOpt <- reactive({
    res <- parseArguments(input$obsPlOpt_advParInput)
    if (class(res)!="try-error" && length(res)>0) {
      res <- res[names(res) %in% forest_args]
    }
    res
  })
  
  forest_obs <- function(new=TRUE, pointsize=12, lwd=1, spacing=1) {
    cilayout("(", " - ")
    pars <- c(list(x=m(), new=new), obs_pltOpt(),
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
    
  output$obsPlOpt_advParOutput <- renderText({
    if (class(obs_pltAdvOpt())=="try-error") return(as.character(attr(obs_pltAdvOpt(), "condition")))
    if (length(obs_pltAdvOpt())==0) return("No extra parameters provided (or parameters unknown to forest.meta())")
    return(gsub("^list\\(|\\)$", "", deparse(obs_pltAdvOpt())))
  })
  
  output$obsForestPlotUI <- renderUI({
    nr <- nrow(obs_dat())
    if (!is.numeric(nr)) nr <- 5
#     if (!flagFirstRun) {
#       flagFirstRun <<- TRUE
#       return()
#     }
    plotOutput(session$ns("obsForestPlot"), height=paste0(12 + 1.1*nr, "em"), width="100%")
  })
  
  # REACTIVE: render the forest plot
  output$obsForestPlot <- renderPlot({
    if (obs_chk()) {
      forest_obs(new=TRUE)
    }
  })
  
  
  # Download the forest plot
  output$obsForestDownload <- downloadHandler(
    filename = function() {
      sprintf("forest.%s", gsub("cairo_", "", obsPlOpt_downloadOpts$fileType, fixed=TRUE))
    },
    content = function(file) {
      fileOptions <- list(filename=file, 
        width=obsPlOpt_downloadOpts$width, height=obsPlOpt_downloadOpts$height, 
        pointsize=obsPlOpt_downloadOpts$pointsize)
      if (obsPlOpt_downloadOpts$fileType %in% c("png", "tiff")) {
        fileOptions$res <- obsPlOpt_downloadOpts$res
        fileOptions$width <- fileOptions$width * fileOptions$res
        fileOptions$height <- fileOptions$height * fileOptions$res
        if (obsPlOpt_downloadOpts$fileType=="tiff") fileOptions$compression <- "lzw"
      }
      do.call(obsPlOpt_downloadOpts$fileType, fileOptions)
      forest_obs(pointsize=obsPlOpt_downloadOpts$pointsize, 
        spacing=obsPlOpt_downloadOpts$spacing, lwd=obsPlOpt_downloadOpts$lwd)
      dev.off()
    }
  )
  

  

  
  # Code to import meta-analysis
  observeEvent(input$obsImport, {
#     values$obsImportReady <- FALSE
    if (is.null(input$obsImport)) return()
    inFile <- input$obsImport
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
      updateSelectInput(session, paste0("obsOpt_", n), 
          selected = m$analysisOptions[[n]])
    }
    for (n in c("combFixed", "combRandom", "hakn")) {
      updateCheckboxInput(session, paste0("obsOpt_", n), 
          value = m$analysisOptions[[n]])
    }
    for (n in c("inclAbsNum", "printI2", "printQ", "printPval", "printTau2")) {
      updateCheckboxInput(session, paste0("obsPlOpt_", n), 
          value = m$plotOptions[[n]])
    }
    updateTextAreaInput(session, "obsPlOpt_advParInput", value = m$plotOptions$advParInput)
    for (n in except(names(obsPlOpt_downloadOpts), "trigger")) {
      obsPlOpt_downloadOpts[[n]] <- m$plotOptions[[n]]
    }
#     values$obsImportReady <- TRUE
  }, ignoreInit=TRUE)

  
  # Export meta-analysis
  output$obsExport <- downloadHandler(
    filename = function() {
      "miniMeta_obs.rds"
    },
    content = function(file) {
      m <- list(
        data = obs_dat(),
        meta = m(),
        analysisOptions = sapply(c("sm", "combFixed", "combRandom", 
          "methodTau", "hakn"), function(x) 
          input[[paste0("obsOpt_", x)]], simplify=FALSE
        ),
        plotOptions = c(reactiveValuesToList(obsPlOpt_downloadOpts),
          sapply(c("printI2", 
            "printQ", "printPval", "printTau2", "advParInput"), function(x)
            input[[paste0("obsPlOpt_", x)]], simplify=FALSE
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
    funnelOptions$fileType <- obsPlOpt_downloadOpts$fileType
    funnelOptions$ptCol <- input$funOpt_ptCol
    funnelOptions$posStudlab <- input$funOpt_posStudlab
  })
  
  callModule(module = funnelTab, id="obsFunnel", 
    meta = reactive(m()),
    options = funnelOptions
  )

}

