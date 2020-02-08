#' Common code between the RCT and observational modules
#' 
#' This function is NOT called internally; its body (the code) is 
#' \code{\link[base]{eval}}uated inside the server functions of the 
#' RCT and observational modules. It contains the code that is common
#' between the two modules.
#'
#' The code uses a local variable \code{mtype} to adjust its behaviour, 
#' which takes a value of 1 for the RCT module and 2 for the observational
#' module. It also uses two reactive expressions \code{dat} and \code{chk},
#' defined differently in each module, as well as a \code{stateEvent} 
#' \code{\link[shiny]{reactiveVal}} used to signal saving/loading of state.
#'
#' @param input Shiny input parameter
#' @param output Shiny output parameter
#' @param session Shiny session object
#' @param mtype 
#' @param dat 
#' @param chk 
#' @param stateEvent 
#'
#' @importFrom methods formalArgs
#' @import shiny
#'
#' @keywords internal
#' @noRd
include_modUniv_serverCode <- function(input, output, session, mtype, dat, chk, stateEvent) {

  # List of forest.meta() arguments, excluding some that we don't want the user to touch
  forest_args <- formalArgs(meta::forest.meta)
  forest_args <- forest_args[!(forest_args %in% 
    c("...", "x", "comb.random", "comb.fixed", "layout", "new"))]

  values <- reactiveValues(
    importReady = FALSE,
    dataset = NULL
  )

  # REACTIVE: get all plot options in a list
  pltOpt <- reactive({
    lcols <- c("studlab")
    if (mtype==1 && input$plOpt_inclAbsNum) lcols <- c(lcols, "event.e", "n.e", "event.c", "n.c")
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
  
  except <- function(x, y) {
    x[!(x %in% y)]
  }

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
  
  output$plOpt_advParOutput <- renderText({
    if (class(pltAdvOpt())=="try-error") return(as.character(attr(pltAdvOpt(), "condition")))
    if (length(pltAdvOpt())==0) return("No extra parameters provided (or parameters unknown to forest.meta())")
    return(gsub("^list\\(|\\)$", "", deparse(pltAdvOpt())))
  })

  
  output$forestPlotUI <- renderUI({
    nr <- nrow(dat())
    if (!is.numeric(nr)) nr <- 5
    plotOutput(session$ns("forestPlot"), height=paste0(12 + 1.1*nr, "em"), width="100%")
  })

  # REACTIVE: render the forest plot
  output$forestPlot <- renderPlot({
    if (chk()) {
      draw_forest(new=TRUE)
    }
  })

  draw_forest <- function(new=TRUE, pointsize=12, lwd=1, spacing=1) {
    cilayout("(", " - ")
    pars <- c(list(x=m(), new=new), pltOpt(),
      list(
        text.fixed = "Fixed-effects model",
        text.random = "Random-effects model",
        print.byvar = FALSE,
        fontsize = pointsize,
        plotwidth = sprintf("%.2fcm", 8*pointsize/12),
        colgap = sprintf("%.2fmm", 2*pointsize/12),
        lwd = lwd,
        spacing = spacing
      ))
    pars <- pars[!duplicated(names(pars))]
    do.call(forest, pars)
  }
  
  
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
      if (chk()) {
        draw_forest(pointsize=plOpt_downloadOpts$pointsize, 
          spacing=plOpt_downloadOpts$spacing, lwd=plOpt_downloadOpts$lwd)
      }
      dev.off()
    }
  )


  # Code to import meta-analysis
  observeEvent(input$import, {
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
    # Does it appear to be a correct miniMeta file?
    if (is.miniMeta(m)) {
      if (mtype==1 && is.miniMeta.obs(m)) {
        showModal(modalDialog(title = "Notice:", 
          "This is indeed a miniMeta file, but it contains a meta-analysis of observational studies.",
          br(), "Please move to the Observational studies module and import it there. Thank you.", 
          footer = modalButton("OK, got it"), size="s"))
        return()      
      }
      if (mtype==2 && is.miniMeta.rct(m)) {
        showModal(modalDialog(title = "Notice:", 
          "This is indeed a miniMeta file, but it contains a meta-analysis of Randomized Controlled Trials (RCTs).",
          br(), "Please move to the RCT module and import it there. Thank you.", 
          footer = modalButton("OK, got it"), size="s"))
        return()
      }
    } else {
      showModal(modalDialog(title = "Whoops...", 
        "This is a serialized R object, but it does not appear to be a valid miniMeta file.",
        footer = modalButton("OK, got it"), size="s"))
      return()
    }
    values$dataset <- NULL
    values$dataset <- m$data
    for (n in c("sm", "method", "methodTau", if (mtype==1) "incr")) {
      updateSelectInput(session, paste0("opt_", n), 
          selected = m$analysisOptions[[n]])
    }
    for (n in c("combFixed", "combRandom", "hakn")) {
      updateAwesomeCheckbox(session, paste0("opt_", n), 
          value = m$analysisOptions[[n]])
    }
    for (n in c("inclAbsNum", "printI2", "printQ", "printPval", "printTau2")) {
      updateAwesomeCheckbox(session, paste0("plOpt_", n), 
          value = m$plotOptions[[n]])
    }
    updateTextAreaInput(session, "plOpt_advParInput", value = m$plotOptions$advParInput)
    for (n in except(names(plOpt_downloadOpts), "trigger")) {
      plOpt_downloadOpts[[n]] <- m$plotOptions[[n]]
    }
  }, ignoreInit=TRUE)

  
  makeMiniMetaObject <- function() {
    m <- list(
      data = dat(),
      meta = m(),
      analysisOptions = sapply(c("sm", "combFixed", "combRandom", 
        "method", "methodTau", "hakn", if (mtype==1) "incr"), function(x) 
        input[[paste0("opt_", x)]], simplify=FALSE
      ),
      plotOptions = c(reactiveValuesToList(plOpt_downloadOpts),
        sapply(c( if (mtype==1) "inclAbsNum",
          "printI2", "printQ", "printPval", "printTau2",
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
      if (mtype==1) "miniMeta_RCTs.rds" else "miniMeta_obs.rds"
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
  
  
  observeEvent(input$loadState, {
    stateEvent("load")
  })
  observeEvent(input$saveState, {
    stateEvent("save")
  })
  observeEvent(input$rmState, {
    stateEvent("clear")
  })
  observeEvent(input$saveStateFile, {
    stateEvent("saveFile")
  })


}
