library(shiny)
library(meta)
library(metafor)
library(readxl)
library(WriteXLS)

source("include.R")

# Load some data in advance!
rctsDAT <- as.data.frame(read_excel("RCTs-template.xls"), stringsAsFactors=FALSE)
names(rctsDAT) <- c("Study", "events.Intervention", "N.Intervention", "events.Control", "N.Control")

# Default sizes for forest plots, by file type
defPltSize <- list(
  width = c(png=700, tiff=700, cairo_pdf=7, cairo_ps=7),
  min_width = c(png=400, tiff=400, cairo_pdf=4, cairo_ps=4),
  max_width = c(png=5000, tiff=5000, cairo_pdf=20, cairo_ps=20),
  height = c(png=900, tiff=900, cairo_pdf=9, cairo_ps=9),
  min_height = c(png=500, tiff=500, cairo_pdf=4, cairo_ps=4),
  max_height = c(png=6000, tiff=6000, cairo_pdf=20, cairo_ps=20),
  pointsize = c(png=12, tiff=12, cairo_pdf=12, cairo_ps=12),
  min_pointsize = c(png=4, tiff=4, cairo_pdf=4, cairo_ps=4),
  max_pointsize = c(png=20, tiff=20, cairo_pdf=20, cairo_ps=20)
)

# List of forest.meta() arguments, excluding some that we don't want the user to touch
forest_args <- formalArgs("forest.meta")
forest_args <- forest_args[!(forest_args %in% 
  c("...", "x", "comb.random", "comb.fixed", "layout", "new"))]


shinyServer(function(input, output, session) {
  
  values <- reactiveValues(
    rctsDAT = rctsDAT,
    rctsFileReady = FALSE# rctsTableReady = FALSE,  # tableready might not be needed,
  )
  
  observe({
    if (!is.null(input$rctsTabWidget)) {
      rctsDAT <<- hot_to_r(input$rctsTabWidget)
    }
    values$rctsDAT <- rctsDAT
    # Check if last value in the table
    if (!is.na(rev(rctsDAT[,2])[1])) {
      dummy <- rctsDAT[1:(nrow(rctsDAT)+1),]
      rownames(dummy) <- NULL
      rctsDAT <<- dummy
      values$rctsFileReady <- TRUE
    }
    
  })
  
  
  # Code to load an Excel file
  observeEvent(input$rctsLoadExcel, {
    values$rctsFileReady <- FALSE
    if (is.null(input$rctsLoadExcel)) return()
    inFile <- input$rctsLoadExcel
    tempDat <- as.data.frame(read_excel(inFile$datapath), stringsAsFactors=FALSE)
    while(ncol(tempDat)<5) {
      tempDat <- cbind(tempDat, NA)
    }
    tempDat <- tempDat[,1:5]
    tempDat[,1] <- as.character(tempDat[,1])
    suppressWarnings(for (i in 2:5) tempDat[,i] <- as.numeric(tempDat[,i]))
    tempDat <- tempDat[getNonEmptyDFrows(tempDat),]
    names(tempDat) <- c("Study", "events.Intervention", "N.Intervention", "events.Control", "N.Control")
    rctsDAT <<- tempDat
    if(!is.data.frame(rctsDAT)) return()
    values$rctsFileReady <- TRUE
  })
  
  # Code to render the table in the widget, if values have changed
  output$rctsTabWidget <- renderRHandsontable({
    if (values$rctsFileReady) {
      values$rctsFileReady <- FALSE
    }
    rhandsontable(rctsDAT, stretchH="all", rowHeaders=NULL, overflow="hidden") %>% 
      hot_col("events.Intervention", format="0") %>% hot_col("N.Intervention", format="0") %>% 
      hot_col("events.Control", format="0") %>% hot_col("N.Control", format="0")
  })
  
  # Code to add rows to the widget
  observeEvent(input$addRowToRctsTabWidget, {
    dummy <- rctsDAT[1:(nrow(rctsDAT)+1),]
    rownames(dummy) <- NULL
    rctsDAT <<- dummy
    values$rctsFileReady <- TRUE
  })

  # REACTIVE: return the table if it has changed
  rcts_dat <- reactive({
    datt <- values$rctsDAT
    colnames(datt) <- c("Study", "e.e", "n.e", "e.c", "n.c")
    datt[getNonEmptyDFrows(datt, ignore.studlab=FALSE),]
  })
  
  # REACTIVE: check validity of the data
  rcts_chk <- reactive({
    checkRCTValidity(rcts_dat())
  })
  
  # REACTIVE: run the meta-analysis
  m <- reactive({
    if (rcts_chk()) {
      optIncr <- input$rctOpt_incr; if (optIncr!="TACC") optIncr <- as.numeric(optIncr)
      
      return(metabin(e.e, n.e, e.c, n.c, data=rcts_dat(), studlab=Study, 
        method=input$rctOpt_method, method.tau=input$rctOpt_methodTau,
        comb.fixed=input$rctOpt_combFixed, comb.random=input$rctOpt_combRandom,
        incr=optIncr, sm=input$rctOpt_sm, hakn=input$rctOpt_hakn
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
  
  # REACTIVE: parse all advanced plot options
  rcts_pltAdvOpt <- reactive({
    res <- readAdvParameters(input$rctPlOpt_advParInput)
    if (class(res)!="try-error" && length(res)>0) {
      res <- res[names(res) %in% forest_args]
    }
    res
  })
  
  forest_rct <- function(new=TRUE) {
    cilayout("(", " - ")
    do.call(forest, c(list(x=m(), new=new), rcts_pltOpt()))
  }
  
  # REACTIVE: render the forest plot
  output$rctsForestPlot <- renderPlot({
    if (rcts_chk()) {
      forest_rct(new=TRUE)
    }
  })
  
  # REACTIVE: render the plot dimension setting controls
  output$rctPlOpt_dims <- renderUI({
    fluidRow(
      column(4, numericInput("rctPlOpt_width", "Width", 
          defPltSize$width[input$rctPlOpt_fileType], 
          min=defPltSize$min_width[input$rctPlOpt_fileType], 
          max=defPltSize$max_width[input$rctPlOpt_fileType])),
      column(4, numericInput("rctPlOpt_height", "Height", 
          defPltSize$height[input$rctPlOpt_fileType], 
          min=defPltSize$min_height[input$rctPlOpt_fileType], 
          max=defPltSize$max_height[input$rctPlOpt_fileType])),
      column(4, numericInput("rctPlOpt_pointsize", "Pointsize", 
          defPltSize$pointsize[input$rctPlOpt_fileType], 
          min=defPltSize$min_pointsize[input$rctPlOpt_fileType], 
          max=defPltSize$max_pointsize[input$rctPlOpt_fileType]))
    )
  })
  
  # Get and filter input for plot dimensions
  rcts_pltDim <- reactive({
    res <- list(
      width = input$rctPlOpt_width,
      height = input$rctPlOpt_height,
      pointsize = input$rctPlOpt_pointsize
    )
    if (is.na(res$width)) {
      res$width <- defPltSize$width[input$rctPlOpt_fileType]
    } else if (res$width < defPltSize$min_width[input$rctPlOpt_fileType]) {
      res$width <- defPltSize$min_width[input$rctPlOpt_fileType]
    } else if (res$width > defPltSize$max_width[input$rctPlOpt_fileType]) {
      res$width <- defPltSize$max_width[input$rctPlOpt_fileType]
    }
    if (is.na(res$height)) {
      res$height <- defPltSize$height[input$rctPlOpt_fileType]
    } else if (res$height < defPltSize$min_height[input$rctPlOpt_fileType]) {
      res$height <- defPltSize$min_height[input$rctPlOpt_fileType]
    } else if (res$height > defPltSize$max_height[input$rctPlOpt_fileType]) {
      res$height <- defPltSize$max_height[input$rctPlOpt_fileType]
    }
    if (is.na(res$pointsize)) {
      res$pointsize <- defPltSize$pointsize[input$rctPlOpt_fileType]
    } else if (res$pointsize < defPltSize$min_pointsize[input$rctPlOpt_fileType]) {
      res$pointsize <- defPltSize$min_pointsize[input$rctPlOpt_fileType]
    } else if (res$pointsize > defPltSize$max_pointsize[input$rctPlOpt_fileType]) {
      res$pointsize <- defPltSize$max_pointsize[input$rctPlOpt_fileType]
    }
    res
  })
  
  output$rctPlOpt_advParOutput <- renderText({
    if (class(rcts_pltAdvOpt())=="try-error") return(as.character(attr(rcts_pltAdvOpt(), "condition")))
    if (length(rcts_pltAdvOpt())==0) return("No extra parameters provided (or parameters unknown to forest.meta())")
    return(gsub("^list\\(|\\)$", "", deparse(rcts_pltAdvOpt())))
  })
  
  output$rctsForestPlotUI <- renderUI({
    nr <- nrow(rcts_dat())
    if (!is.numeric(nr)) nr <- 5
    plotOutput("rctsForestPlot", height=paste0(12 + 1.1*nr, "em"))
  })
  
  # Download the forest plot
  output$rctsForestDownload <- downloadHandler(
    filename = function() {
      sprintf("forest.%s", gsub("cairo_", "", input$rctPlOpt_fileType, fixed=TRUE))
    },
    content = function(file) {
      fileOptions <- list(filename=file, 
        width=rcts_pltDim()$width, height=rcts_pltDim()$height, 
        pointsize=rcts_pltDim()$pointsize)
      if (input$rctPlOpt_fileType=="tiff") fileOptions$compression <- "lzw"
      do.call(input$rctPlOpt_fileType, fileOptions)
      forest_rct()
      dev.off()
    }
  )
  
  # Clear empty rows from TabWidget
  observeEvent(input$trimRctsTabWidget, {
    dummy <- rctsDAT
    dummy <- dummy[getNonEmptyDFrows(dummy),]
    dummy <- dummy[1:(nrow(dummy)+1),]
    rownames(dummy) <- NULL
    print(dummy[,1])
    rctsDAT <<- dummy
    values$rctsFileReady <- TRUE
  })  
  
  # Download data as Excel
  output$rctsSaveExcel <- downloadHandler(
    filename = function() {
      "studies.xls"
    },
    content = function(file) {
      dummy <- rcts_dat()
      names(dummy) <- c("Study", "events.Intervention", "N.Intervention", "events.Control", "N.Control")
      WriteXLS(dummy, file, "RCTs")
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
  
})
