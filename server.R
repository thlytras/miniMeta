library(shiny)
library(meta)
library(metafor)
library(readxl)
library(WriteXLS)

source("include.R")

# Load some data in advance!
rctsDAT <- as.data.frame(read_excel("RCTs-template.xls"), stringsAsFactors=FALSE)
names(rctsDAT) <- c("Study", "events.Intervention", "N.Intervention", "events.Control", "N.Control")


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
  
  forest_rct <- function(new=TRUE) {
    cilayout("(", " - ")
    forest(m(), new=new, leftcols="studlab", col.study=input$rctPlotOpt_col)
  }
  
  # REACTIVE: render the forest plot
  output$rctsForestPlot <- renderPlot({
    if (rcts_chk()) {
      forest_rct(new=TRUE)
    }
  })
  
  output$rctsForestPlotUI <- renderUI({
    nr <- nrow(rcts_dat())
    if (!is.numeric(nr)) nr <- 5
    plotOutput("rctsForestPlot", height=paste0(12 + 1.1*nr, "em"))
  })
  
  # Download the forest plot
  output$rctsForestDownload <- downloadHandler(
    filename = function() {
      "forest.pdf"
    },
    content = function(file) {
      cairo_pdf(file, width=7, height=9)
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
