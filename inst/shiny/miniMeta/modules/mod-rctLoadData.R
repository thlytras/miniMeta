library(rhandsontable)
library(readxl)


rctLoadDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns('rctsLoadExcel'), 'Load an Excel file with abstracted data',
        accept = c('application/vnd.ms-excel', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')),
    helpText("or place your values here:", style="font-weight:bold"),
    rHandsontableOutput(ns("rctsTabWidget")),
    splitLayout(
      actionButton(ns("addRowToRctsTabWidget"), "Add rows"),
      actionButton(ns("trimRctsTabWidget"), "Clear empty rows"),
      downloadButton(ns("rctsSaveExcel"), "Save as Excel"),
      cellArgs = list(style = "padding: 6px; text-align:center")
    )
  )
}


rctLoadData <- function(input, output, session, dataset = NULL) {

  # Helper function
  getNonEmptyDFrows <- function(dat, ignore.studlab=FALSE) {
    if (ignore.studlab) {
      apply(dat[,2:5], 1, function(x) !sum(is.na(unlist(x)))==4)
    } else {
      apply(dat[,1:5], 1, function(x) (sum(is.na(unlist(x))) + sum(unlist(x)=="", na.rm=TRUE))<5)
    }
  }

  # Helper function
  formatRctDat <- function(tempDat) {
    while(ncol(tempDat)<6) {
      tempDat <- cbind(tempDat, NA)
    }
    tempDat <- tempDat[,1:6]
    tempDat[,1] <- as.character(tempDat[,1])
    suppressWarnings(for (i in 2:5) tempDat[,i] <- as.numeric(tempDat[,i]))
    tempDat[,6] <- as.character(tempDat[,6])
    tempDat <- tempDat[getNonEmptyDFrows(tempDat),]
    names(tempDat) <- c("Study", "events.Intervention", "N.Intervention", "events.Control", "N.Control", "Group")
    tempDat
  }

  # Load some data in advance!
  rctsDAT <- as.data.frame(read_excel("examples/RCTs-template.xls"), stringsAsFactors=FALSE)
  rctsDAT$group <- ""
  names(rctsDAT) <- c("Study", "events.Intervention", "N.Intervention", "events.Control", "N.Control", "Group")

  values <- reactiveValues(
    rctsDAT = rctsDAT,
    rctsFileReady = FALSE
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
    tempDat <- try(as.data.frame(read_excel(inFile$datapath), stringsAsFactors=FALSE), silent=TRUE)
    if (length(tempDat)==1 && class(tempDat)=="try-error") {
      showModal(modalDialog(title = "Whoops...", 
        "Error while trying to read this file.", br(), "Is it an actual Excel file?", 
        footer = modalButton("OK, got it"), size="s"))
      return()
    }
    tempDat <- formatRctDat(tempDat)
    rctsDAT <<- tempDat
    if(!is.data.frame(rctsDAT)) return()
    if (!is.na(rev(rctsDAT[,2])[1])) {
      dummy <- rctsDAT[1:(nrow(rctsDAT)+1),]
      rownames(dummy) <- NULL
      rctsDAT <<- dummy
    }
    values$rctsFileReady <- TRUE
  }, ignoreInit=TRUE)
  
  observe({
    if (!is.null(dataset())) {
      tempDat <- dataset()[,1:6]
      names(tempDat) <- c("Study", "events.Intervention", "N.Intervention", "events.Control", "N.Control", "Group")
      rctsDAT <<- tempDat
      if(!is.data.frame(rctsDAT)) return()
      if (!is.na(rev(rctsDAT[,2])[1])) {
        dummy <- rctsDAT[1:(nrow(rctsDAT)+1),]
        rownames(dummy) <- NULL
        rctsDAT <<- dummy
      }
      values$rctsFileReady <- TRUE
    }
  })
  
  # Code to render the table in the widget, if values have changed
  output$rctsTabWidget <- renderRHandsontable({
    if (values$rctsFileReady) {
      values$rctsFileReady <- FALSE
    }
    rhandsontable(rctsDAT, stretchH="all", rowHeaders=NULL, overflow="hidden") %>% 
      hot_col("events.Intervention", format="0") %>% hot_col("N.Intervention", format="0") %>% 
      hot_col("events.Control", format="0") %>% hot_col("N.Control", format="0")  %>% hot_col("Group")
  })
  
  # Code to add rows to the widget
  observeEvent(input$addRowToRctsTabWidget, {
    dummy <- rctsDAT[1:(nrow(rctsDAT)+1),]
    rownames(dummy) <- NULL
    rctsDAT <<- dummy
    values$rctsFileReady <- TRUE
  }, ignoreInit=TRUE)

  # Clear empty rows from TabWidget
  observeEvent(input$trimRctsTabWidget, {
    dummy <- rctsDAT
    dummy <- dummy[getNonEmptyDFrows(dummy),]
    dummy <- dummy[1:(nrow(dummy)+1),]
    rownames(dummy) <- NULL
    rctsDAT <<- dummy
    values$rctsFileReady <- TRUE
  }, ignoreInit=TRUE)  
  
  # Download data as Excel
  output$rctsSaveExcel <- downloadHandler(
    filename = function() {
      "studies.xls"
    },
    content = function(file) {
      dummy <- rcts_dat()
      names(dummy) <- c("Study", "events.Intervention", "N.Intervention", "events.Control", "N.Control", "Group")
      WriteXLS(dummy, file, "RCTs")
    }
  )

  # REACTIVE: return the table if it has changed
  rcts_dat <- reactive({
    datt <- values$rctsDAT
    colnames(datt) <- c("Study", "e.e", "n.e", "e.c", "n.c", "group")
    datt[getNonEmptyDFrows(datt, ignore.studlab=FALSE),]
  })

  return(reactive({ rcts_dat() }))

}
