library(rhandsontable)
library(readxl)

source("modules/include.R")

# Load some data in advance!
rctsDAT <- as.data.frame(read_excel("RCTs-template.xls"), stringsAsFactors=FALSE)
rctsDAT$group <- ""
names(rctsDAT) <- c("Study", "events.Intervention", "N.Intervention", "events.Control", "N.Control", "Group")



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

  values <- reactiveValues(
    rctsDAT = rctsDAT,
    rctsImportReady = FALSE,
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
    while(ncol(tempDat)<6) {
      tempDat <- cbind(tempDat, NA)
    }
    tempDat <- tempDat[,1:6]
    tempDat[,1] <- as.character(tempDat[,1])
    suppressWarnings(for (i in 2:5) tempDat[,i] <- as.numeric(tempDat[,i]))
    tempDat[,6] <- as.character(tempDat[,6])
    tempDat <- tempDat[getNonEmptyDFrows(tempDat),]
    names(tempDat) <- c("Study", "events.Intervention", "N.Intervention", "events.Control", "N.Control", "Group")
    rctsDAT <<- tempDat
    if(!is.data.frame(rctsDAT)) return()
    values$rctsFileReady <- TRUE
  })
  
  observe({
    if (!is.null(dataset())) {
      tempDat <- dataset()[,1:6]
      names(tempDat) <- c("Study", "events.Intervention", "N.Intervention", "events.Control", "N.Control", "Group")
      rctsDAT <<- tempDat
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
  })

  # REACTIVE: return the table if it has changed
  rcts_dat <- reactive({
    datt <- values$rctsDAT
    colnames(datt) <- c("Study", "e.e", "n.e", "e.c", "n.c", "group")
    datt[getNonEmptyDFrows(datt, ignore.studlab=FALSE),]
  })

#   return(rcts_dat())

  # Clear empty rows from TabWidget
  observeEvent(input$trimRctsTabWidget, {
    dummy <- rctsDAT
    dummy <- dummy[getNonEmptyDFrows(dummy),]
    dummy <- dummy[1:(nrow(dummy)+1),]
    rownames(dummy) <- NULL
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
      names(dummy) <- c("Study", "events.Intervention", "N.Intervention", "events.Control", "N.Control", "Group")
      WriteXLS(dummy, file, "RCTs")
    }
  )

  # REACTIVE: return the table if it has changed
#   return(reactive({
#     datt <- values$rctsDAT
#     colnames(datt) <- c("Study", "e.e", "n.e", "e.c", "n.c", "group")
#     datt[getNonEmptyDFrows(datt, ignore.studlab=FALSE),]
#   }))
  return(reactive({ rcts_dat() }))

}
