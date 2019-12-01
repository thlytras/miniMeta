library(rhandsontable)
library(readxl)


obsLoadDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns('obsLoadExcel'), 'Load an Excel file with abstracted data',
        accept = c('application/vnd.ms-excel', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')),
    helpText("or place your values here:", style="font-weight:bold"),
    rHandsontableOutput(ns("obsTabWidget")),
    splitLayout(
      actionButton(ns("addRowToRctsTabWidget"), "Add rows"),
      actionButton(ns("trimRctsTabWidget"), "Clear empty rows"),
      downloadButton(ns("obsSaveExcel"), "Save as Excel"),
      cellArgs = list(style = "padding: 6px; text-align:center")
    ),
#     conditionalPanel(sprintf("output['%s'] != ''", ns("msgpanel")),
    conditionalPanel(sprintf("document.getElementById('%s').innerHTML != ''", ns("msgpanel")),
              wellPanel(uiOutput(ns("msgpanel")))
    )
  )
}


obsLoadData <- function(input, output, session, dataset = NULL, logMeasure = TRUE) {

  # Helper function
  getNonEmptyDFrows <- function(dat, ignore.studlab=FALSE) {
    if (ignore.studlab) {
      apply(dat[,2:5], 1, function(x) !sum(is.na(unlist(x)))==4)
    } else {
      apply(dat[,1:5], 1, function(x) (sum(is.na(unlist(x))) + sum(unlist(x)=="", na.rm=TRUE))<5)
    }
  }
  
  # Helper function
  formatObsDat <- function(tempDat, log=TRUE) {
    while(ncol(tempDat)<6) {
      tempDat <- cbind(tempDat, NA)
    }
    tempDat <- tempDat[,1:6]
    tempDat[,1] <- as.character(tempDat[,1])
    suppressWarnings(for (i in 2:5) tempDat[,i] <- as.numeric(tempDat[,i]))
    tempDat[,6] <- as.character(tempDat[,6])
    names(tempDat) <- c("Study", "Eff.measure", "95CI.LL", "95CI.UL", "SE", "Group")
    if (log) {
      tempDat$SE <- ifelse(is.na(tempDat$SE), with(tempDat, (log(`95CI.UL`)-log(`95CI.LL`))/(2*qnorm(0.975))), tempDat$SE)
    } else {
      tempDat$SE <- ifelse(is.na(tempDat$SE), with(tempDat, (`95CI.UL`-`95CI.LL`)/(2*qnorm(0.975))), tempDat$SE)
    }
    if (!is.na(rev(tempDat[,2])[1])) {
      tempDat <- tempDat[1:(nrow(tempDat)+1),]
      rownames(tempDat) <- NULL
    }
    tempDat
  }

  # Load some data in advance!
  obsDAT <- formatObsDat(as.data.frame(read_excel("examples/obs-template.xls"), stringsAsFactors=FALSE), log=TRUE)

  values <- reactiveValues(
    obsDAT = obsDAT,
    obsJustRendered = FALSE
  )
  
  observe({
    if (!is.null(input$obsTabWidget) && !isolate(values$obsJustRendered)) {
      values$obsDAT <- formatObsDat(hot_to_r(input$obsTabWidget), log=isolate(logMeasure()))
    } else {
      values$obsJustRendered <- FALSE
    }
  })
  
  
  # Code to load an Excel file
  observeEvent(input$obsLoadExcel, {
    values$obsFileReady <- FALSE
    if (is.null(input$obsLoadExcel)) return()
    inFile <- input$obsLoadExcel
    tempDat <- try(as.data.frame(read_excel(inFile$datapath), stringsAsFactors=FALSE), silent=TRUE)
    if (length(tempDat)==1 && class(tempDat)=="try-error") {
      showModal(modalDialog(title = "Whoops...", 
        "Error while trying to read this file.", br(), "Is it an actual Excel file?", 
        footer = modalButton("OK, got it"), size="s"))
      return()
    }
    tempDat <- formatObsDat(tempDat, log=logMeasure())
    tempDat <- tempDat[getNonEmptyDFrows(tempDat),]
    tempDat <- tempDat[1:(nrow(tempDat)+1),]
    values$obsDAT <- tempDat
  }, ignoreInit=TRUE)
  
  observe({
    if (!is.null(dataset())) {
      tempDat <- dataset()[,1:6]
      names(tempDat) <- c("Study", "Eff.measure", "95CI.LL", "95CI.UL", "SE", "Group")
      values$obsDAT <- formatObsDat(tempDat, log=logMeasure())
    }
  })
  
  # Code to render the table in the widget, if values have changed
  output$obsTabWidget <- renderRHandsontable({
    values$obsJustRendered <- TRUE
    rhandsontable(values$obsDAT, stretchH="all", rowHeaders=NULL, overflow="hidden") %>% 
      hot_col("Eff.measure", format="0,00") %>% hot_col("95CI.LL", format="0,00") %>% 
      hot_col("95CI.UL", format="0,00") %>% hot_col("SE", format="0,00") %>% hot_col("Group")
  })
  
  # Code to add rows to the widget
  observeEvent(input$addRowToRctsTabWidget, {
    values$obsDAT <- values$obsDAT[1:(nrow(values$obsDAT)+1),]
  }, ignoreInit=TRUE)

  # Clear empty rows from TabWidget
  observeEvent(input$trimRctsTabWidget, {
    dummy <- values$obsDAT
    dummy <- dummy[getNonEmptyDFrows(dummy),]
    dummy <- dummy[1:(nrow(dummy)+1),]
    rownames(dummy) <- NULL
    values$obsDAT <- dummy
  }, ignoreInit=TRUE)  
  
  # Download data as Excel
  output$obsSaveExcel <- downloadHandler(
    filename = function() {
      "studies.xls"
    },
    content = function(file) {
      dummy <- obs_dat()
      names(dummy) <- c("Study", "Eff.measure", "95CI.LL", "95CI.UL", "SE", "Group")
      WriteXLS(dummy, file, "RCTs")
    }
  )
  
  # REACTIVE: render the messaging panel
  output$msgpanel <- renderUI({
    if (logMeasure()) {
      calcSE <- (log(values$obsDAT[,4]) - log(values$obsDAT[,3]))/(2*qnorm(0.975))
      symm <- abs(log(values$obsDAT[,4]) - log(values$obsDAT[,2]))/abs(log(values$obsDAT[,2]) - log(values$obsDAT[,3]))
    } else {
      calcSE <- (values$obsDAT[,4] - values$obsDAT[,3])/(2*qnorm(0.975))
      symm <- abs(values$obsDAT[,4] - values$obsDAT[,2])/abs(values$obsDAT[,2] - values$obsDAT[,3])
    }
    i.seOff <- which(abs(calcSE-values$obsDAT[,5])>0.01)
    i.asymm <- which((abs(symm)-1)>0.05)
    if (length(i.seOff)+length(i.asymm)==0) return(NULL)
    res <- ""
    if (length(i.seOff)>0) {
      res <- paste0(res, 
        "The standard errors in these studies do not match the confidence intervals: ",
        as.character(strong(paste(values$obsDAT[i.seOff,1], collapse=", "))), "<br/>",
        as.character(em("--> Maybe delete the SE column and allow miniMeta to recompute it?")), 
        "<br/><br/>")
    }
    if (length(i.asymm)) {
      res <- paste0(res, 
        "The confidence intervals for these studies do not appear symmetric: ",
        as.character(strong(paste(values$obsDAT[i.asymm,1], collapse=", "))), "<br/>",
        as.character(em("--> Make sure the data you've entered are correct.")), "<br/>")
    }
    HTML(res)
  })
  
  outputOptions(output, "msgpanel", suspendWhenHidden=FALSE)

  # REACTIVE: return the table if it has changed
  obs_dat <- reactive({
    datt <- values$obsDAT
    colnames(datt) <- c("Study", "TE", "LL", "UL", "seTE", "group")
    datt[getNonEmptyDFrows(datt, ignore.studlab=FALSE),]
  })

  return(reactive({ obs_dat() }))

}
