library(shiny)
library(meta)
library(metafor)
library(readxl)
library(WriteXLS)

source("include.R")

# Load some data in advance!
rctsDAT <- as.data.frame(read_excel("RCTs-template.xls"), stringsAsFactors=FALSE)
rctsDAT$group <- ""
names(rctsDAT) <- c("Study", "events.Intervention", "N.Intervention", "events.Control", "N.Control", "Group")

# List of forest.meta() arguments, excluding some that we don't want the user to touch
forest_args <- formalArgs("forest.meta")
forest_args <- forest_args[!(forest_args %in% 
  c("...", "x", "comb.random", "comb.fixed", "layout", "new"))]


shinyServer(function(input, output, session) {
  
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
  
  # REACTIVE: render the forest plot
  output$rctsForestPlot <- renderPlot({
    if (rcts_chk()) {
      forest_rct(new=TRUE)
    }
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
        width=input$rctPlOpt_width, height=input$rctPlOpt_height, 
        pointsize=input$rctPlOpt_pointsize)
      if (input$rctPlOpt_fileType %in% c("png", "tiff")) {
        fileOptions$res <- input$rctPlOpt_res
        fileOptions$width <- fileOptions$width * fileOptions$res
        fileOptions$height <- fileOptions$height * fileOptions$res
        if (input$rctPlOpt_fileType=="tiff") fileOptions$compression <- "lzw"
      }
      do.call(input$rctPlOpt_fileType, fileOptions)
      forest_rct(pointsize=input$rctPlOpt_pointsize, 
        spacing=input$rctPlOpt_spacing, lwd=input$rctPlOpt_lwd)
      dev.off()
    }
  )
  
  # Clear empty rows from TabWidget
  observeEvent(input$setDefaultForestSize, {
    updateSliderInput(session, "rctPlOpt_width", value=10)
    updateSliderInput(session, "rctPlOpt_height", value=6)
    updateSliderInput(session, "rctPlOpt_pointsize", value=10)
    updateSliderInput(session, "rctPlOpt_res", value=300)
    updateSliderInput(session, "rctPlOpt_lwd", value=1)
    updateSliderInput(session, "rctPlOpt_spacing", value=1)
  })  

  
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
      names(dummy) <- c("Study", "events.Intervention", "N.Intervention", "events.Control", "N.Control", "Group")
      WriteXLS(dummy, file, "RCTs")
    }
  )

  
  # Code to import meta-analysis
  observeEvent(input$rctsImport, {
    values$rctsImportReady <- FALSE
    if (is.null(input$rctsImport)) return()
    inFile <- input$rctsImport
    m <- readRDS(inFile$datapath)
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
    updateSelectInput(session, "rctPlOpt_fileType", 
        selected = m$plotOptions$fileType)
    updateSliderInput(session, "rctPlOpt_res", 
        value = m$plotOptions$res)
    updateSliderInput(session, "rctPlOpt_width", 
        value = m$plotOptions$width)
    updateSliderInput(session, "rctPlOpt_height", 
        value = m$plotOptions$height)
    updateSliderInput(session, "rctPlOpt_lwd", 
        value = m$plotOptions$lwd)
    updateSliderInput(session, "rctPlOpt_spacing", 
        value = m$plotOptions$spacing)
    updateSliderInput(session, "rctPlOpt_pointsize", 
        value = m$plotOptions$pointsize)
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
        plotOptions = sapply(c("fileType", "res", "width", "height", 
          "lwd", "spacing", "pointsize", "inclAbsNum", "printI2", 
          "printQ", "printPval", "printTau2", "advParInput"), function(x)
          input[[paste0("rctPlOpt_", x)]], simplify=FALSE
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

  
  # REACTIVE: Output for Bucher method
  output$buch_output <- renderPrint({
    eff <- as.numeric(c(input$buch_AC_est, input$buch_AC_lo, input$buch_AC_hi,
        input$buch_BC_est, input$buch_BC_lo, input$buch_BC_hi))
    if (sum(is.na(eff))==0) {
      # Make sure we're not trying to logarithmize negative values
        if (input$buch_type=="exp" & sum(eff<0, na.rm=TRUE)>0) return(cat(" "))
      if (input$buch_type=="exp") eff <- log(eff)
      eff <- matrix(eff, nrow=2, byrow=TRUE)
      colnames(eff) <- c("est", "lo", "hi"); rownames(eff) <- c("AC", "BC")
      pool.eff <- diff(eff[c("BC","AC"), "est"])
      pool.sd <- sqrt(
        (abs(diff(eff["AC", c("lo","hi")]))/(2*qnorm(0.975)))^2 + 
        (abs(diff(eff["BC", c("lo","hi")]))/(2*qnorm(0.975)))^2)
      symm.ac <- abs(abs(diff(eff["AC", c("est","hi")]))/abs(diff(eff["AC", c("lo","est")]))-1)<0.05
      symm.bc <- abs(abs(diff(eff["BC", c("est","hi")]))/abs(diff(eff["BC", c("lo","est")]))-1)<0.05
      output <- c(
        if (input$buch_type=="exp") "Exponentiated measure - converting to log:\n" else "",
        sprintf("Point estimate, A vs C: %.3f", eff["AC", "est"]),
        sprintf("Standard error, A vs C: %.3f", 
          abs(diff(eff["AC", c("lo","hi")]))/(2*qnorm(0.975))),
        sprintf("\nPoint estimate, B vs C: %.3f", eff["BC", "est"]),
        sprintf("Standard error, B vs C: %.3f",
          abs(diff(eff["BC", c("lo","hi")]))/(2*qnorm(0.975))),
        sprintf("\nPoint estimate, A vs B: %.3f", pool.eff),
        sprintf("Standard error, A vs B: %.3f", pool.sd),
        sprintf("95%% Confidence Interval, A vs B: %.3f — %.3f", 
          pool.eff-qnorm(0.975)*pool.sd, pool.eff+qnorm(0.975)*pool.sd),
        (if (input$buch_type=="exp") paste(
            "\nExponentiating:",
            sprintf("Point estimate, A vs B: %.2f", exp(pool.eff)),
            sprintf("95%% Confidence Interval, A vs B: %.2f — %.2f",
              exp(pool.eff-qnorm(0.975)*pool.sd), exp(pool.eff+qnorm(0.975)*pool.sd)
            ), sep="\n") else ""), "")
      if (!symm.ac) output <- c(output, 
        "WARNING: The 95% CI for A vs C does not look symmetric.",
        "         Are the values you entered correct?")
      if (!symm.bc) output <- c(output, 
        "WARNING: The 95% CI for B vs C does not look symmetric.",
        "         Are the values you entered correct?")
      return(cat(paste(output, collapse="\n")))
    } else return(cat(" "))
  })

})
