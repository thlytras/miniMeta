#' UI function for the funnel tab module
#'
#' @param id Module id
#' 
#' @import shiny
#' 
#' @keywords internal
#' @noRd
funnelTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    splitLayout(
        downloadButton(ns("funnelDownload"), "Download plot"),
        cellArgs = list(style = "padding: 6px; text-align:center")
    ),
    wellPanel(
        plotOutput(ns("funnelPlot")),
        verbatimTextOutput(ns("BeggAndEgger")),
        style="background:white"
    )
  )
}


#' Server function for the funnel tab module
#'
#' @param input Shiny input parameter
#' @param output Shiny output parameter
#' @param session Shiny session object
#' 
#' @import shiny
#' @import meta
#' @importFrom grDevices dev.off
#' 
#' @keywords internal
#' @noRd
funnelTab <- function(input, output, session, meta, options, labbe=FALSE) {

  piformat <- function (x, html = FALSE) {
    res <- x
    res[which(res < 0 | res > 1)] <- NA
    res[which(res >= 0.05)] <- round(res[which(res >= 0.05)], 
        2)
    res[which(res < 0.05 & res >= 0.001)] <- round(res[which(res < 
        0.05 & res >= 0.001)], 3)
    res[which(res < 0.001)] <- ifelse(html, "p &lt; 0.001", "p < 0.001")
    res[if (html) 
        (res != "p &lt; 0.001")
    else (res != "p < 0.001")] <- paste("p = ", res[res != "p < 0.001"], 
        sep = "")
    return(res)
  }


  drawPlot <- function() {
    if (labbe) # suppressWarnings, in case meta version does not support pos.studlab
      suppressWarnings(labbe(meta(),
        studlab=options$showStudlab, pos.studlab=options$posStudlab, 
        col=options$ptCol, bg=options$ptCol)) 
    else 
      funnel(meta(), 
        studlab=options$showStudlab, pos.studlab=options$posStudlab, 
        col=options$ptCol, bg=options$ptCol)
  }
  
  # Download the funnel plot
  output$funnelDownload <- downloadHandler(
    filename = function() {
      sprintf("%s.%s", 
        if (labbe) "labbe" else "funnel", 
        gsub("cairo_", "", options$fileType, fixed=TRUE))
    },
    content = function(file) {
      fileOptions <- list(filename=file, 
        width=7, height=6)
      if (options$fileType %in% c("png", "tiff")) {
        fileOptions$width <- fileOptions$width * 400
        fileOptions$height <- fileOptions$height * 400
        fileOptions$res <- 400
        if (options$fileType=="tiff") fileOptions$compression <- "lzw"
      }
      do.call(options$fileType, fileOptions)
      if (inherits(meta(), "meta")) drawPlot()
      dev.off()
    }
  )

  # REACTIVE: render the plot
  output$funnelPlot <- renderPlot({
    if (inherits(meta(), "meta")) drawPlot()
  })

  output$BeggAndEgger <- renderText({
    if (!labbe && inherits(meta(), "meta") && meta()$k>=3) {
      return(sprintf("Begg & Mamzudar test: %s\n          Egger test: %s%s",
        piformat(metabias(meta(), "rank", k.min=3)$p.value), 
        piformat(metabias(meta(), "linreg", k.min=3)$p.value),
        if (meta()$k<10) "\n\nWarning: number of studies lower than the recommended ten." else ""
      ))
    }
  })
  
}


#' Helper UI function to draw options controlling the funnel plot
#'
#' This is called from the RCT and observational studies modules, 
#' separately from the funnel tab module, to draw UI controls for 
#' various funnel plot options. These are then passed towards the 
#' funnel tab module.
#'
#' @param id Namespace id of the module that calls this function
#' 
#' @import shiny
#' 
#' @keywords internal
#' @noRd
funnelOptsUi <- function(ns) {
  tagList(
    fluidRow(
      column(6, 
        awesomeCheckbox(ns("funOpt_showStudlab"), "Funnel plot: show study labels", FALSE),
        selectInput(ns("funOpt_posStudlab"), "Funnel plot: study label position",
          c("Top"=3, "Bottom"=1, "Left"=2, "Right"="4"))
      ),
      column(6, 
        colourpicker::colourInput(ns("funOpt_ptCol"), "Funnel plot: color for points", "#A9A9A9")
      )
    )
  )
}
