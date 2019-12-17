funnelOptsUi <- function(ns) {
  tagList(
    checkboxInput(ns("funOpt_showStudlab"), "Funnel plot: show study labels", FALSE),
    colourInput(ns("funOpt_ptCol"), "Funnel plot: color for points", "#A9A9A9")
  )
}


funnelTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    splitLayout(
        downloadButton(ns("funnelDownload"), "Download plot"),
        cellArgs = list(style = "padding: 6px; text-align:center")
    ),
    wellPanel(
        plotOutput(ns("funnelPlot")),
        style="background:white"
    )
  )
}


funnelTab <- function(input, output, session, meta, options, labbe=FALSE) {

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
      if (inherits(meta(), "meta")) {
        if (labbe)
          labbe(meta(), studlab=options$showStudlab, 
            col=options$ptCol, bg=options$ptCol)
        else 
          funnel(meta(), studlab=options$showStudlab, 
            col=options$ptCol, bg=options$ptCol)
      }
      dev.off()
    }
  )
  

  # REACTIVE: render the forest plot
  output$funnelPlot <- renderPlot({
    if (inherits(meta(), "meta")) {
      if (labbe)
        labbe(meta(), studlab=options$showStudlab, 
          col=options$ptCol, bg=options$ptCol)
      else 
        funnel(meta(), studlab=options$showStudlab, 
          col=options$ptCol, bg=options$ptCol)
    }
  })

}
