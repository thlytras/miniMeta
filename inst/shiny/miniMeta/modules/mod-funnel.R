funnelTabUI <- function(id) {
  ns <- NS(id)
  tabPanel("Funnel plot", 
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


funnelTab <- function(input, output, session, meta, fileType) {

  # Download the funnel plot
  output$funnelDownload <- downloadHandler(
    filename = function() {
      sprintf("funnel.%s", gsub("cairo_", "", fileType(), fixed=TRUE))
    },
    content = function(file) {
      fileOptions <- list(filename=file, 
        width=7, height=7)
      if (fileType() %in% c("png", "tiff")) {
        fileOptions$width <- fileOptions$width * 400
        fileOptions$height <- fileOptions$height * 400
        fileOptions$res <- 400
        if (fileType()=="tiff") fileOptions$compression <- "lzw"
      }
      do.call(fileType(), fileOptions)
      if (inherits(meta(), "meta")) {
        funnel(meta())
      }
      dev.off()
    }
  )


  # REACTIVE: render the forest plot
  output$funnelPlot <- renderPlot({
    if (inherits(meta(), "meta")) {
      funnel(meta())
    }
  })


}
