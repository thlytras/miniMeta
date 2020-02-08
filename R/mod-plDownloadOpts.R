#' UI function for the "plot download options" module
#'
#' The plot download options module is used in the RCT and observational
#' studies modules, to adjust the device options used when downloading
#' forest plots and other plots (funnel, labbe)
#'
#' @param id Module id
#' 
#' @import shiny
#' 
#' @keywords internal
#' @noRd
plDownloadOptsUI <- function(id) {
  ns <- NS(id)
  wellPanel(
    fluidRow(
      column(8, selectInput(ns("fileType"), "File type", 
        c("pdf"="cairo_pdf", "tiff"="tiff", "png"="png", "ps"="cairo_ps"))),
      column(4, sliderInput(ns("res"), "Resolution (dpi)",
        100, 1200, 600, step=100, ticks=FALSE))
    ),
    fluidRow(
      column(6, sliderInput(ns("width"), "Width (in)", 
          4, 20,10, step=1, ticks=FALSE)),
      column(6, sliderInput(ns("height"), "Height (in)", 
          3, 30, 6, step=1, ticks=FALSE))
    ),
    conditionalPanel(sprintf("input['%s']", ns("showDownloadOptionsMore")), 
      fluidRow(
        column(4, sliderInput(ns("lwd"), "Line width", 
            0.4, 3, 1, step=0.2, ticks=FALSE)),
        column(4, sliderInput(ns("spacing"), "Spacing", 
            0.5, 5, 1, step=0.25, ticks=FALSE)),
        column(4, sliderInput(ns("pointsize"), "Pointsize", 
            4, 24, 10, step=1, ticks=FALSE))
      )
    ),
    fluidRow(
      column(6, actionButton(ns("setDefaultForestSize"), "Set default size")),
      column(6, prettySwitch(ns("showDownloadOptionsMore"), "More options", FALSE))
    )
  )
}


#' Server function for the "plot download options" module
#'
#' The plot download options module is used in the RCT and observational
#' studies modules, to adjust the device options used when downloading
#' forest plots and other plots (funnel, labbe)
#'
#' @param input Shiny input parameter
#' @param output Shiny output parameter
#' @param session Shiny session object
#' @param setOpts A reactiveValues object used for "messaging" the module
#'     and sending it options to use.
#' 
#' @import shiny
#' 
#' @keywords internal
#' @noRd
plDownloadOpts <- function(input, output, session, setOpts) {

  res <- reactiveValues(fileType=NULL, width=NULL, height=NULL, pointsize=NULL,
      res=NULL, lwd=NULL, spacing=NULL, trigger=NULL)
  
  observe({
    res$fileType <- input$fileType
    res$width <- input$width
    res$height <- input$height
    res$pointsize <- input$pointsize
    res$res <- input$res
    res$lwd <- input$lwd
    res$spacing <- input$spacing
    res$trigger <- ifelse(is.null(isolate(res$trigger)), 0, isolate(res$trigger)) + 1
  })

  # Set default options
  observeEvent(input$setDefaultForestSize, {
    updateSliderInput(session, "res", value=600)
    updateSliderInput(session, "width", value=10)
    updateSliderInput(session, "height", value=6)
    updateSliderInput(session, "lwd", value=1)
    updateSliderInput(session, "spacing", value=1)
    updateSliderInput(session, "pointsize", value=10)
  })  

  observe({
    if (!is.null(setOpts)) {
    opts <- reactiveValuesToList(setOpts)
      if (sum(sapply(opts, is.null))==0) {
        updateSelectInput(session, "fileType", selected = opts$fileType)
        for (x in c("res","width","height","lwd","spacing","pointsize"))
        updateSliderInput(session, x, value = opts[[x]])
      }
    }
  })

  return(res)

}
