#' Main Shiny UI function for miniMeta
#' 
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjs extendShinyjs
#'
#' @keywords internal
#' @noRd
miniMetaUI <- function() {
  fluidPage(
    saveState_header(),
    titlePanel(span(
      img(src="www/logo-large.svg", style="height:3em; margin-right:1em;"),
      div(span("A simple tool to run meta-analyses,"), br(), span("with a focus on GRADE SoF tables"), style="display:inline-block; font-size:50%;")
    ), "miniMeta"),
    tabsetPanel(
      rct_moduleUI(id = "rctModule"),
      obs_moduleUI(id = "obsModule"),
      tabPanel("Tools", br(),
        tabsetPanel(
          bucher_moduleUI(id = "bucher"),
          samplesize_moduleUI(id = "samplesize")
        )
      )
    )
  )
}


#' UI function elements that handle storage/retrieval of state via cookies
#'
#' This function is called from the miniMeta main UI function
#' \code{\link{miniMetaUI}} and contains the UI part that handles storage 
#' and retrieval of state via cookies and javascript. 
#' It depends on the \code{shinyjs} package.
#' 
#' Its counterpart for the main \emph{server} function 
#' \code{\link{miniMetaServer}} is the 
#' \code{\link{include_saveState_serverCode}} function. 
#' Also see function \code{\link{ui_importer}} in the 
#' \code{components-modUniv-ui.R} script.
#'
#' @seealso \code{\link{include_saveState_serverCode}}
#'
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjs extendShinyjs
#'
#' @keywords internal
#' @noRd
saveState_header <- function() {
  jsCode <- '
    shinyjs.getcookie = function(params) {
        var cookie = Cookies.get("miniMeta_prefs");
        Shiny.onInputChange("jscookie", cookie);
    }
    shinyjs.setcookie = function(params) {
        Cookies.set("miniMeta_prefs", escape(params), { expires: 900 });  
        Shiny.onInputChange("jscookie", params);
    }
    shinyjs.rmcookie = function(params) {
        Cookies.remove("miniMeta_prefs");
        Shiny.onInputChange("jscookie", "");
    }
  '
  tagList(
    tags$head(
      tags$script(src = "www/js.cookie.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "www/mine.css"),
      tags$link(rel= "shortcut icon", href="www/logo-small.png")
    ),
    useShinyjs(),
    extendShinyjs(text = jsCode, functions=c("getcookie","setcookie","rmcookie")),
    div(
      miniFileInput("loadStateFromFile", "Load state from file", accept = c('application/octet-stream')),
      downloadButton("saveStateToFile", "Save state to file"), 
      style="visibility:hidden; height: 0px"
    )
  )
}


