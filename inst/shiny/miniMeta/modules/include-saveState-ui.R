source("modules/miniFileInput.R")

addResourcePath("js", "www")

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
      tags$script(src = "js/js.cookie.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "mine.css"),
      tags$link(rel="shortcut icon", href="logo-small.png")
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

