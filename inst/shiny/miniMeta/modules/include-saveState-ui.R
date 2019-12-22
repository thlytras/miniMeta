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
      tags$style(HTML("#shiny-notification-panel { top: 10px; }"))
    ),
    useShinyjs(),
    extendShinyjs(text = jsCode, functions=c("getcookie","setcookie","rmcookie"))
  )
}

