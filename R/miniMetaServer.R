#' Main Shiny server function for miniMeta
#'
#' @param input Shiny input parameter
#' @param output Shiny output parameter
#' @param session Shiny session object
#' 
#' @import shiny
#' 
#' @keywords internal
#' @noRd
miniMetaServer <- function(input, output, session) {
  
  stateEvent <- reactiveVal()
  
  # Include the code required to be able to save state, 
  #   found in script include-saveState_serverCode.R
  eval(body(include_saveState_serverCode))
    
  stateEvent <- callModule(module = rct_module, id = "rctModule", stateEvent)  
  stateEvent <- callModule(module = obs_module, id = "obsModule", stateEvent)  
  callModule(module = bucher_module, id = "bucher")
  callModule(module = samplesize_module, id = "samplesize")
  
}
