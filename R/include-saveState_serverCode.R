#' miniMeta server code to handle state
#' 
#' This function is NOT called internally; its body (the code) is 
#' \code{\link[base]{eval}}uated inside the main miniMeta server 
#' function \code{\link{miniMetaServer}}. 
#' 
#' The code contains server functions to implement the saving and
#' loading of state, on browser cookies or on files. The UI counterpart
#' of this is in function \code{\link{saveState_header}} 
#' in script \code{miniMetaUI.R}
#'
#' @param input Shiny input parameter
#' @param output Shiny output parameter
#' @param session Shiny session object
#' @param stateEvent A reactiveVal that signals events to save/load state 
#'     from the modules
#'
#' @importFrom shinyjs runjs delay js
#' @importFrom colourpicker updateColourInput
#' @importFrom utils URLdecode URLencode
#' @importFrom jsonlite base64_enc base64_dec
#'
#' @keywords internal
#' @noRd
include_saveState_serverCode <- function(input, output, session, stateEvent) {

  getState <- function(encode=TRUE) {
    combine <- function(...) {
      apply(expand.grid(...), 1, paste, collapse="-")
    }    
    state <- list(
      sliders = sapply(combine(c("obsModule","rctModule"), "downloadOpts", 
        c("height", "lwd", "pointsize", "res", "spacing", "width")), 
        function(x) input[[x]]),
      text = sapply(combine(c("obsModule","rctModule"), 
        c("plOpt_advParInput")), function(x) input[[x]]),
      numeric = sapply(combine(c("obsModule"), 
        c("baseRisk")), function(x) input[[x]]),
      select = sapply(c(
          combine(c("obsModule","rctModule"), "downloadOpts", "fileType"),
          combine(c("obsModule","rctModule"), 
            c("funOpt_posStudlab", "opt_methodTau", "opt_sm")),
          combine(c("rctModule"), c("opt_method", "opt_incr"))), 
        function(x) input[[x]]),
      color = sapply(combine(c("obsModule","rctModule"), 
        c("funOpt_ptCol", paste0("plOpt_", c("barCol", "diamCol", "sqCol")))), 
        function(x) input[[x]]),
      checkboxes = sapply(c("obsModule-loadData-autoSE", "rctModule-plOpt_inclAbsNum",
        combine(c("obsModule","rctModule"), 
          c("funOpt_showStudlab", paste0("opt_", c("combFixed","combRandom","hakn")),
            paste0("plOpt_print", c("I2","Pval","Q","Tau2")), "plOpt_showWeights"))),
        function(x) input[[x]])
    )
    if (encode) state <- URLencode(jsonlite::base64_enc(memCompress(serialize(state, NULL), "gzip")), reserved=TRUE)
    return(state)
  }
  
  setState <- function(state) {
    if (is.character(state) && length(state)==1) {
      state <- unserialize(memDecompress(jsonlite::base64_dec(URLdecode(state)), "gzip"))
    }
    if (is.list(state)) {
      for (n in names(state$sliders)) {
        updateSliderInput(session, n, value=state$sliders[[n]])
      }
      for (n in names(state$text)) {
        updateTextInput(session, n, value=state$text[[n]])
      }
      for (n in names(state$numeric)) {
        updateNumericInput(session, n, value=state$numeric[[n]])
      }
      for (n in names(state$select)) {
        updateSelectInput(session, n, selected=state$select[[n]])
      }
      for (n in names(state$color)) {
        colourpicker::updateColourInput(session, n, value=state$color[[n]])
      }
      for (n in names(state$checkboxes)) {
        updateAwesomeCheckbox(session, n, value=state$checkboxes[[n]])
      }
    }
  }
  
  setTheState <- function() {
    if (!is.null(values$cookie)) {
      success <- try(setState(values$cookie))
      if (inherits(success, "try-error")) {
        showNotification("Error in restoring miniMeta settings!", type="error", duration=3)
      } else {
        showNotification("Restored miniMeta settings from browser.", type="message", duration=3)
      }
    }
  }

  values <- reactiveValues(cookie = NULL)

  shinyjs::delay(10, {
    js$getcookie()
    cookie <- input$jscookie
    if (is.null(cookie) || cookie!="") {
      cookie <- session$request$HTTP_COOKIE
      if (!is.null(cookie) && grepl("miniMeta_prefs\\s*=\\s*", cookie)) {
        cookie <- gsub("^.*miniMeta_prefs\\s*=\\s*", "", cookie)
        cookie <- gsub(";.*", "", cookie)
        cookie <- gsub("%(25)+", "%", cookie)
        js$setcookie(cookie)
      } else {
        cookie <- NULL
      }
    }
    if (!is.null(cookie)) {
      values$cookie <- cookie; setTheState()
    }
  })
  
  shinyjs::delay(100, {
    if (is.null(values$cookie)) {
      js$getcookie()
      cookie <- input$jscookie
      if (!is.null(cookie) && cookie!="") { 
        cookie <- gsub("%(25)+", "%", cookie)
        values$cookie <- cookie; setTheState()
      }
    }
  })

  shinyjs::delay(1000, {
    if (is.null(values$cookie)) {
      js$getcookie()
      cookie <- input$jscookie
      if (!is.null(cookie) && cookie!="") { 
        cookie <- gsub("%(25)+", "%", cookie)
        values$cookie <- cookie; setTheState()
      }
    }
  })

  observe({
    if (!is.null(stateEvent())) {
    
      if (stateEvent()=="save") {
        cookie <- getState()
        js$setcookie(cookie)
        showNotification("Stored miniMeta settings in browser.", type="message", duration=3)
        stateEvent(NULL)
        
      } else if (stateEvent()=="load") {
        js$getcookie()
        cookie <- input$jscookie
        if (!is.null(cookie) && cookie!="") { 
          cookie <- gsub("%(25)+", "%", cookie)
          values$cookie <- cookie; setTheState()
        } else {
          showNotification("No miniMeta settings found in browser.", type="warning", duration=3)
        }      
        stateEvent(NULL)
        
      } else if (stateEvent()=="clear") {
        js$rmcookie()
        showNotification("Cleaned miniMeta settings from browser.", type="message", duration=3)
        stateEvent(NULL)
        
      } else if (stateEvent()=="saveFile") {
        shinyjs::runjs("$('#saveStateToFile')[0].click();")
        stateEvent(NULL)
        
      }
    }
  })
  
  
  
  output$saveStateToFile <- downloadHandler(
    filename = function() {
      "miniMeta_settings.rds"
    },
    content = function(file) {
      state <- getState(encode=FALSE)
      saveRDS(state, file=file)
    }
  )

  observeEvent(input$loadStateFromFile, {
    if (is.null(input$loadStateFromFile)) return()
    inFile <- input$loadStateFromFile
    state <- try(readRDS(inFile$datapath), silent=TRUE)
      # Has the file been read successfully?
    if (length(state)==1 && class(state)=="try-error") {
      showModal(modalDialog(title = "Whoops...", 
        "Error while trying to read this file.", br(), "Is it an actual miniMeta file?", 
        footer = modalButton("OK, got it"), size="s"))
      return()
    }
    success <- try(setState(state))
    if (inherits(success, "try-error")) {
      showNotification("Error in restoring miniMeta settings!", type="error", duration=3)
    } else {
      showNotification("Restored miniMeta settings from file.", type="message", duration=3)
    }
  }, ignoreInit=TRUE)

  
}

