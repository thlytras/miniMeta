
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
        updateColourInput(session, n, value=state$color[[n]])
      }
      for (n in names(state$checkboxes)) {
        updateCheckboxInput(session, n, value=state$checkboxes[[n]])
      }
    }
  }
  
  setTheState <- function() {
    if (!is.null(values$cookie)) {
      success <- try(setState(values$cookie))
      if (inherits(success, "try-error")) {
        showNotification("Error in restoring preferences!", type="error", duration=3)
      } else {
        showNotification("Restored miniMeta preferences from browser.", type="message", duration=3)
      }
    }
  }

  values <- reactiveValues(cookie = NULL)

  delay(10, {
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
  
  delay(100, {
    if (is.null(values$cookie)) {
      js$getcookie()
      cookie <- input$jscookie
      if (!is.null(cookie) && cookie!="") { 
        cookie <- gsub("%(25)+", "%", cookie)
        values$cookie <- cookie; setTheState()
      }
    }
  })

  delay(1000, {
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
        showNotification("Stored miniMeta preferences in browser.", type="message", duration=3)
        stateEvent(NULL)
        
      } else if (stateEvent()=="load") {
        js$getcookie()
        cookie <- input$jscookie
        if (!is.null(cookie) && cookie!="") { 
          cookie <- gsub("%(25)+", "%", cookie)
          values$cookie <- cookie; setTheState()
        } else {
          showNotification("No miniMeta preferences found in browser.", type="warning", duration=3)
        }      
        stateEvent(NULL)
        
      } else if (stateEvent()=="clear") {
        js$rmcookie()
        showNotification("Cleaned miniMeta preferences from browser.", type="message", duration=3)
        stateEvent(NULL)
        
      }
    }
  })
