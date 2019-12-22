library(shiny)
library(meta)
library(metafor)
library(WriteXLS)
library(miniMeta)
library(colourpicker)


shinyServer(function(input, output, session) {
  
  stateEvent <- reactiveVal()
  source("modules/include-saveState.R", local=TRUE)
  
  stateEvent <- callModule(module = rct_module, id = "rctModule", stateEvent)  
  stateEvent <- callModule(module = obs_module, id = "obsModule", stateEvent)  
  callModule(module = bucher_module, id = "bucher")
  
})
