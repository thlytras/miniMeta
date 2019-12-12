library(shiny)
library(meta)
library(metafor)
library(WriteXLS)
library(miniMeta)


shinyServer(function(input, output, session) {
  
  callModule(module = rct_module, id = "rctModule")  
  callModule(module = obs_module, id = "obsModule")  
  callModule(module = bucher_module, id = "bucher")

})
