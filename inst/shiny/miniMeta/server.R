library(shiny)
library(meta)
library(metafor)
library(WriteXLS)


shinyServer(function(input, output, session) {
  
  callModule(module = rct_module, id = "rctModule")  
  callModule(module = bucher_module, id = "bucher")

})
