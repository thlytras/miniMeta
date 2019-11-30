library(shiny)
library(markdown)

source("modules/mod-bucher.R")
source("modules/mod-rct.R")


shinyUI(fluidPage(
  titlePanel("miniMeta"),
  span("A simple tool to run meta-analyses, with a focus on GRADE SoF tables"),
  tabsetPanel(
    rct_moduleUI(id = "rctModule"),
    tabPanel("Observational studies module"),
    tabPanel("Tools", br(),
      tabsetPanel(
        bucher_moduleUI(id = "bucher")
      )
    )
  )
))
