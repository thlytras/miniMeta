library(shiny)
library(shinyjs)
library(markdown)
library(colourpicker)
library(shinyWidgets)

source("modules/mod-bucher.R")
source("modules/mod-samplesize.R")
source("modules/mod-rct.R")
source("modules/mod-obs.R")

source("modules/include-saveState-ui.R")


shinyUI(fluidPage(
  saveState_header(),
  titlePanel(span(
    img(src="logo-large.svg", style="height:3em; margin-right:1em;"),
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
))
