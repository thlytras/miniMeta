library(miniMeta)
library(meta)
library(metafor)
library(colourpicker)

# Load required modules
source("modules/mod-obsLoadData.R")
source("modules/mod-plDownloadOpts.R")
source("modules/miniFileInput.R")
source("modules/mod-funnel.R")

# Load the UI of this module from separate file
source("modules/mod-obs-ui.R")

# Server logic of the module follows
obs_module <- function(input, output, session, stateEvent) {

  mtype <- 2   # This is an observational module

  # Import the "guts" of the module
  source("modules/include-mod-univ.R", local=TRUE)

  dat <- callModule(module = obsLoadData, id="loadData", 
        dataset = reactive(values$dataset), 
        logMeasure = reactive(input$opt_sm %in% c("RR","OR")))
  
  # REACTIVE: check validity of the data
  chk <- reactive({
    return(TRUE)
  })
  
  # REACTIVE: run the meta-analysis
  m <- reactive({
    if (chk()) {
      grp <- trimws(as.character(dat()$group)); grp[grp==""] <- NA
      if (sum(is.na(grp))==0 & length(unique(grp))>1) {
        byVar <- factor(grp)
      } else {
        byVar <- NULL
      }
      te <- if (input$opt_sm %in% c("RR","OR")) log(dat()$TE) else dat()$TE
      return(metagen(te, abs(seTE), data=dat(), studlab=Study, 
        method.tau=input$opt_methodTau,
        comb.fixed=input$opt_combFixed, comb.random=input$opt_combRandom,
        byvar=byVar, sm=input$opt_sm, hakn=input$opt_hakn
      ))
    }
  })



  # REACTIVE: render the output panel
  output$uncpanel <- renderPrint({
    bR <- as.numeric(input$baseRisk)
    if (!is.na(bR) && (bR<0 || bR>100)) bR <- NA
    if (is.na(bR)) return(cat("Missing baseline risk"))
    if (inherits(m(), "metagen")) {
      return(print(gradeObs(m(), bR)))
    } else {
      return(cat("No meta-analysis input"))
    }
  })

  gradeObs <- function(m, bR) {
    lim <- function(x) {
        x[x<0] <- 0
        x[x>1000] <- 1000
        x
    }
    ef <- with(m, c(TE.random, lower.random, upper.random))
    uR <- bR*10
    sg <- function(x) sprintf("%.0f %s", abs(x), c("fewer", "more")[as.integer(x>=0)+1])
    if (m$sm=="RR") {
      eR <- lim(uR * exp(ef))
      a <- sprintf("RR, %.2f (%.2f – %.2f)", exp(ef[1]), exp(ef[2]), exp(ef[3]))
    } else if (m$sm=="OR") {
      ORtoP <- function(o) o / (o+1)
      eR <- lim(ORtoP(uR/(1000-uR) * exp(ef))*1000)
      a <- sprintf("OR, %.2f (%.2f – %.2f)", exp(ef[1]), exp(ef[2]), exp(ef[3]))
    } else if (m$sm=="RD") {
      eR <- lim(uR + ef*1000)
      a <- sprintf("RD, %.2f (%.2f – %.2f)", ef[1], ef[2], ef[3])
    } else { # Arcsine risk difference
      eR <- (sin(asin(sqrt(uR/1000)) + ef)^2)*1000
      a <- sprintf("ASD, %.2f (%.2f – %.2f)", ef[1], ef[2], ef[3])
    }
    rdI <- eR-uR; rdI[2:3] <- rdI[2:3][order(abs(rdI[2:3]))]
    a <- c(a, 
          sprintf("%.0f per 1000", uR),
          sprintf("%.0f per 1000", eR[1]),
          sprintf("%.0f per 1000", uR),
          sprintf("%s per 1000 (from %s to %s)", sg(rdI[1]), sg(rdI[2]), sg(rdI[3]))
      )[c(2,3,1,4,5)]
    a <- data.frame("Results"=a)
    rownames(a) <- c("Event rate (unexposed)", "Event rate (exposed)", "Relative effect", "Risk in unexposed", "RD with exposure")
    a
  }

  
  return(stateEvent)

}

