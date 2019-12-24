library(miniMeta)
library(meta)
library(metafor)
library(colourpicker)

# Load required modules
source("modules/mod-rctLoadData.R")
source("modules/mod-plDownloadOpts.R")
source("modules/mod-funnel.R")

# Load the UI of this module from separate file
source("modules/mod-rct-ui.R")

# Server logic of the module follows
rct_module <- function(input, output, session, stateEvent) {

  mtype <- 1   # This is an RCT module

  # Import the "guts" of the module
  source("modules/include-mod-univ.R", local=TRUE)
  
  dat <- callModule(module = rctLoadData, id="loadData", 
        dataset = reactive(values$dataset))
  
  # REACTIVE: check validity of the data
  chk <- reactive({
    checkRCTValidity(dat())
  })

  checkRCTValidity <- function(rctDAT) {
    msg <- list()
    if (nrow(rctDAT)==0) {
        msg <- c(msg, "Empty data -- cannot perform meta-analysis")
    } else {
        if (sum(is.na(rctDAT[,2:5]))>0) msg <- c(msg, "Blank cells not allowed.")
        a <- with(rctDAT, which(e.e>n.e | e.c>n.c))
        if (length(a)>0) msg <- c(msg, paste("Number of events cannot be higher than the number randomized.\n   Check studies: '", paste(rctDAT[a,1], collapse="', '"), "'.", sep=""))
        a <- with(rctDAT, which(n.e==0 | n.c==0))
        if (length(a)>0) msg <- c(msg, paste("Number randomized cannot be zero.\n   Check studies: '", paste(rctDAT[a,1], collapse="', '"), "'.", sep=""))  
    }
    res <- TRUE
    if (length(msg)>0) {
        res <- FALSE
        attr(res, "msg") <- msg
    }
    return(res)
  }



  # REACTIVE: run the meta-analysis
  m <- reactive({
    if (chk()) {
      optIncr <- input$opt_incr; if (optIncr!="TACC") optIncr <- as.numeric(optIncr)
      grp <- trimws(as.character(dat()$group)); grp[grp==""] <- NA
      if (sum(is.na(grp))==0 & length(unique(grp))>1) {
        byVar <- factor(grp)
      } else {
        byVar <- NULL
      }
      return(metabin(e.e, n.e, e.c, n.c, data=dat(), studlab=Study, 
        method=input$opt_method, method.tau=input$opt_methodTau,
        comb.fixed=input$opt_combFixed, comb.random=input$opt_combRandom,
        byvar=byVar, incr=optIncr, sm=input$opt_sm, hakn=input$opt_hakn
      ))
    }
  })




  # REACTIVE: render the output panel
  output$uncpanel <- renderPrint({
    if (chk()) {
      return(print(gradeRCT(dat()[,-1], m())))
    } else {
      return(cat(paste(attr(chk(), "msg"), sep="", collapse="\n")))
    }
  })
  

  callModule(module = funnelTab, id="labbe", labbe=TRUE,
    meta = reactive(m()),
    options = funnelOptions
  )


  gradeRCT <- function(dat, m) {
    lim <- function(x) {
        x[x<0] <- 0
        x[x>1000] <- 1000
        x
    }
    ef <- with(m, c(TE.random, lower.random, upper.random))
    uR <- sum(dat[,3])*1000/sum(dat[,4])
    if (m$sm=="RR") {
        eR <- lim(uR * exp(ef))
        effM <- sprintf("RR, %.2f (%.2f – %.2f)", exp(ef[1]), exp(ef[2]), exp(ef[3]))
    } else if (m$sm=="OR") {
        ORtoP <- function(o) o / (o+1)
        eR <- lim(ORtoP(uR/(1000-uR) * exp(ef))*1000)
        effM <- sprintf("OR, %.2f (%.2f – %.2f)", exp(ef[1]), exp(ef[2]), exp(ef[3]))
    } else if (m$sm=="RD") {
        eR <- lim(uR + ef*1000)
        effM <- sprintf("RD, %.2f (%.2f – %.2f)", ef[1], ef[2], ef[3])
    } else { # Arcsine risk difference
        eR <- (sin(asin(sqrt(uR/1000)) + ef)^2)*1000
        effM <- sprintf("ASD, %.2f (%.2f – %.2f)", ef[1], ef[2], ef[3])
    }
    rdI <- eR-uR; rdI[2:3] <- rdI[2:3][order(abs(rdI[2:3]))]  
    sg <- function(x) sprintf("%.0f %s", abs(x), c("fewer", "more")[as.integer(x>=0)+1])
    a <- c(sprintf("%s/%s (%.1f%%)", sum(dat[,3]), sum(dat[,4]), uR/10),
        sprintf("%s/%s (%.1f%%)", sum(dat[,1]), sum(dat[,2]), sum(dat[,1])*100/sum(dat[,2])),
        effM,
        sprintf("%s per 1000", round(uR)),
        sprintf("%s per 1000 (from %s to %s)", sg(rdI[1]), sg(rdI[2]), sg(rdI[3])))
    a <- data.frame("Results"=a)
    rownames(a) <- c("Event rate (control)", "Event rate (intervention)", "Relative effect", "Risk with control", "RD with intervention")
    a
  }

  return(stateEvent)

}

