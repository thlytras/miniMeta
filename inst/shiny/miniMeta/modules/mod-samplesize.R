
samplesize_moduleUI <- function(id) {
  ns <- NS(id)
  tabPanel("Optimal sample size",
    h4("Sample size calculator, for binary and continuous outcomes"),
    fluidRow(
    column(6,
      wellPanel(
        h4("Binary outcome?"),
        fluidRow(
          column(6, numericInput(ns("ois_cer"), "Control group Event Rate (%)", NA, min=0, max=100)),
          column(6, numericInput(ns("ois_ier"), "Intervention group Event Rate (%)", NA, min=0, max=100))
        ),
        fluidRow(
          column(6, numericInput(ns("ois_RRR"), "Relative Risk Reduction (RRR, %)", 25, max=100)),
          helpText("Delete the RRR value if you want to manually enter the anticipated Intervention group Event Rate.")
        )
      ),
      wellPanel(
        h4("Continuous outcome?"),
        fluidRow(
          column(6, numericInput(ns("ois_Dm"), "Absolute mean difference", NA)),
          column(6, numericInput(ns("ois_SD"), "Standard Deviation", NA, min=0))
        )
      ),
      fluidRow(
        column(4, numericInput(ns("ois_alpha"), "Type I error rate (alpha)", 0.05, min=0, max=0.5)),
        column(4, sliderInput(ns("ois_pow"), "Power (%) (1 - beta)", 50, 100, 80, ticks=FALSE)),
        column(4, sliderTextInput(ns("ois_kappa"), 
            label = HTML("Group size ratio (n<sub>1</sub>/n<sub>2</sub>)"), 
            choices = c("1/5", "1/4", "1/3", "1/2", 1:5),
            selected = "1"
        ))
      ),
      fluidRow(
        column(7, helpText("If this is checked, \"Control group Event Rate\" represents a fixed population event rate and \"Intervention group\" represents that of the single-group study. The group size ratio setting is ignored."), br()),
        column(1, icon("hand-point-right", "fa-2x"), style="padding-top: 10px"),
        column(4, awesomeCheckbox(ns("ois_kinf"), "Single-group study"))
      )
    ),
    column(6, verbatimTextOutput(ns("ois_output")))
    )
  )
}


samplesize_module <- function(input, output, session) {

  Kmatrix <- c(1/5, 1/4, 1/3, 1/2, 1:5)
  names(Kmatrix) <- c("1/5", "1/4", "1/3", "1/2", 1:5)
    
  observe({
    input$ois_ier # Take dependency on ois_ier
    RRR <- as.numeric(input$ois_RRR)
    cer <- as.numeric(input$ois_cer)/100
    if (!is.na(RRR) & RRR<=100 & !is.na(cer) & cer>=0 & cer<=1) {
      updateNumericInput(session, "ois_ier", value=cer*(100-RRR))
    }
  })
  
    # REACTIVE: Output for sample size calculator
  output$ois_output <- renderPrint({
    output <- c()
    # Checking sanity of values
    alpha <- as.numeric(input$ois_alpha)
    if (is.na(alpha) || alpha<=0 || alpha>=1)
      output <- c(output, "Type I error rate (alpha) should be a value between 0 and 1.")
    cer <- suppressWarnings(as.numeric(input$ois_cer))/100
    if (!is.na(cer) && (cer<=0 || cer>=1))
      output <- c(output, "Control group Event Rate should be a value between 0 and 100 (%).")
    ier <- suppressWarnings(as.numeric(input$ois_ier))/100
    if (!is.na(ier) && (ier<=0 || ier>=1))
      output <- c(output, "Intervention group Event Rate should be a value between 0 and 100 (%).")
    SD <- suppressWarnings(as.numeric(input$ois_SD))
    if (!is.na(SD) && SD<=0)
      output <- c(output, "Standard Deviation cannot be zero or negative.")
    if (length(output)>0)
      return(cat(paste(output, collapse="\n")))

    Dm <- suppressWarnings(as.numeric(input$ois_Dm))
    pow <- as.numeric(input$ois_pow)
    K <- as.numeric(Kmatrix[as.character(input$ois_kappa)])
    if (input$ois_kinf) K <- Inf
    
    if (!is.na(cer) && !is.na(ier)) {
      if (is.finite(K)) {
        output <- c(output, 
          "Optimal sample size for a study with binary outcome",
          "---------------------------------------------------"
        )
      } else {
        output <- c(output, 
          "Optimal sample size for a single-group study with binary outcome",
          "----------------------------------------------------------------"
        )
      }
      ss <- try(sampleSizeBin(cer=cer, ier=ier, a=alpha, b=1-pow/100, K=K))
      if (inherits(ss, "try-error")) {
        output <- c(output, "Error calculating sample size...", "\n")
      } else {
        output <- c(output, sprintf("Total sample size: %s", sum(ss)), "")
        if (is.finite(K)) {
          output <- c(output, 
            sprintf("(Control group: %s, Intervention group: %s)", ss["control"], ss["intervention"]),
            sprintf("(Event rates, control: %.1f%%, intervention: %.1f%%)", cer*100, ier*100),
            sprintf("(alpha = %.2f, Power = %s%%, K = %s)", alpha, pow, input$ois_kappa), "\n"
          )
        } else {
          output <- c(output, 
            sprintf("(Event rates, population: %.1f%%, study: %.1f%%)", cer*100, ier*100),
            sprintf("(alpha = %.2f, Power = %s%%)", alpha, pow), "\n"
          )
        }
      }
    }
    
    if (!is.na(Dm) && !is.na(SD)) {
      if (is.finite(K)) {
        output <- c(output, 
          "Optimal sample size for a study with continuous outcome",
          "-------------------------------------------------------"
        )
      } else {
        output <- c(output, 
          "Optimal sample size for a single-group study with continuous outcome",
          "--------------------------------------------------------------------"
        )
      }
      ss <- try(sampleSizeCont(Dm, SD, a=alpha, b=1-pow/100, K=K))
      if (inherits(ss, "try-error")) {
        output <- c(output, "Error calculating sample size...", "\n")
      } else {
        output <- c(output,
          sprintf("Total sample size: %s", sum(ss)), "",
          if (is.finite(K)) sprintf("(Group 1: %s, Group 2: %s)", ss["group1"], ss["group2"]) else NULL,
          sprintf("(Anticipated absolute mean difference: %s)", Dm),
          sprintf("(Standard Deviation: %s)", SD),
          if (is.finite(K)) {
            sprintf("(alpha = %.2f, Power = %s%%, K = %s)", alpha, pow, input$ois_kappa)
          } else {
            sprintf("(alpha = %.2f, Power = %s%%)", alpha, pow)
          },
          "\n"
        )
      }
    }
    
    if (length(output)==0)
      output <- c(output, "No (appropriate) input data provided.")
    
    return(cat(paste(output, collapse="\n")))
  })

}
