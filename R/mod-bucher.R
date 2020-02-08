#' UI function for the Bucher module
#'
#' @param id Module id
#' 
#' @import shiny
#' 
#' @keywords internal
#' @noRd
bucher_moduleUI <- function(id) {
  ns <- NS(id)
  tabPanel("Bucher method",
    h4("Adjusted Indirect Comparisons (Bucher method)"),
    fluidRow(
    column(6,
      fluidRow(
        column(4, numericInput(ns("buch_AC_est"), "A vs C", NA)),
        column(4, numericInput(ns("buch_AC_lo"), "95%CI LL", NA)),
        column(4, numericInput(ns("buch_AC_hi"), "95%CI UL", NA))
      ),
      fluidRow(
        column(4, numericInput(ns("buch_BC_est"), "B vs C", NA)),
        column(4, numericInput(ns("buch_BC_lo"), "95%CI LL", NA)),
        column(4, numericInput(ns("buch_BC_hi"), "95%CI UL", NA))
      ),
      radioButtons(ns("buch_type"), "What kind of effect measure is this?", 
        choices = list(
        "Relative Risk / Odds Ratio / Other exponentiated measure" = "exp", 
        "logRR / logOR / Other absolute measure" = "abs"
      ), width="100%")
    ),
    column(6, verbatimTextOutput(ns("buch_output")))
    )
  )
}


#' Server function for the Bucher module
#'
#' @param input Shiny input parameter
#' @param output Shiny output parameter
#' @param session Shiny session object
#' 
#' @import shiny
#' @importFrom stats qnorm
#' 
#' @keywords internal
#' @noRd
bucher_module <- function(input, output, session) {

    # REACTIVE: Output for Bucher method
  output$buch_output <- renderPrint({
    eff <- as.numeric(c(input$buch_AC_est, input$buch_AC_lo, input$buch_AC_hi,
        input$buch_BC_est, input$buch_BC_lo, input$buch_BC_hi))
    if (sum(is.na(eff))==0) {
      # Make sure we're not trying to logarithmize negative values
        if (input$buch_type=="exp" & sum(eff<0, na.rm=TRUE)>0) return(cat(" "))
      if (input$buch_type=="exp") eff <- log(eff)
      eff <- matrix(eff, nrow=2, byrow=TRUE)
      colnames(eff) <- c("est", "lo", "hi"); rownames(eff) <- c("AC", "BC")
      pool.eff <- diff(eff[c("BC","AC"), "est"])
      pool.sd <- sqrt(
        (abs(diff(eff["AC", c("lo","hi")]))/(2*qnorm(0.975)))^2 + 
        (abs(diff(eff["BC", c("lo","hi")]))/(2*qnorm(0.975)))^2)
      symm.ac <- abs(abs(diff(eff["AC", c("est","hi")]))/abs(diff(eff["AC", c("lo","est")]))-1)<0.05
      symm.bc <- abs(abs(diff(eff["BC", c("est","hi")]))/abs(diff(eff["BC", c("lo","est")]))-1)<0.05
      output <- c(
        if (input$buch_type=="exp") "Exponentiated measure - converting to log:\n" else "",
        sprintf("Point estimate, A vs C: %.3f", eff["AC", "est"]),
        sprintf("Standard error, A vs C: %.3f", 
          abs(diff(eff["AC", c("lo","hi")]))/(2*qnorm(0.975))),
        sprintf("\nPoint estimate, B vs C: %.3f", eff["BC", "est"]),
        sprintf("Standard error, B vs C: %.3f",
          abs(diff(eff["BC", c("lo","hi")]))/(2*qnorm(0.975))),
        sprintf("\nPoint estimate, A vs B: %.3f", pool.eff),
        sprintf("Standard error, A vs B: %.3f", pool.sd),
        sprintf("95%% Confidence Interval, A vs B: %.3f \u2014 %.3f", 
          pool.eff-qnorm(0.975)*pool.sd, pool.eff+qnorm(0.975)*pool.sd),
        (if (input$buch_type=="exp") paste(
            "\nExponentiating:",
            sprintf("Point estimate, A vs B: %.2f", exp(pool.eff)),
            sprintf("95%% Confidence Interval, A vs B: %.2f \u2014 %.2f",
              exp(pool.eff-qnorm(0.975)*pool.sd), exp(pool.eff+qnorm(0.975)*pool.sd)
            ), sep="\n") else ""), "")
      if (!symm.ac) output <- c(output, 
        "WARNING: The 95% CI for A vs C does not look symmetric.",
        "         Are the values you entered correct?")
      if (!symm.bc) output <- c(output, 
        "WARNING: The 95% CI for B vs C does not look symmetric.",
        "         Are the values you entered correct?")
      return(cat(paste(output, collapse="\n")))
    } else return(cat("No (appropriate) input data provided."))
  })

}
