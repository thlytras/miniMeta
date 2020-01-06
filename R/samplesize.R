#' Sample size calculator for binary outcomes
#'
#' Calculates sample size for a trial with a binomial outcome, for a given
#' power and false positive rate.
#'
#' @param cer Control group event rate, a value between 0 and 1.
#'     All should be named.
#' @param RRR Relative Risk Reduction (\%) in the intervention group.
#' @param ier Intervention group event rate, a value between 0 and 1
#'     If \code{NULL}, it is calculated from \code{RRR}. If non-\code{NULL},
#'     the value of this argument is used and \code{RRR} is ignored.
#' @param a False positive rate (alpha). Defaults to 0.05 (5\%). 
#' @param b False negative rate (beta). Defaults to 0.2. 
#'     Power is one minus beta; thus the default is 80\% power.
#' @param K Ratio of intervention group size to control group size. 
#'     Defaults to 1, meaning both groups have the same size. 
#'     Set to infinity (\code{Inf}) in order to calculate sample size for a 
#'     \emph{single-group study}, see details below.
#'
#' @return An integer vector of length 2, with the sample sizes for the 
#'     control and intervention groups. 
#'
#'     If \code{K=Inf}, then the sample size calculation is not for a study
#'     with two groups, but for a single-group study in which a fixed known 
#'     population event rate is assumed. In that case, argument \code{cer}
#'     represents the population event rate, and \code{ier} the study event 
#'     rate that it we anticipate. And the return value is a single value,
#'     i.e. the sample size of the study.
#'
#' @importFrom stats qnorm
#' 
#' @examples
#' # Sample size for a trial with 40\% control event rate and 1:1 randomization,
#' # aiming to show a Relative Risk Reduction of 30\% with 80\% power.
#' sampleSizeBin(0.4, RRR=30)
#' 
#' # Sample size for a single-group study aiming to show an event rate of 20\%
#' # against a population event rate of 10\%, with 90\% power.
#' sampleSizeBin(0.1, ier=0.2, b=0.1, K=Inf)
#'
#' @export
sampleSizeBin <- function(cer, RRR = 25, ier=NULL, a = 0.05, b = 0.2, K=1) {
  if (cer<0 || cer>1) stop("Control event rate must be between 0 and 1")
  if (!is.null(ier)) {
    if (ier<0 || ier>1) stop("Intervention group event rate must be between 0 and 1")
    RRR <- 100*(cer-ier)/cer
  } else {
    if (RRR>100) stop("Relative Risk Reduction cannot be higher than 100%")
    ier <- cer*(1-RRR/100)
    if (ier>1) stop("Calculated intervention event rate cannot be higher than 1. Check your RRR and control event rate values.")
  }
  D <- ier - cer
  if (!is.infinite(K)) {
    pavg <- (cer+K*ier)/(1+K)
    n1 <- (qnorm(a/2)*sqrt(pavg*(1-pavg)*(1+1/K)) + qnorm(b)*sqrt(cer*(1-cer)+(ier*(1-ier)/K)))^2/D^2
    n2 <- K * n1
    return(round(c(control=n1,intervention=n2)))
  } else {
    n <- (qnorm(a/2)*sqrt(cer*(1-cer)) + qnorm(b)*sqrt(ier*(1-ier)))^2/D^2
    return(round(c(size=n)))
  }
}



#' Sample size calculator for continuous outcomes
#'
#' Calculates sample size for a trial with a continuous outcome, for a given
#' power and false positive rate.
#'
#' @param Dm Anticipated absolute difference in means between the two groups
#'     (intervention and control).
#' @param SD Anticipated standard deviation for the outcome.
#' @param a False positive rate (alpha). Defaults to 0.05 (5\%). 
#' @param b False negative rate (beta). Defaults to 0.2. 
#'     Power is one minus beta; thus the default is 80\% power.
#' @param K Ratio of intervention group size to control group size. 
#'     Defaults to 1, meaning both groups have the same size. 
#'     Set to infinity (\code{Inf}) in order to calculate sample size for a 
#'     \emph{single-group study}, see details below.
#'
#' @return An integer vector of length 2, with the sample sizes for the 
#'     control and intervention groups. 
#'
#'     If \code{K=Inf}, then the sample size calculation is not for a study
#'     with two groups, but for a single-group study in which we try to show
#'     a difference from a fixed known population mean. In that case, argument 
#'     \code{Dm} represents the absolute difference between the study mean and
#'     population mean, rather than the difference in means between two groups.
#'     And the return value is a single value, i.e. the sample size of the study.
#'
#' @examples
#' # Sample size for a trial with 2:1 randomization, aiming to show a mean
#' # difference of 2 for a continuous outcome with a standard deviation of 3, 
#' # with 90\% power.
#' sampleSizeCont(2, 3, b=0.1, K=2)
#' 
#' # Similar for a single-group study aiming to show a difference of 2 against
#' # a known population mean. 
#' sampleSizeCont(2, 3, b=0.1, K=Inf)
#'
#' @export
sampleSizeCont <- function(Dm, SD, a = 0.05, b = 0.2, K=1) {
  n1 <- ((SD^2+SD^2/K)*(qnorm(a/2)+qnorm(b))^2)/(Dm^2)
  if (is.infinite(K)) {
    return(round(c(size=n1)))
  } else {
    n2 <- K*n1
    return(round(c(group1=n1, group2=n2)))
  }
}

