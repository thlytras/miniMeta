% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/samplesize.R
\name{sampleSizeBin}
\alias{sampleSizeBin}
\title{Sample size calculator for binary outcomes}
\usage{
sampleSizeBin(cer, RRR = 25, ier = NULL, a = 0.05, b = 0.2, K = 1)
}
\arguments{
\item{cer}{Control group event rate, a value between 0 and 1.
All should be named.}

\item{RRR}{Relative Risk Reduction (\%) in the intervention group.}

\item{ier}{Intervention group event rate, a value between 0 and 1
If \code{NULL}, it is calculated from \code{RRR}. If non-\code{NULL},
the value of this argument is used and \code{RRR} is ignored.}

\item{a}{False positive rate (alpha). Defaults to 0.05 (5\%).}

\item{b}{False negative rate (beta). Defaults to 0.2. 
Power is one minus beta; thus the default is 80\% power.}

\item{K}{Ratio of intervention group size to control group size. 
Defaults to 1, meaning both groups have the same size. 
Set to infinity (\code{Inf}) in order to calculate sample size for a 
\emph{single-group study}, see details below.}
}
\value{
An integer vector of length 2, with the sample sizes for the 
    control and intervention groups. 

    If \code{K=Inf}, then the sample size calculation is not for a study
    with two groups, but for a single-group study in which a fixed known 
    population event rate is assumed. In that case, argument \code{cer}
    represents the population event rate, and \code{ier} the study event 
    rate that it we anticipate. And the return value is a single value,
    i.e. the sample size of the study.
}
\description{
Calculates sample size for a trial with a binomial outcome, for a given
power and false positive rate.
}
\examples{
# Sample size for a trial with 40\\% control event rate and 1:1 randomization,
# aiming to show a Relative Risk Reduction of 30\\% with 80\\% power.
sampleSizeBin(0.4, RRR=30)

# Sample size for a single-group study aiming to show an event rate of 20\\%
# against a population event rate of 10\\%, with 90\\% power.
sampleSizeBin(0.1, ier=0.2, b=0.1, K=Inf)

}
