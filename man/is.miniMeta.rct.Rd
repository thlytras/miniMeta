% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is.miniMeta.R
\name{is.miniMeta.rct}
\alias{is.miniMeta.rct}
\title{Is this a miniMeta object for RCTs?}
\usage{
is.miniMeta.rct(x)
}
\arguments{
\item{x}{An object of class \code{miniMeta}}
}
\value{
\code{TRUE} if it is a valid miniMeta object holding a meta-analysis 
of Randomized Controlled Trials (RCTs), \code{FALSE} if it is not.
}
\description{
This function checks whether this is a valid miniMeta object holding a
a meta-analysis of Randomized Controlled Trials (RCTs).
}
\examples{
is.miniMeta.rct(example_miniMeta_obs) # returns FALSE
is.miniMeta.rct(example_miniMeta_rct) # returns TRUE

}
